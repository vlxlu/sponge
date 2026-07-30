library(dplyr)
library(ggplot2)
library(cowplot)
library(emmeans)

#Things to potentially flag:
#CA_S5_01 puff #4 is lagging edge
#DA_S35_02 puff #3 is lagging edge
#CA)S62)_O1 puff #5 is short (cut?)

sponge_flow <- read.csv("raw_data/sponge_tracks_25July2026.csv")
sponge_flow <- sponge_flow %>%
  mutate(across(c(t, x, y, r, v), as.numeric))

speed_perpuff <- sponge_flow %>%
  group_by(osculum_ID, tracking_num) %>%
  filter(osculum_ID != "") %>%
  summarise(
    abs_distance_cm = abs(last(r) - first(r)),
    time_elapsed_s  = last(t) - first(t),
    speed_cm_s      = abs_distance_cm / time_elapsed_s,
    .groups = "drop"
  )
print(speed_perpuff, n = Inf)
#What is up with AB_S26_O3     puff        6? why so slow?

#Here, what is we trim to only keep first 2 cm max. This will make videos approximately hte same 
#length
speed_perpuff_2cm <- sponge_flow %>%
  filter(osculum_ID != "") %>%
  arrange(osculum_ID, tracking_num, t) %>%
  group_by(osculum_ID, tracking_num) %>%
  filter(r <= first(r) + 2) %>%
  summarise(
    abs_distance_cm = abs(last(r) - first(r)),
    time_elapsed_s  = last(t) - first(t),
    speed_cm_s      = abs_distance_cm / time_elapsed_s,
    .groups = "drop"
  )


print(speed_perpuff_2cm, n=Inf)


#Correlation
cor(speed_perpuff_2cm$speed_cm_s, speed_perpuff$speed_cm_s )
# r = 0.9500454
plot(speed_perpuff_2cm$speed_cm_s, speed_perpuff$speed_cm_s )

#Calculate mean per osculum
speed_mean <- speed_perpuff_2cm %>%
  group_by(osculum_ID) %>%
  summarise(
    mean_speed = mean(speed_cm_s, na.rm = TRUE),
    sd_speed   = sd(speed_cm_s, na.rm = TRUE),
    .groups = "drop"
  )  
print(speed_mean, n = Inf)

## check why AB_27_O1 and DA_S36_O2 sd_speed is NA

## Read in sponge morphology data set
sponge_size <- read.csv("raw_data/sponge_size_data.csv")


#Join sponge size data for matching sponges
speed_mean <- speed_mean %>%
  left_join(sponge_size, by = c("osculum_ID" = "id"))

ggplot(speed_mean, aes(x = species, y = mean_speed)) +
  geom_boxplot() +
  labs(x = "Species", y = "Mean speed (cm/s)") +
  theme_cowplot()

#So far, particle velocity not sig different across three species
model <- aov(mean_speed ~ species, data = speed_mean)
summary(model)
## species has a significant effect on mean speed (p = 0.0187)

#Ok, let's look at flow rate...this requires different columns for different species
#becaues they have different shapes.

speed_mean <- speed_mean %>%
  mutate(
    osc_diam1_cm = as.numeric(na_if(as.character(osc_diam1_cm), ".")),
    osc_diam2_cm = as.numeric(na_if(as.character(osc_diam2_cm), "."))
  )

#first calculate cross sectional area
speed_mean <- speed_mean %>%
  mutate(
    cross_sec_area = case_when(
      species %in% c("Archeri", "Lacunosa") ~ pi * (osc_diam1_cm/2)^2,
      species == "Touchmenot"               ~ pi * (osc_diam1_cm/2) * (osc_diam2_cm/2)
    )
  )
ggplot(speed_mean, aes(x = species, y = cross_sec_area)) +
  geom_boxplot() +
  labs(x = "Species", y = "Cross-sectional area (cm²)") +
  theme_cowplot()

#Now calculate flow, make box plot, and run anova
speed_mean <- speed_mean %>%
  mutate(osc_flow = cross_sec_area * mean_speed)
ggplot(speed_mean, aes(x = species, y = osc_flow, fill = species)) +
  geom_boxplot() +
  labs(x = "Sponge species", y = "Osculum flow (cm³/s)", fill = "Species") +
  scale_fill_manual(values = c("lavender","palevioletred1","lightcoral")) +
  theme_cowplot()


## Q1: Does oscular flow rate differ across sponge species?
model_flow <- aov(osc_flow ~ species, data = speed_mean)
summary(model_flow)
TukeyHSD(model_flow)
##tmn is significantly higher than other two

#Think about applying correction of 0.5...look for old papers....
#we will also want to emphasize that this is about RELATIVE speed, not necessarily
#getting perfect measurement of flow!

#For TMN...want to look at flow and how it (maybe) impacts tail beat freq of fish

#read in fish data
fish_tbf <- read.csv("raw_data/tbf_correlates.csv")
str(fish_tbf)
fish_tbf <- fish_tbf  %>%
  mutate(across(c(diam1, diam2, osc_depth, osculum_vol, max_height_cm, tbf_mean, depth), 
                ~as.numeric(na_if(., "."))))

fish_tbf$Fish.Size <- factor(fish_tbf$Fish.Size, levels = c("Small", "Medium", "Large"))

#Join rows that match from the flow and fish data (so far there are 20 vids)
TMN_complete <- speed_mean %>%
  inner_join(fish_tbf, by = c("osculum_ID" = "Sponge"))
print(TMN_complete, n=Inf)


## Q3: is flow rate in TMN correlated w resident goby swimming patterns?
#Let's see if we can't recreate Basma's finding of marg effect of fish size (qualitative)
#on tbf
lm_size <- lm(TMN_complete$tbf_first ~ TMN_complete$Fish.Size)
summary(lm_size)

#Same trend...see if we can boost sample size a bit more
plot(TMN_complete$Fish.Size, TMN_complete$tbf_first)

#now try flow rate (nope!)
lm_flow <- lm( TMN_complete$tbf_first ~ TMN_complete$osc_flow)
summary(lm_flow)
plot(TMN_complete$osc_flow, TMN_complete$tbf_first)

ggplot(data = TMN_complete, aes(x = osc_flow, y = tbf_first)) + 
  geom_point() +
  labs(x = "Osculum flow (cm³/s)", y = "First tail beat frequency") + 
  theme_cowplot()

## just flippling ^^ axes to see if it looks a bit better (prob not)
ggplot(data = TMN_complete, aes(x = tbf_first, y = osc_flow)) + 
  geom_point() +
  labs(y = "Osculum flow (cm³/s)", x = "First tail beat frequency") + 
  theme_cowplot()

#Ok, look at interaction just in case
lm_interac <- lm( TMN_complete$tbf_first ~ TMN_complete$osc_flow*TMN_complete$Fish.Size)
summary(lm_interac)
#nope...it's just size


# trying models -----------------------------------------------------------
## this is to try out models that we discussed in meeting on July 13


lm_osc_flow <- lm(osc_flow ~ depth_m + oscula 
                  + max_height_cm + species, data = speed_mean)
summary(lm_osc_flow)


## Q2: intra and interspecies flowrate-morphology relationship

#intraspecies
lm_flow_species <- lm(osc_flow ~ species*max_height_cm, data = speed_mean)
summary(lm_flow_species)
anova(lm_flow_species)


#filtering data for Aplysina archeri
archeri <- speed_mean %>% 
  filter(species == "Archeri")

lacunosa <- speed_mean %>% 
  filter(species == "Lacunosa")

tmn <- speed_mean  %>% 
  filter(species == "Touchmenot")

lm_flow_aa <- lm(osc_flow ~ max_height_cm + depth_m + oscula, data = archeri)
summary(lm_flow_aa)
anova(lm_flow_aa)

lm_flow_al <- lm(osc_flow ~ max_height_cm + depth_m + oscula, data = lacunosa)
summary(lm_flow_al)
anova(lm_flow_al)

lm_flow_tmn <- lm(osc_flow ~ max_height_cm + depth_m + oscula, data = tmn)
summary(lm_flow_tmn)
anova(lm_flow_tmn)


