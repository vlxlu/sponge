#set your wd

library(dplyr)
library(ggplot2)
#Things to potentially flag:
#CA_S5_01 puff #4 is lagging edge
#DA_S35_02 puff #3 is lagging edge
#CA)S62)_O1 puff #5 is short (cut?)

sponge_flow <- read.csv("sponge_tracks_26June2026.csv")
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

speed_mean <- speed_perpuff %>%
  group_by(osculum_ID) %>%
  summarise(
    mean_speed = mean(speed_cm_s, na.rm = TRUE),
    sd_speed   = sd(speed_cm_s, na.rm = TRUE),
    .groups = "drop"
  )
print(speed_mean, n = Inf)

#See if it matters trimming last cm off from each video
 #note, each video will still be variable in length via this trim
speed_perpuff_trimmed <- sponge_flow %>%
  filter(osculum_ID != "") %>%
  arrange(osculum_ID, tracking_num, t) %>%
  group_by(osculum_ID, tracking_num) %>%
  filter(r <= max(r) - 1) %>%
  summarise(
    abs_distance_cm = abs(last(r) - first(r)),
    time_elapsed_s  = last(t) - first(t),
    speed_cm_s      = abs_distance_cm / time_elapsed_s,
    .groups = "drop"
  )
comparison <- speed_perpuff %>%
  rename(speed_full = speed_cm_s) %>%
  left_join(
    speed_perpuff_trimmed %>% rename(speed_trimmed = speed_cm_s),
    by = c("osculum_ID", "tracking_num")
  )

comparison <- speed_perpuff %>%
  select(osculum_ID, tracking_num, speed_cm_s) %>%
  rename(speed_full = speed_cm_s) %>%
  left_join(
    speed_perpuff_trimmed %>% select(osculum_ID, tracking_num, speed_cm_s) %>% rename(speed_trimmed = speed_cm_s),
    by = c("osculum_ID", "tracking_num")
  )
print(comparison, n = Inf)
#Here you can see speed generally goes down a little bit if we cut off last cm

#Here, what is we trim to only keep first 2.5 cm max. This will make videos approximately hte same 
#length
speed_perpuff_2.5cm <- sponge_flow %>%
  filter(osculum_ID != "") %>%
  arrange(osculum_ID, tracking_num, t) %>%
  group_by(osculum_ID, tracking_num) %>%
  filter(r <= first(r) + 2.5) %>%
  summarise(
    abs_distance_cm = abs(last(r) - first(r)),
    time_elapsed_s  = last(t) - first(t),
    speed_cm_s      = abs_distance_cm / time_elapsed_s,
    .groups = "drop"
  )

comparison <- speed_perpuff %>%
  select(osculum_ID, tracking_num, speed_cm_s) %>%
  rename(speed_full = speed_cm_s) %>%
  left_join(
    speed_perpuff_trimmed %>% select(osculum_ID, tracking_num, speed_cm_s) %>% rename(speed_trimmed = speed_cm_s),
    by = c("osculum_ID", "tracking_num")
  ) %>%
  left_join(
    speed_perpuff_2.5cm %>% select(osculum_ID, tracking_num, speed_cm_s) %>% rename(speed_2.5cm = speed_cm_s),
    by = c("osculum_ID", "tracking_num")
  )
print(comparison)
cor(comparison[, c("speed_full", "speed_trimmed", "speed_2.5cm")], use = "complete.obs")

#Checking whether velocity changes in different video segments

velocity_profile <- sponge_flow %>%
  filter(osculum_ID != "", !is.na(v)) %>%
  arrange(osculum_ID, tracking_num, t) %>%
  group_by(osculum_ID, tracking_num) %>%
  mutate(
    obs_index  = row_number(),
    n_obs      = n(),
    rel_pos    = obs_index / n_obs,
    segment    = case_when(
      obs_index <= 3         ~ "early",
      obs_index >= n_obs - 5 ~ "late",
      TRUE                   ~ "middle"
    )
  ) %>%
  ungroup()

velocity_profile %>%
  #filter(segment != "middle") %>%
  group_by(segment) %>%
  summarise(mean_v = mean(v, na.rm = TRUE),
            sd_v   = sd(v, na.rm = TRUE))

#Interesting...overall, middle segment is slowest (marginally)
#I think we want to trim off first few tracks and last bit


## Read in sponge morphology data set
sponge_size <- read.csv("sponge_size_data.csv")

#Join sponge size data for matching sponges
speed_mean <- speed_mean %>%
  left_join(sponge_size, by = c("osculum_ID" = "id"))

#Look at how many per species and make boxplot of mean speeds (note:
#this is DIFFERENT FROM FLOW!!)
speed_mean %>%
  count(species)

ggplot(speed_mean, aes(x = species, y = mean_speed)) +
  geom_boxplot() +
  labs(x = "Species", y = "Mean speed (cm/s)") +
  theme_classic()

#So far, particle velocity not sig different across three species
model <- aov(mean_speed ~ species, data = speed_mean)
summary(model)

#Ok, let's look at flow rate...this requires different columns for different species
#becaues they have different shapes.

speed_mean <- speed_mean %>%
  mutate(
    osc_diam1_cm = as.numeric(na_if(as.character(osc_diam1_cm), ".")),
    osc_diam2_cm = as.numeric(na_if(as.character(osc_diam2_cm), "."))
  )

#first calculator cross sectional area
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
  theme_classic()

#Now calculate flow, make box plot, and run anova
speed_mean <- speed_mean %>%
  mutate(osc_flow = cross_sec_area * mean_speed)
ggplot(speed_mean, aes(x = species, y = osc_flow)) +
  geom_boxplot() +
  labs(x = "Species", y = "Osculum flow (cm³/s)") +
  theme_classic()

model_flow <- aov(osc_flow ~ species, data = speed_mean)
summary(model_flow)
TukeyHSD(model_flow)
##tmn is significantly higher than other two

#Think about applying correction of 0.5...look for old papers....
#we will also want to emphasize that this is about RELATIVE speed, not necessarily
#getting perfect measurement of flow!

