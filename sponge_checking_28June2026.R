---
title: "data_checking_sponge_28June"
author: "vlxlu"
date: "2026-06-28"
output: html_document
---
  
# Set up ------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
sponge_prelim <- read.csv("raw_data/sponge_tracks_26June2026.csv")
sponge_size <- read.csv("raw_data/sponge_size_data.csv")


# data cleaning -----------------------------------------------------------

View(sponge_prelim)
sponge_prelim_clean <- sponge_prelim %>% 
  select(osculum_ID,
         tracking_num,
         t,
         x,
         y,
         r,
         v)

write.csv(sponge_prelim_clean, file = "data/sponge_tracks_26June2026_clean.csv")
