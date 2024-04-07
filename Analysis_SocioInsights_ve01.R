# Project Name: Social Insights
# Author: Bruno Alves de Carvalho
# Status: ongoing


# Set up ------------------------------------------------------------------

# Set the directory
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Load packages for data processing
library(tidyverse)
library(memoise)
library(haven)

# Load functions
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Load color palette
source("R_Scripts/ColorPalette_20240128_ve01.R")

# Load data
aggregated_data_shp <- 
  readRDS("SHP/Data_Aggregated_1999_2022/cached_data_shp.rds")

# Select variables necessary for analysis
selected_vars_shp <- 
  rep(
    list(
      c(
        # id variables
        "idpers",
        "idhous$$",
        # time variable
        "year",
        # social variables
        "age$$",
        "generation",
        "sex$$",
        "civsta$$", 
        "edyear$$", # number of years in education
        # economic variables
        "iptotni", # total income
        "iwyni", # working income
        "noga2m$$", # current job: nomenclature of economic activities
        # geographical variables
        "canton$$",
        "com2_$$",
        # political variables
        "p$$p10", # political position, scale 0 (left) to 10 (right)
        # health and well-being variables
        "p$$c18" # frequency of energy and optimism
      )
    ),
    length(1999:2022)
  )

# Merge data into one single data-set
merged_data_shp <-
  shp_merge_data(aggregated_data_shp, selected_vars_shp)


# Transform Data ----------------------------------------------------------
merged_data_shp$pol_ideology <- 
  ifelse(
    merged_data_shp$`p$$p10` > 5, "right_wing", 
    ifelse(
      merged_data_shp$`p$$p10` < 5 & merged_data_shp$`p$$p10` >= 0, "left_wing", 
      ifelse(
        merged_data_shp$`p$$p10` == 5, "Neutral", NA)
    )
  )

merged_data_shp$age_group_01 <-
  cut(
    merged_data_shp$`age$$`,
    breaks = c(0,18,30,65,125),
    labels = c("minors", "young adults", "adults", "seniors"),
    right = FALSE
  )

merged_data_shp$optimism_scale <- 
  ifelse(
    merged_data_shp$`p$$c18` >= 0,
    merged_data_shp$`p$$c18`,
    NA
  )

merged_data_shp$optimism_group <- 
  ifelse(
    merged_data_shp$`p$$c18` > 5, "optimistic", 
    ifelse(
      merged_data_shp$`p$$c18` < 5 & merged_data_shp$`p$$c18` >= 0, "pessimistic", 
      ifelse(
        merged_data_shp$`p$$c18` == 5, "neither", NA)
    )
  )


# Exploratory Data Analysis -----------------------------------------------
merged_data_shp %>% 
  filter(!is.na(age_group_01) & !is.na(optimism_group)) %>% 
  group_by(year, age_group_01, optimism_group) %>% 
  summarise(n = n()) %>% 
  group_by(year, age_group_01) %>% 
  mutate(prct = n / sum(n)) %>% 
  filter(optimism_group != "neither" & age_group_01 %in% c("young adults", "seniors")) %>% 
  select(year, age_group_01, optimism_group, prct) %>% 
  spread("optimism_group", "prct") %>% 
  mutate(gap = optimistic - pessimistic) %>% 
  ggplot(aes(year, gap, group = age_group_01, color = age_group_01)) + 
  geom_point() + 
  geom_smooth(se = F)
