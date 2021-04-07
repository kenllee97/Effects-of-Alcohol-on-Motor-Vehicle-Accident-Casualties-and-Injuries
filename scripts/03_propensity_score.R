#### Preamble ####
# Purpose: Creating Propensity Score for the data
# Author: Ken Lee
# Data: April 6, 2021
# Contact: kenllee97@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data in script 00, and cleaned the data in script 01.


### Workspace setup ###
# Use R Projects, not setwd().
library(tidyverse)
library(huxtable)
library(broom)

# Load/Read in the cleaned data.
cleaned_data <- readr::read_delim("inputs/data/cleaned_data.csv", delim = ","
)

propensity_score <- glm(ALCOHOL ~ DATE + HOUR + STREET1 + STREET2 + ROAD_CLASS + District + Division + 
                          LOCCOORD + TRAFFCTL + VISIBILITY + LIGHT + RDSFCOND + IMPACTYPE + INVTYPE + 
                          INVAGE + VEHTYPE + PEDESTRIAN + CYCLIST + AUTOMOBILE + MOTORCYCLE + TRUCK +
                          TRSN_CITY_VEH + EMERG_VEH + PASSENGER + SPEEDING + AG_DRIV + REDLIGHT + DISABILITY +
                          Hood_ID,
                        family = binomial,
                        data = cleaned_data,
                        maxit = 100)

# Saving propensity scores into inputs data folder
write_rds(propensity_score, "inputs/models/propensity_score.rds")

# Loading model
propensity_score <- read_rds("inputs/models/propensity_score.rds")

collision_data <-
  augment(propensity_score,
          data = cleaned_data,
          type.predict = "response") %>% 
  dplyr::select(-.resid, -.std.resid, -.hat, -.sigma, -.cooksd)
