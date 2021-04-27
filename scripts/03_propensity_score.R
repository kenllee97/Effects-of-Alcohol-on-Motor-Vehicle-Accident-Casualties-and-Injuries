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
library(broom)
library(arm)


# Load/Read in the final data with the right classes
final <- read.csv("inputs/data/final_data.csv", sep = ",", colClasses = c( HOUR = "numeric",
                                                                           INVAGE = "numeric", 
                                                                           STREET1 = "factor",
                                                                           STREET2 = "factor", 
                                                                           ALCOHOL = "factor", 
                                                                           INJURY = "factor"))
# Reconverting the dates into class date
final$DATE <- as.Date(final$DATE)

# Creating propensity weights
propensity_score <- glm(ALCOHOL ~ DATE + HOUR + STREET1 + STREET2 + INVAGE,
                        family = binomial,
                        data = final,
                        maxit = 100)

# Saving propensity scores into inputs data folder
write_rds(propensity_score, "inputs/models/propensity_score.rds")

# Loading model
propensity_score <- read_rds("inputs/models/propensity_score.rds")

collision_data <-
  augment(propensity_score,
          data = final,
          type.predict = "response") %>% 
  dplyr::select(-.resid, -.std.resid, -.hat, -.sigma, -.cooksd)

# Saving collision data with scores into inputs data folder
write_delim(collision_data, "inputs/data/final_collision_data_scores.csv", na = "NA", delim = ",")

# Saving data with collision scores into paper folder
write_delim(collision_data, "outputs/paper/final_collision_data_scores.csv", 
            na = "NA", delim = ",")

# Arranging scores and alcohol data
collision_data <- 
  collision_data %>% 
  arrange(.fitted, ALCOHOL)

# Adding a treated column
collision_data$treated <- if_else(collision_data$ALCOHOL == 0, 0, 1)

# Converting treated variable to an integer
collision_data$treated <- as.integer(collision_data$treated)

# Using the arm package to find matches
matches <- matching(z = collision_data$treated, score = collision_data$.fitted)

# Binding collision data and the matches
collision_data <- cbind(collision_data, matches)

# Filtering to samples that got matches
collisions_matched <- 
  collision_data %>% 
  filter(match.ind != 0)

# Filtering out match.ind, pairs, treated, and cnts which are no longer needed
collisions_matched <- subset(collisions_matched, select = (-c(match.ind, pairs, treated, cnts)))

# Saving processed data into data and paper folder
write_delim(collisions_matched, "outputs/paper/final_processed_data.csv", 
            na = "NA", delim = ",")

write_delim(collisions_matched, "inputs/data/final_processed_data.csv", na = "NA", delim = ",")

# Loading final processed data
final_processed <- readr::read_delim("inputs/data/final_processed_data.csv", delim = ","
)

# Changing Alcohol Values for graph
final_processed$ALCOHOL[final_processed$ALCOHOL == 1] <- "Treated"
final_processed$ALCOHOL[final_processed$ALCOHOL == 0] <- "Control"

# Setting colors
c <- c("#add8e6", "#ffcccb")

# Checking the distribution of Injuries in each group

final_processed %>% group_by(ALCOHOL) %>% 
  count(INJURY) %>% 
  ggplot(aes(fill=ALCOHOL, y=n, x=INJURY)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  labs(y = "Accidents/Collisions (Total = 418)", x = "Injury Type")

# Creating table with percentages
final_processed %>% group_by(ALCOHOL) %>% 
  count(INJURY) %>% 
  mutate(Percentage = round(((n/418)*100), 2)) %>%
  dplyr::select(ALCOHOL, INJURY, Percentage) %>% 
  spread(ALCOHOL, Percentage) %>% 
  kbl(align = "cccc",
      caption = "Street 2 Distribution") %>% 
  kable_minimal()%>% 
  kable_material(c("striped", "hover"))
