#### Preamble ####
# Purpose: Cleaning the raw data
# Author: Ken Lee
# Data: April 4, 2021
# Contact: kenllee97@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the "Motor Vehicle Collisions involving Killed or Seriously Injured Persons" data and saved it to inputs/data


### Workspace setup ###
# Use R Projects, not setwd().
library(tidyverse)
library(janitor)

# Load/Read in the raw data.
raw_data <- readr::read_delim("inputs/data/Motor_Vehicle_Collisions_with_KSI_Data.csv", delim = ","
)

# Checking the number of NA values for each column.
NA_values <- raw_data %>% 
  is.na() %>% 
  colSums()

# Reviewing how many missing values there are for each column
print(NA_values) 
# We can see that OFFSET, LATITUDE, LONGITUDE, ACCLOC, FATAL_NO, INITDIR, MANOEUVER, DRIVACT, DRIVCOND,
# PEDTYPE, PEDACT, PEDCOND, CYCLISTYPE, CYCACT, and CYCCOND have too many null values (More than 30%)

#Removing said features
cleaned_data <- subset(raw_data, select = -c(OFFSET, LATITUDE, LONGITUDE, ACCLOC, FATAL_NO, INITDIR,
                                             MANOEUVER, DRIVACT, DRIVCOND, PEDTYPE, PEDACT, PEDCOND, 
                                             CYCLISTYPE, CYCACT, CYCCOND))

# Replacing NA values in certain key columns with the value "No" as it can be inferred it represents no since there were only Yes variables.
cleaned_data <- cleaned_data %>% 
  replace_na(list(PEDESTRIAN = 0, CYCLIST = 0, AUTOMOBILE = 0, MOTORCYCLE = 0, 
                  TRUCK = 0, TRSN_CITY_VEH = 0, EMERG_VEH = 0, PASSENGER = 0, 
                  SPEEDING = 0, AG_DRIV = 0, REDLIGHT = 0, ALCOHOL = 0, 
                  DISABILITY = 0))

# Turning Date Time to just Date
cleaned_data$DATE <- as.Date(cleaned_data$DATE)

# Removing unnecessary and redundant features
cleaned_data <- subset(cleaned_data, select = -c(X_id, Index_, ACCNUM, YEAR, TIME, ObjectId, geometry))

# Removing ward number because the missing values may remove key values.
# Additionally, the two streets, district, and division are already in it, so it should be enough to control for location
cleaned_data <- subset(cleaned_data, select = -c(WardNum))

# Removing Accident Class since Injuries already denote whether there was a fatality in the accident.
cleaned_data <- subset(cleaned_data, select = -c(ACCLASS))

# Removing Neighbourhood because it is redundant since Hood_ID is already present
cleaned_data <- subset(cleaned_data, select = -c(Neighbourhood))

# Classifying unknown ages and NULL. Removing unknown ages since we cannot control them
cleaned_data$INVAGE[cleaned_data$INVAGE == "unknown"] <- NA

# Classifying "Other" values in Visibility as NA, since we cannot control for it
cleaned_data$VISIBILITY[cleaned_data$VISIBILITY == "Other"] <- NA

# Classifying "Other" values in Light as NA, since we cannot control for it
cleaned_data$LIGHT[cleaned_data$LIGHT == "Other"] <- NA

# Classifying "Other" values in RDSFCOND as NA, since we cannot control for it
cleaned_data$RDSFCOND[cleaned_data$RDSFCOND == "Other"] <- NA

# Classifying "Other" values in IMPACTYPE as NA, since we cannot control for it
cleaned_data$IMPACTYPE[cleaned_data$IMPACTYPE == "Other"] <- NA

# Classifying "Other" values in INVTYPE as NA, since we cannot control for it
cleaned_data$INVTYPE[cleaned_data$INVTYPE == "Other"] <- NA

# Dropping remaining null values
cleaned_data <- cleaned_data %>% 
  drop_na()

# creating filter to exclude date to easily replace "yes" with "1"
f <- sapply(cleaned_data, class) != 'Date'

# Turning all "Yes" to 1
cleaned_data[f][cleaned_data[f] == 'Yes'] <- '1'

# Turning Age bins into Ratio data
cleaned_data$INVAGE[cleaned_data$INVAGE == "0 to 4"] <- 2
cleaned_data$INVAGE[cleaned_data$INVAGE == "5 to 9"] <- 7
cleaned_data$INVAGE[cleaned_data$INVAGE == "10 to 14"] <- 12
cleaned_data$INVAGE[cleaned_data$INVAGE == "15 to 19"] <- 17
cleaned_data$INVAGE[cleaned_data$INVAGE == "20 to 24"] <- 22
cleaned_data$INVAGE[cleaned_data$INVAGE == "25 to 29"] <- 27
cleaned_data$INVAGE[cleaned_data$INVAGE == "30 to 34"] <- 32
cleaned_data$INVAGE[cleaned_data$INVAGE == "35 to 39"] <- 37
cleaned_data$INVAGE[cleaned_data$INVAGE == "40 to 44"] <- 42
cleaned_data$INVAGE[cleaned_data$INVAGE == "45 to 49"] <- 47
cleaned_data$INVAGE[cleaned_data$INVAGE == "50 to 54"] <- 52
cleaned_data$INVAGE[cleaned_data$INVAGE == "55 to 59"] <- 57
cleaned_data$INVAGE[cleaned_data$INVAGE == "60 to 64"] <- 62
cleaned_data$INVAGE[cleaned_data$INVAGE == "65 to 69"] <- 67
cleaned_data$INVAGE[cleaned_data$INVAGE == "70 to 74"] <- 72
cleaned_data$INVAGE[cleaned_data$INVAGE == "75 to 79"] <- 77
cleaned_data$INVAGE[cleaned_data$INVAGE == "80 to 84"] <- 82
cleaned_data$INVAGE[cleaned_data$INVAGE == "85 to 89"] <- 87
cleaned_data$INVAGE[cleaned_data$INVAGE == "90 to 94"] <- 92
cleaned_data$INVAGE[cleaned_data$INVAGE == "Over 95"] <- 97

### Save Cleaned Data ###

# Saving cleaned data into inputs data folder
write_delim(cleaned_data, "inputs/data/cleaned_data.csv", na = "NA", delim = ",")

# Saving cleaned data into paper folder
write_delim(cleaned_data, "outputs/paper/cleaned_data.csv", 
            na = "NA", delim = ",")


