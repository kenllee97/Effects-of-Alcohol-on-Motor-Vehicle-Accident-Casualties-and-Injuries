### Examining-the-Effects-of-Alcohol-on-Motor-Vehicle-Accident-Casualties-and-Injuries ###

## Project Description:
This is an R Project analyzing the effects of alcohol on the collision injury types. The purpose of this project is to conduct a propensity score matching, leveraging a logistic regression mode, on the "Motor Vehicle Collisions involving Killed or Seriously Injured Persons" data set from Open Data Toronto, and identify the key predictors of alcohol use while driving, while also its effects on potential collision injuries.

## Prerequisites:
- opendatatoronto
- tidyverse
- janitor
- ggplot2
- knitr
- kableExtra
- corrplot
- lsr
- lubridate
- broom
- arm
- caret

## Steps:
1. Install required packages
2. Execute script: 00_vehicle_collision_toront_data_import (Found in the scripts folder)
3. Execute script: 01_vehicle_collision_toronto_data_cleaning (Found in the scripts folder)
4. Execute script: 02_exploratory_analysis (Found in the scripts folder)
5. Execute script: 03_propensity_score (Found in the scripts folder)
6. Run the R markdown Effects-of-Alcohol-on-Motor-Vehicle-Accident-Casualties-and-Injuries.Rmd found in the outputs/paper folder.

## License:
This project is licensed under the MIT License.