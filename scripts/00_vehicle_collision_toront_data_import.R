### Preamble ###
# Purpose: Using opendatatoronto to obtain the motor vehicle accidents
# Author: Ken Lee
# Contact: kenllee97@gmail.com
# Date: April 4, 2021
# Prerequisites None
# TODOs: -

### Workspace set-up ###
library(opendatatoronto)
library(tidyverse)

###Get Data###
raw_data <- 
  opendatatoronto::search_packages("Motor Vehicle Collisions") %>% 
  opendatatoronto::list_package_resources() %>% 
  filter(title == "Motor Vehicle Collisions involving Killed or Seriously Injured Persons") %>% # This is the row we are interested in.
  select(id) %>% 
  opendatatoronto::get_resource()

### Save Data ### 
# I have decided to use write_delim because using write_csv ends up creating
# a duplicate column of id when I read/load it back in later.
write_delim(raw_data, "Motor_Vehicle_Collisions_with_KSI_Data.csv", na = "NA", delim = ",")
