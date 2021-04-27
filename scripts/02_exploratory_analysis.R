#### Preamble ####
# Purpose: Exploring the independence of the variables and picking the right variables for the propensity score
# Author: Ken Lee
# Data: April 19, 2021
# Contact: kenllee97@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data in script 00, and cleaned the data in script 01.


### Workspace setup ###
# Use R Projects, not setwd()
library(tidyverse)
library(corrplot)
library(lsr)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(lubridate)

# Loading the cleaned data
cleaned_data <- readr::read_delim("inputs/data/cleaned_data.csv", delim = ","
)

# Complete data without the injury type to prevent data leakage
full <- subset(cleaned_data, select = -c(INJURY))

# Split into Categorical variables
cat <- subset(full, select = -c(DATE, HOUR, INVAGE))

# Split into Numerical variables
num <- subset(cleaned_data, select = c(HOUR, INVAGE))

# Creating function to get chi square p value and Cramers V
chis = function(x,y) {
  tbl = cat %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 4)
  cramV = round(cramersV(tbl), 4)
  data.frame(x, y, chisq_pval, cramV) }

# Creating unique combinations of column names and sorting for a better plot (upper triangular)
comb = data.frame(t(combn(sort(names(cat)), 2)), stringsAsFactors = F)

# apply function to each variable combination
res = map2_df(comb$X1, comb$X2, chis)

# Saving res into inputs data folder
write_delim(res, "inputs/data/res.csv", na = "NA", delim = ",")

# Saving res into paper folder
write_delim(res, "outputs/paper/res.csv", 
            na = "NA", delim = ",")

# plot results
res %>%
  ggplot(aes(x,y,fill=cramV)) +
  geom_tile() +
  scale_fill_gradient(low="Transparent", high="Red") +
  xlab("Features") +
  ylab("Features") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.95))
        axis.text.y = element_text(vjust = 0.3, hjust = 0.95)

# Creating final data        
final <- select(cleaned_data, c(DATE, HOUR, INVAGE, STREET2, STREET1, ALCOHOL, INJURY))

# Saving cleaned data into inputs data folder
write_delim(final, "inputs/data/final_data.csv", na = "NA", delim = ",")

# Saving cleaned data into paper folder
write_delim(final, "outputs/paper/final_data.csv", 
            na = "NA", delim = ",")

### Exploring Features

# Alcohol
final$ALCOHOL[final$ALCOHOL == 1] <- "Yes"
final$ALCOHOL[final$ALCOHOL == 0] <- "No"

final$ALCOHOL %>% table() %>% 
  kbl(col.names = c("Alcohol", "Frequency")) %>% 
  kable_minimal() %>% 
  kable_material(c("striped", "hover"))

# Injury

final %>% 
  count(INJURY) %>% 
  ggplot(aes(y=n, x=INJURY)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  labs(y = "Frequency", x = "Injury Type")

# Street 1
final %>%
  group_by(STREET1) %>% 
  summarise(Frequency = n())%>% 
  top_n(10) %>% 
  arrange(desc(Frequency)) %>% 
  kbl() %>% 
  kable_minimal()%>% 
  kable_material(c("striped", "hover"))

# Street 2
final %>%
  group_by(STREET2) %>% 
  summarise(Frequency = n())%>% 
  top_n(10) %>% 
  arrange(desc(Frequency)) %>% 
  kbl() %>% 
  kable_minimal()%>% 
  kable_material(c("striped", "hover"))

# Date
temp <- final %>% 
  mutate(year = lubridate::year(final$DATE), 
         month = lubridate::month(final$DATE), 
         day = lubridate::day(final$DATE))

temp %>%
  group_by(year) %>% 
  summarise(Frequency = n()) %>% 
  ggplot(aes(y=Frequency, x=year)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Frequency", x = "Year") +
  scale_x_continuous(name = "Year", breaks = c(2006, 2007, 2008, 2009, 2010, 2011
                              , 2012, 2013, 2014, 2015, 2016, 2017
                              , 2018, 2019)) +
  labs(title = "Year Collision Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

temp %>%
  group_by(month) %>% 
  summarise(Frequency = n()) %>% 
  ggplot(aes(y=Frequency, x=month)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Frequency", x = "Month") +
  scale_x_continuous(name = "Month", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  labs(title = "Month Collision Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

temp %>%
  group_by(day) %>% 
  summarise(Frequency = n()) %>% 
  ggplot(aes(y=Frequency, x=day)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(y = "Frequency", x = "Day of the Month") +
  labs(title = "Day of the Month Collision Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Hour
temp %>%
  group_by(HOUR) %>% 
  summarise(Frequency = n()) %>% 
  ggplot(aes(y=Frequency, x=HOUR)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Hour of Collision Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Age
final %>% 
  group_by(INVAGE) %>% 
  summarise(Frequency = n()) %>% 
  ggplot(aes(y=Frequency, x=INVAGE)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Age Distribution") +
  theme(plot.title = element_text(hjust = 0.5))
