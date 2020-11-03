#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from IPUMS USA https://usa.ipums.org/usa/
# Author: Jianzhong You, Ziyue Xu
# Data: 2nd November 2020
# Contact: max.you@mail.utoronto.ca, ziyue.xu@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data.
raw_data_census <- read_dta("inputs/usa_00007.dta.gz")


# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)
census.data.names <- names(raw_data_census)

# select variables of interests
reduced_data_census <- 
  raw_data_census %>% 
  select(hhincome, sex, stateicp, age, racamind, racasian, racblk, racpacis, racwht, racother)


levels(reduced_data_census$age) <- c(levels(reduced_data_census$age), 0, 90)

# rename the values for ages
reduced_data_census$age[reduced_data_census$age == "less than 1 year old"] <- 0
reduced_data_census$age[reduced_data_census$age == "90 (90+ in 1980 and 1990)"] <- 90

# remove all ages unders 18
reduced_data_census$age <- as.integer(reduced_data_census$age)
reduced_data_census <- reduced_data_census %>% filter(age >= 18)

# map the household income to the survey category
reduced_data_census$hhincome <- as.integer(reduced_data_census$hhincome)
reduced_data_census <- reduced_data_census %>%
  mutate(hhincome_modified = case_when(hhincome < 25000 ~ 'Less than $25,000',
                                       hhincome < 50000 ~ '$25,000 to $49,999',
                                       hhincome < 75000 ~ '$50,000 to $74,999',
                                       hhincome < 100000 ~ '$75,000 to $100,000',
                                       hhincome < 150000 ~ '$100,000 to $149,999',
                                       hhincome < 200000 ~ '$150,000 to $199,999',
                                       hhincome < 250000 ~ '$200,000 to  $249,000',
                                       hhincome >= 250000 ~ 'More than $250,000',
                                       TRUE ~ 'No response'))

# map race to survey category
reduced_data_census <- reduced_data_census %>%
  mutate(race_modifed = case_when(
    racwht == "yes" ~ "White",
    racblk == "yes" ~ "Black, or African American",
    racpacis == "yes" ~ "Pacific Islander",
    racasian == "yes" ~ "Asian",
    racamind == "yes" ~ "American Indian or Alaska Native",
    racother == "yes" ~ "Some other race",
    TRUE ~ "Some other race"))

# map census age
reduced_data_census$age <- as.integer(reduced_data_census$age)
reduced_data_census <- reduced_data_census %>%
  mutate(age_modified = case_when(age < 30 ~ "age 18-30",
                                  age < 45 ~ "age 30-44",
                                  age < 60 ~ "age 40-59",
                                  age >= 60 ~ "age 60+"))

# grouping of variables of interest as combination
reduced_data_census <- labelled::to_factor(reduced_data_census)
census_data <- reduced_data_census %>%
  count(stateicp, hhincome_modified, age_modified, sex, race_modifed) %>% 
  group_by(stateicp, hhincome_modified, age_modified, sex, race_modifed)
# rename them to make it more meaningful
names(census_data) <- c("state_modified", "household_income_modified", "age_modified", "sex_modified", "race_modified", "number")

# Saving the census data as a csv file in my
# working directory
write_csv(census_data, "census_data.csv")



         