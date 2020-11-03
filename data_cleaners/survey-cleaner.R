#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Jianzhong You, Ziyue Xu
# Data: 2nd November 2020
# Contact: max.you@mail.utoronto.ca, ziyue.xu@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_survey <- read_dta("inputs/ns20200625/ns20200625.dta")

# Add the labels
raw_data_survey <- labelled::to_factor(raw_data_survey)
survey.data.name <- names(raw_data_survey)

# keep varibles of interests
reduced_data_survey <- 
  raw_data_survey %>% 
  select(vote_2020, # for post stratification
         household_income,
         race_ethnicity,
         gender,
         state,
         age)

# create one variable for each candidate for future modeling
reduced_data_survey<-
  reduced_data_survey %>% 
  mutate(vote_trump = ifelse(vote_2020=="Donald Trump", 1, 0),
         vote_biden = ifelse(vote_2020=="Joe Biden", 1, 0))

# mutate the income categories
reduced_data_survey <- reduced_data_survey %>%
  mutate(household_income_modified = case_when(household_income == 'Less than $14,999' | household_income == '$15,000 to $19,999' | household_income == '$20,000 to $24,999' ~ 'Less than $25,000',
                                               household_income == '$25,000 to $29,999' | 
                                                 household_income == '$30,000 to $34,999' | 
                                                 household_income == '$35,000 to $39,999' | 
                                                 household_income == '$40,000 to $44,999' |
                                                 household_income == '$45,000 to $49,999' ~ '$25,000 to $49,999',
                                               household_income == '$50,000 to $54,999' |
                                                 household_income == '$55,000 to $59,999' |
                                                 household_income == '$60,000 to $64,999' |
                                                 household_income == '$65,000 to $69,999' |
                                                 household_income == '$70,000 to $74,999' ~ '$50,000 to $74,999',
                                               household_income == '$75,000 to $79,999' |
                                                 household_income == '$80,000 to $84,999' |
                                                 household_income == '$85,000 to $89,999' |
                                                 household_income == '$90,000 to $94,999' |
                                                 household_income == '$95,000 to $99,999' ~ '$75,000 to $100,000',
                                               household_income == '$100,000 to $124,999' |
                                                 household_income == '$125,000 to $149,999' ~ '$100,000 to $149,999',
                                               household_income == '$150,000 to $174,999' |
                                                 household_income == '$175,000 to $199,999' ~ '$150,000 to $199,999',
                                               household_income == '$200,000 to $249,999' ~ '$200,000 to  $249,000',
                                               household_income == '$250,000 and above' ~ 'More than $250,000',
                                               TRUE ~ 'No response'))

# mutate race category
reduced_data_survey <- reduced_data_survey %>%
  mutate(race_modified = case_when(grepl("Asian", race_ethnicity, fixed=TRUE) ~ "Asian",
                                   grepl("Pacific Islander", race_ethnicity, fixed=TRUE) ~ "Pacific Islander",
                                   TRUE ~ as.character(race_ethnicity)))

# mutate state category, match from survey to census
reduced_data_survey<-
  reduced_data_survey %>%
  mutate(state_modified = case_when(state == "WI" ~ "wisconsin",
                                    state == "VA" ~ "virginia",
                                    state == "TX" ~ "texas",
                                    state == "WA" ~ "washington",
                                    state == "OH" ~ "ohio",
                                    state == "MA" ~ "massachusetts",
                                    state == "CA" ~ "california",
                                    state == "NC" ~ "north carolina",
                                    state == "MD" ~ "maryland",
                                    state == "FL" ~ "florida",
                                    state == "WV" ~ "west virginia",
                                    state == "NY" ~ "new york",
                                    state == "KY" ~ "kentucky",
                                    state == "IN" ~ "indiana",
                                    state == "MI" ~ "michigan",
                                    state == "IA" ~ "iowa",
                                    state == "SC" ~ "south carolina",
                                    state == "MN" ~ "minnesota",
                                    state == "GA" ~ "georgia",
                                    state == "PA" ~ "pennsylvania",
                                    state == "NJ" ~ "new jersey",
                                    state == "AZ" ~ "arizona",
                                    state == "IL" ~ "illinois",
                                    state == "AR" ~ "arkansas",
                                    state == "OK" ~ "oklahoma",
                                    state == "NV" ~ "nevada",
                                    state == "OR" ~ "oregon",
                                    state == "CT" ~ "connecticut",
                                    state == "DE" ~ "delaware",
                                    state == "MO" ~ "missouri",
                                    state == "CO" ~ "colorado",
                                    state == "DC" ~ "district of columbia",
                                    state == "NM" ~ "new mexico",
                                    state == "TN" ~ "tennessee",
                                    state == "HI" ~ "hawaii",
                                    state == "MT" ~ "montana",
                                    state == "VT" ~ "vermont",
                                    state == "UT" ~ "utah",
                                    state == "NE" ~ "nebraska",
                                    state == "KS" ~ "kansas",
                                    state == "NH" ~ "new hampshire",
                                    state == "LA" ~ "louisiana",
                                    state == "ME" ~ "maine",
                                    state == "AL" ~ "alabama",
                                    state == "ID" ~ "idaho",
                                    state == "MS" ~ "mississippi",
                                    state == "SD" ~ "south dakota",
                                    state == "WY" ~ "wyoming",
                                    state == "ND" ~ "north dakota",
                                    state == "AK" ~ "alaska",
                                    state == "RI" ~ "rhode island",
                                    TRUE ~ as.character(state)))

# mutate age category
reduced_data_survey <- reduced_data_survey %>%
  mutate(age_modified = case_when(age < 30 ~ "age 18-30",
                                   age < 45 ~ "age 30-44",
                                   age < 60 ~ "age 40-59",
                                   age >= 60 ~ "age 60+"))

# lower the case of the gender
reduced_data_survey <- reduced_data_survey %>%
  mutate(sex_modified = case_when(gender == 'Male' ~ "male",
                                  gender == 'Female' ~ "female",
                                  TRUE ~ as.character(gender)))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data_survey, "survey_data.csv")



