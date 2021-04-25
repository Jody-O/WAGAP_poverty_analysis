# week 7 Rin3 assignment - data tidying

# load packages ----------------------------------

library(tidyverse)
library(janitor)
library(readxl)

# import WAGAP community survey data -------------------------------------

wagap_raw <- read_excel("data-raw/MASTER WAGAP 2020 CNA Community survey data.xlsx") %>% 
  clean_names()

wagap_tidy_food <- wagap_raw %>% 
  select(timestamp, please_enter_the_zip_code_for_the_community_you_live_in, during_the_12_months_before_covid_19_lockdowns_in_march_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, during_the_months_after_covid_19_lockdowns_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, is_food_a_challenge_in_your_community, please_select_the_races_or_ethnicities_you_most_identify_with, please_mark_the_gender_you_most_identify_with, if_you_said_yes_please_mark_all_the_reasons_food_is_a_problem_for_you_or_for_people_you_know) %>% 
  rename(zip_code = please_enter_the_zip_code_for_the_community_you_live_in, 
         food_stress_b4_covid = during_the_12_months_before_covid_19_lockdowns_in_march_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more) %>% 
  rename(food_stress_during_covid = during_the_months_after_covid_19_lockdowns_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, is_food_a_challenge = is_food_a_challenge_in_your_community, gender = please_mark_the_gender_you_most_identify_with, race_ethnicity = please_select_the_races_or_ethnicities_you_most_identify_with, reasons_food_is_a_problem = if_you_said_yes_please_mark_all_the_reasons_food_is_a_problem_for_you_or_for_people_you_know)

wagap_tidy_food[wagap_tidy_food == "n/a"] <- NA

# clean and group race/ethnicity variable
wagap_tidy_food <- wagap_tidy_food %>% 
  mutate(race_ethnicity = case_when(race_ethnicity %in% c("Pirate", "Blue", "all", "American", "Human", "Jewish", "Russian", "Russian Jewish Immigrant") ~ "other",
                                         race_ethnicity %in% c("Mixed", "mixed race", "Hispanic + Native American", "Mixed/Donít know", "Northern Norwegian Eskimo") ~ "Mixed race", TRUE ~ race_ethnicity))
  
all_gender_responses <- wagap_tidy_food %>% 
  count(gender)

wagap_tidy_food <- wagap_tidy_food %>% 
  mutate(gender = case_when(gender %in% c("gender is fluid according to the whim of the day", "Transgender", "Male & Female" ~ "Gender non-conforming", TRUE ~ gender)))
  
  # mutate(gender, "Prefer not to answer" = NA) HOW CHANGE na INTO AND CHARACTER VALUE????
  






