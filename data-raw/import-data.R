# week 7 Rin3 project assignment - data tidying

# load packages ----------------------------------

library(tidyverse)
library(janitor)
library(readxl)

# import WAGAP community survey data -------------------------------------

wagap_raw <- read_excel("data-raw/MASTER WAGAP 2020 CNA Community survey data.xlsx",
                        sheet = 1) %>% 
  clean_names()

wagap_tidy_food <- wagap_raw %>% 
  select(timestamp, please_enter_the_zip_code_for_the_community_you_live_in, during_the_12_months_before_covid_19_lockdowns_in_march_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, during_the_months_after_covid_19_lockdowns_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, is_food_a_challenge_in_your_community, please_select_the_races_or_ethnicities_you_most_identify_with, please_mark_the_gender_you_most_identify_with, if_you_said_yes_please_mark_all_the_reasons_food_is_a_problem_for_you_or_for_people_you_know, how_many_people_are_in_your_household_sharing_income_and_expenses, during_the_past_year_what_was_the_total_gross_income_for_your_household) %>% 
  rename(zip_code = please_enter_the_zip_code_for_the_community_you_live_in, 
         food_stress_b4_covid = during_the_12_months_before_covid_19_lockdowns_in_march_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more) %>% 
  rename(food_stress_during_covid = during_the_months_after_covid_19_lockdowns_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, is_food_a_challenge = is_food_a_challenge_in_your_community, gender = please_mark_the_gender_you_most_identify_with, race_ethnicity = please_select_the_races_or_ethnicities_you_most_identify_with, reasons_food_is_a_problem = if_you_said_yes_please_mark_all_the_reasons_food_is_a_problem_for_you_or_for_people_you_know, household_income = during_the_past_year_what_was_the_total_gross_income_for_your_household, household_size = how_many_people_are_in_your_household_sharing_income_and_expenses)

# Alternate way to change variable names using a separate tibble or csv listing old and new names:
# updated_colnames <- tribble(
  ~old_column_name, ~new_column_name,
  "please_enter_the_zip_code_for_the_community_you_live_in", "zip_code",
  "during_the_12_months_before_covid_19_lockdowns_in_march_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more", "food_stress_b4_covid", "is_food_a_challenge_in_your_community", "is_food_a_challenge") %>% 
  select(new_column_name, old_column_name) %>% 
  # select() is used to re-order the columns
  deframe()
# deframe() from dplyr converts a 2-column tibble into a named vector, which has to happen before using the vector in rename().  A csv file could be used instead of the tibble to do the same rename process.

#wagap_raw %>% 
#  select(timestamp, please_enter_the_zip_code_for_the_community_you_live_in, during_the_12_months_before_covid_19_lockdowns_in_march_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, during_the_months_after_covid_19_lockdowns_did_you_or_the_people_you_live_with_worry_that_you_would_run_out_of_food_before_you_were_able_to_get_more, is_food_a_challenge_in_your_community, please_select_the_races_or_ethnicities_you_most_identify_with, please_mark_the_gender_you_most_identify_with, if_you_said_yes_please_mark_all_the_reasons_food_is_a_problem_for_you_or_for_people_you_know) %>% 
#  rename(updated_colnames)


wagap_tidy_food <- wagap_tidy_food %>% 
  mutate(across(everything(), ~na_if(.x, "n/a")))

# alternate way to replace all "n/a" with NA:
# wagap_tidy_food[wagap_tidy_food == "n/a"] <- NA


# Delete repeated entries (by timestamp) ----------------------------------
wagap_tidy_food <- wagap_tidy_food %>% 
  distinct(timestamp, .keep_all = TRUE)


# clean and group race/ethnicity variable  -----------------------------
# NOTE to self:  if you're looking across many job titles that contain the word “Manager” then you would be looking for a substring - use str_detect(). If you're looking for either “Technical Manager” or “Team Manager” use %in%

wagap_tidy_food <- wagap_tidy_food %>% 
  drop_na() %>% 
  mutate(race_ethnicity = case_when(race_ethnicity %in% c("Pirate", "Blue", "all", "American", "Human", "Jewish", "Russian", "Russian Jewish Immigrant") ~ "Other",
                                         race_ethnicity %in% c("Mixed", "mixed race", "Hispanic + Native American", "Mixed/Donít know", "Mixed/Don’t know", "Northern Norwegian Eskimo") ~ "Mixed race", TRUE ~ race_ethnicity))



# Group gender responses --------------------------------------------------

all_gender_responses <- wagap_tidy_food %>% 
  count(gender)

wagap_tidy_food <- wagap_tidy_food %>% 
  mutate(gender = case_when(gender %in% c("gender is fluid according to the whim of the day", "Transgender", "Male & Female") ~ "Gender non-conforming", TRUE ~ gender)) %>% 
  mutate(gender = replace_na(gender, "Prefer not to answer"))


# Separate responses to "reasons food is a problem?" ----------------------------

food_problems <- wagap_tidy_food %>% 
  select(timestamp, reasons_food_is_a_problem, race_ethnicity) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, " [(]pantries, food bank, gleaning, etc.[)]")) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, ", n/a"))

  
food_response_options <- tribble(
  ~selected_response, ~response_column_name,
  "Lack of transportation to grocery stores or markets", "food_problem_lack_of_transportation",
  "Not enough alternative food sources available", "food_problem_not_enough_alternatives",
  "Reduced access to free and reduced school meals because of COVID-19 school closures", "food_problem_reduced_access",
  "Not enough income to purchase food", "food_problem_not_enough_income"
)


# create separate cols for each stock answer & remove all stock answers from 'reasons_food_is_a_problem' col & remove extra commas using regex (language character manipulation)
food_problems_categorized <- food_problems %>% 
  select(reasons_food_is_a_problem) %>% 
  mutate(food_problem_lack_of_transportation = str_detect(reasons_food_is_a_problem, "Lack of transportation to grocery stores or markets"),
         food_problem_not_enough_alternatives = str_detect(reasons_food_is_a_problem, "Not enough alternative food sources available"),
         food_problem_reduced_access = str_detect(reasons_food_is_a_problem, "Reduced access to free and reduced school meals because of COVID-19 school closures"),
         food_problem_not_enough_income = str_detect(reasons_food_is_a_problem, "Not enough income to purchase food")) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Lack of transportation to grocery stores or markets"),
         reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Not enough alternative food sources available"),
         reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Reduced access to free and reduced school meals because of COVID-19 school closures"),
         reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Not enough income to purchase food")) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "^,")) %>% 
  mutate(reasons_food_is_a_problem = str_trim(reasons_food_is_a_problem),
         reasons_food_is_a_problem = ifelse(reasons_food_is_a_problem == "", NA, reasons_food_is_a_problem)) %>% 
  mutate(reasons_food_is_a_problem = ifelse(str_detect(reasons_food_is_a_problem, "[:alnum:]"), reasons_food_is_a_problem, NA)) %>% 
# more complicated regex option:
  #   mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, ",[^[:alnum:]]*$")) %>%
  #   mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "^ ,[^[:alnum:]]*")) %>%
mutate(unique_food_problems = reasons_food_is_a_problem) %>% 
  select(-reasons_food_is_a_problem)


# QUESTION: why are all food_problem write_in values TRUE below?  One problem is that multiple answers in each cell have not been separated into their own row yet, but even single stock answers show up as TRUE.  It doesn't affect the .Rmd calculations because food_problems_categorized was used instead of food_problems.  But I still want to know why the ifelse is returning all TRUE values.

food_problems %>% 
  mutate(write_in = ifelse(reasons_food_is_a_problem %in% food_response_options,
                           FALSE,
                           TRUE))

# NOTES to self: 
# to get all answers into the same column:

# I wanted to combine all responses from reasons_food_is_a_problem into one column.  The survey allowed each respondent to select more than one 'reason', and the resulting Excel file kept all selected responses in one column, separated by commas.  I wanted to create a chart showing how many times each problem type was selected, so I needed to separate all responses, then pivot so all responses ('reasons') end up as individual values all in the same variable column.
  
# this doesn't work:  
# food_problems %>% 
# separate(reasons_food_is_a_problem, 
#           sep = ",",
#           into = c("prob1", "prob2", "prob3", "prob4", "prob5", "prob6", "prob7", "prob8", "prob9"))
# pivot_longer((cols = prob1, prob2, prob3, prob4, prob5, prob6, prob7, prob8, prob9), values_to = "reasons_food_is_a_problem")


# use separate_rows() to move comma delimited answers into their own cell in the same column (not pivot_longer)


# I also wanted to be able to view unique responses (write-in responses) so I can group similar responses or remove them from the final chart... I started working on this with a count and if_else command but abandoned the code to let Charlie do it her way
# count(reasons_food_is_a_problem) %>% 
# mutate(unique_responses = if_else(n == 1, reasons_food_is_a_problem, ""))




# Export data as .rds to project data folder -------------------------------

write_rds(wagap_tidy_food, file = "data/wagap_tidy_food.rds")

write_rds(food_problems, file = "data/food_problems.rds")

write_rds(food_problems_categorized, file = "data/food_problems_categorized.rds")






