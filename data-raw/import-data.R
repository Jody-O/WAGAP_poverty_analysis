# week 7 Rin3 project assignment - data tidying

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

# QUESTIONS: is there an easier way to change variable names?
# is there a better way to convert n/a and other missing data to 'NA'?

wagap_tidy_food[wagap_tidy_food == "n/a"] <- NA


# Delete repeated entries (by timestamp) ----------------------------------



# clean and group race/ethnicity variable  -----------------------------
wagap_tidy_food <- wagap_tidy_food %>% 
  drop_na() %>% 
  mutate(race_ethnicity = case_when(race_ethnicity %in% c("Pirate", "Blue", "all", "American", "Human", "Jewish", "Russian", "Russian Jewish Immigrant") ~ "other",
                                         race_ethnicity %in% c("Mixed", "mixed race", "Hispanic + Native American", "Mixed/Donít know", "Mixed/Don’t know", "Northern Norwegian Eskimo") ~ "Mixed race", TRUE ~ race_ethnicity))


 race_ethnicity_proportions <- wagap_tidy_food %>% 
  count(race_ethnicity) %>% 
  mutate(pct_of_total_race_ethnicity = n/sum(n))



# Group gender responses --------------------------------------------------

  
all_gender_responses <- wagap_tidy_food %>% 
  count(gender)

wagap_tidy_food <- wagap_tidy_food %>% 
  mutate(gender = case_when(gender %in% c("gender is fluid according to the whim of the day", "Transgender", "Male & Female") ~ "Gender non-conforming", TRUE ~ gender)) %>% 
  mutate(gender = replace_na(gender, "Prefer not to answer"))


# Separate responses to "reasons food is a problem?" ----------------------------

food_problems <- wagap_tidy_food %>% 
  select(timestamp, reasons_food_is_a_problem, race_ethnicity) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "(pantries, food bank, gleaning, etc.)")) %>% 
  
# QUESTION:  how can I remove parentheses-bracket character strings?? This does not work:  mutate(reasons_food_is_a_problem = str_replace(reasons_food_is_a_problem, " ()", "")) %>% 
  
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, ", n/a")) %>% 
  separate(reasons_food_is_a_problem, 
           sep = ",",
           into = c("prob1", "prob2", "prob3", "prob4", "prob5", "prob6", "prob7", "prob8", "prob9"))
# this doesn't work:  pivot_longer((cols = prob1, prob2, prob3, prob4, prob5, prob6, prob7, prob8, prob9), values_to = "reasons_food_is_a_problem")

# QUESTION: how can I combine all responses from reasons_food_is_a_problem into one column?  The survey allowed each respondent to select more than one 'reason', and the resulting Excel file kept all selected responses in one column, separated by commas.  I want to create a chart showing how many times each problem type was selected, so I am trying to separate all responses, then pivot so all responses ('reasons') end up as individual values all in the same variable column.

# I also want to be able to view unique responses (write-in responses) so I can group similar responses or remove them from the final chart... I started working on this with a count and if_else command but I need to sort out the question above first...
# count(reasons_food_is_a_problem) %>% 
# mutate(unique_responses = if_else(n == 1, reasons_food_is_a_problem, ""))




# Export data as .rds to project data folder -------------------------------

write_rds(wagap_tidy_food, file = "data/wagap_tidy_food.rds")

write_rds(food_problems, file = "data/food_problems.rds")




