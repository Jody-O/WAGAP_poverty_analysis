# vector vs. tibble exploration code:
# tibbles are tidyverse tables that show up nicely in the console
library("tidyverse")
library(palmerpenguins)

# find out what kind of data.frame you have:
class(iris)

starwars

class(penguins)

starwars %>% 
  group_by(homeworld)

iris %>% 
  group_by(Species)

penguins


#select() returns selected columns in a tibble
penguins %>% 
  select(flipper_length_mm)

# pull() returns a single value from a vector.  This can be used to pull a value into a knitted .Rmd file text section.
penguins %>% 
  pull(flipper_length_mm) %>% 
  class()

penguins %>% 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE)) %>% 
  pull(mean_flipper)


# use of ~ to write function (exploration code)
library(tidyverse)

starwars %>% 
  View()

# function() and ~ do the same thing (allow you to write a function formula).  Used when applying a function across multiple columns.
# ~ operates on the value inside EACH cell.  The "." in .x refers to the current value in a cell.
starwars %>% 
  mutate(across(where(is.character), ~na_if(.x, "unknown"))) %>% 
  View()

starwars %>% 
  mutate(across(where(is.character), function(x) na_if(x, "unknown")))


starwars %>% 
  mutate(height = height / 100)

#use ?"operator" for help on sytnax/operators:
?"~"

# c creates a named vector, and allows you to combine values into a vector. vectors have a depth of 1.  Lists can create more complex data: 

library("tidyverse")

starwars %>% 
  rename(c("character_name" = "name",
           "character_height" = "height"))

c("character_name" = "name",
  "character_height" = "height")

c(1, "two", 3)

charlies_vectors <- c("name_for_the_first_part_of_the_vector" = 22, "second_name" = 33)

names(charlies_vectors)

charlies_list <- list("name_for_thing_1" = c(1, 2, 3), 
                      "name_for_thing_2" = c("two", "three", "four"))

charlies_list

charlies_list[2]


# regex refers to regular expression (like white space) - use it to search for ways to deal with human expressions/ language.  "^, " looks for strings that start with ",".  ^ also means "not". $ means end of string, * means anything.  "[:alnum:]" looks for any alphanumeric (not a comma or white space, etc)

# Complicated regex example
food_problems %>% 
  select(reasons_food_is_a_problem) %>% 
  mutate(food_problem_lack_of_transportation = str_detect(reasons_food_is_a_problem, "Lack of transportation to grocery stores or markets"),
         food_problem_not_enough_alternatives = str_detect(reasons_food_is_a_problem, "Not enough alternative food sources available"),
         food_problem_reduced_access = str_detect(reasons_food_is_a_problem, "Reduced access to free and reduced school meals because of COVID-19 school closures"),
         food_problems_not_enough_food = str_detect(reasons_food_is_a_problem, "Not enough income to purchase food")) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Lack of transportation to grocery stores or markets"),
         reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Not enough alternative food sources available"),
         reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Reduced access to free and reduced school meals because of COVID-19 school closures"),
         reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "Not enough income to purchase food")) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "^,")) %>% 
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, ",[^[:alnum:]]*$")) %>%
  mutate(reasons_food_is_a_problem = str_remove(reasons_food_is_a_problem, "^ ,[^[:alnum:]]*")) %>%
  mutate(reasons_food_is_a_problem = str_trim(reasons_food_is_a_problem),
         reasons_food_is_a_problem = ifelse(reasons_food_is_a_problem == "", NA, reasons_food_is_a_problem)) %>% 
  View()

# tibble vs. tribble:  tribble allows you to write data by row

tibble(
  a = c(1, 2, 4),
  b = c(6, 7, 8)
)

tribble(
  ~name, ~age,
  "Charlie", 33,
  "Wren", 34,
  "Jeremy", 56
)

# interactive charts: http://www.htmlwidgets.org/showcase_highcharts.html

# fancy TABLES:  knitr::kable  or  library(DT)  or  library(gt)