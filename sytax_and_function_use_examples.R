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