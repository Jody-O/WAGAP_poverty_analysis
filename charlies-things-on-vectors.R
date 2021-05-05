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

