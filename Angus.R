library(tidyverse)
# WORKSHOP STUFF

read_csv("workshop_materials/workshop_materials/oz_climate.csv")

responses <- read_csv("responses.csv", show_col_types = FALSE)
# radhika will tell us specific chapters to look at (bad or good ones) and then we can look at the questions within those specific chapters. Is it one big marker q bringing the EOC results down? Is it one specific class or instituion? distribution of the results (ideally we want little skew)
responses %>% 
  filter(book == "College / Advanced Statistics and Data Science (ABCD)" & chapter_number == 1) %>% 
  group_by(item_id, chapter, book) %>% 
  summarise(total_earned = sum(points_earned),
            total_possible = sum(points_possible)) %>% 
  pivot_longer(cols = 4:5, names_to = "type", values_to = "sum") %>% 
  ggplot(aes(x=item_id, y = )) +
    geom_smth()
  # potentially facet_wrap based on what we will analyse as contributors
