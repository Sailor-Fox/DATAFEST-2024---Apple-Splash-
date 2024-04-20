library(tidyverse)


responses <- read_csv("responses.csv", show_col_types = FALSE)
# radhika will tell us specific chapters to look at (bad or good ones) and then we can look at the questions within those specific chapters. Is it one big marker q bringing the EOC results down? Is it one specific class or instituion? distribution of the results (ideally we want little skew)
responses %>%
  filter(chapter_number == 1) %>%
  group_by(item_id) %>%
  summarise(average_points = sum(points_earned) / sum(points_possible)) %>%
  ungroup() %>%
  left_join(responses, by = "item_id") %>%
  select(average_points, item_id, item_type) %>%
  ggplot(aes(x = item_id, y = average_points, colour = item_type)) +
  geom_point()

