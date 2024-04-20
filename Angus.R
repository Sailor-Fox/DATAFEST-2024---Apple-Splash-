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


# LOOKING AT HOW THE BOTTOM PPL AFTER CHAPTER ONE PERFORMS OVER TIME

checkpoints_eoc <- read_csv("checkpoints_eoc.csv")

sorted_data <- checkpoints_eoc %>% 
  filter(chapter_number==1) %>% 
  arrange(EOC)
bottom_index <- round(0.01 * nrow(sorted_data))
top_index <- round(0.99 * nrow(sorted_data))
bottom_decile <- sorted_data %>% 
  slice(1:bottom_index) %>%
  pull(student_id)
median_decile <- sorted_data %>% 
  slice(round(0.495 * nrow(sorted_data)):round(0.505 * nrow(sorted_data))) %>%
  pull(student_id)
top_decile <- sorted_data %>% 
  slice((top_index + 1):nrow(sorted_data)) %>%
  pull(student_id)
checkpoints_eoc %>% 
  mutate(decile = if_else(student_id %in% bottom_decile, "bottom", if_else(student_id %in% top_decile, "top", if_else(student_id %in% median_decile, "med", NA)))) %>% 
  select(chapter_number, EOC, decile) %>% 
  filter(!is.na(decile)) %>% 
  group_by(chapter_number, decile) %>% 
  summarise(average_eoc = mean(EOC, na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x=chapter_number, y=average_eoc, colour=decile))

