install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)
items <- read.csv("items.csv")
setwd("~/DF24")
responses <- read.csv("responses.csv")



view(items)
filter(items, !is.na(dcl_sct))
items1 <- select(items, -c(dcl_pre_exercise_code, dcl_sample_code, dcl_solution, dcl_sct, dcl_hint))
view(items1)

question_id_filter_2 <- function(review, q_type) {
  output <- select(filter(items, review_flag == review, lrn_type == q_type, chapter_number == 2), lrn_question_reference)
  return(output)
}
question_id_filter_2(FALSE, "mcq")

mcq_response_grade_ABCD <- filter(responses, review_flag == FALSE, lrn_type == "clozeassociation", !is.na(points_earned), !is.na(points_possible), book == "College / Advanced Statistics and Data Science (ABCD)")
mcq_response_grade_ABC <- filter(responses, review_flag == FALSE, lrn_type == "clozeassociation", !is.na(points_earned), !is.na(points_possible), book == "College / Statistics and Data Science (ABC)")
mcq_marks_ABCD <- sum(mcq_response_grade_ABCD$points_earned) / sum(mcq_response_grade_ABCD$points_possible)
mcq_marks_ABC <- sum(mcq_response_grade_ABC$points_earned) / sum(mcq_response_grade_ABC$points_possible)

c(mcq_marks_ABCD, mcq_marks_ABC)

responses$book %>% unique()

responses %>%
  filter(item_type != "code") %>%
  select(book, points_earned, points_possible, item_type, review_flag, chapter_number) %>% 
  mutate(item_type = if_else(item_type == "learnosity-activity", "learnosity", item_type)) %>% 
  group_by(book, item_type, review_flag, chapter_number) %>% 
  summarise(average_points = sum(points_earned, na.rm = TRUE)/sum(points_possible, na.rm = TRUE)) %>% 
  ggplot() +
    geom_line(aes(x = chapter_number, y = average_points, colour = review_flag)) +
    facet_wrap(~book) +
    theme_classic() +
    scale_x_continuous(breaks = seq(min(responses$chapter_number, na.rm = TRUE), max(responses$chapter_number, na.rm = TRUE), by = 1)) +
    labs(x = "Chapter Number", y = "Average points (%)")
# choice matrix is bad
responses %>%
  select(points_earned, points_possible, lrn_type, chapter_number) %>% 
  group_by(lrn_type) %>% 
  filter(!is.na(points_earned)) %>% 
  summarise(average_points = sum(points_earned, na.rm = TRUE)/sum(points_possible, na.rm = TRUE)) %>%
  mutate(lrn_type = if_else(is.na(lrn_type), "Code", lrn_type)) %>% 
  ggplot() +
    geom_col(aes(x = reorder(lrn_type, +average_points), y = average_points, fill = reorder(lrn_type, +average_points))) + 
    labs(x="Question type", y="Average Points (%)") + 
    scale_x_discrete(labels = c("Choice\nMatrix", "Short\nText", "Sort\nList", "Image\nclose\nAssociation", "R Code", "Multi\nChoice\nQs", "Close\nAssoc.", "Formulae", "Association", "Plain\nText")) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_fill_manual(values=c("red","#636363","red","red","red","#636363","#636363","red","#636363","cyan"), guide = "none") +
    theme(axis.text = element_text(size = 13))
# other types
responses %>%
  select(prompt, response, points_earned, points_possible, lrn_type) %>%
  filter(lrn_type == "plaintext") %>% 
  filter(!is.na(points_possible)) %>% 
  #filter(!is.na(str_extract(response, "null"))) %>% 
  #filter(is.na(response)) %>% 
  filter(!is.na(points_earned)) %>%
  filter(points_earned == 1) %>% 
  view()

q_type <- c("choice matrix", "sort list", "image association", "code", "Formula")
null_percent <- c(443/6667 *100, 192/1311 *100, 70/2384 *100, 13232/383174 *100, 5/389 *100)
null_answer_percent <- as_tibble(q_type, null_percent)
null_answer_percent %>% 
  ggplot() +
  geom_col(aes(x = as_factor(q_type), y = null_percent, fill = q_type)) + 
  labs(x = "Question Type", y = "Empty answers (%)") + 
  scale_fill_manual(values=c("red","red","red","red","red"), guide = "none")
# responses %>% 
#   select(item_type, prompt, response, points_possible, points_earned) %>% 
#   #filter(item_type == "code", !(prompt == response)) %>%
#   sample_n(100) %>% 
#   mutate(response = map2(response, prompt, ~str_replace_all(.x, .y, ""))) %>% 
#   view()





