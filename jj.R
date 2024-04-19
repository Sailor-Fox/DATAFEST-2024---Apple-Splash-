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
  select(book, points_earned, points_possible, item_type, review_flag, chapter_number) %>% 
  mutate(item_type = if_else(item_type == "learnosity-activity", "learnosity", item_type)) %>% 
  group_by(book, review_flag, chapter_number) %>% 
  summarise(average_points = sum(points_earned, na.rm = TRUE)/sum(points_possible, na.rm = TRUE)) %>% 
  ggplot() +
    geom_line(aes(x = chapter_number, y = average_points, colour = review_flag)) +
    facet_wrap(~book) +
    theme_bw() +
    scale_x_continuous(breaks = seq(min(responses$chapter_number, na.rm = TRUE), max(responses$chapter_number, na.rm = TRUE), by = 1)) +
    labs(x = "Chapter Number", y = "Average points (%)")


# responses %>% 
#   select(item_type, prompt, response, points_possible, points_earned) %>% 
#   #filter(item_type == "code", !(prompt == response)) %>%
#   sample_n(100) %>% 
#   mutate(response = map2(response, prompt, ~str_replace_all(.x, .y, ""))) %>% 
#   view()
