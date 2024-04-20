install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)
items <- read.csv("items.csv")
setwd("~/DF24")
responses <- read.csv("responses.csv")

responses %>% 
  filter(lrn_type != "plaintext", !is.na(points_earned), !is.na(points_possible)) %>% 
  group_by(chapter_number, section_number, review_flag) %>% 
  summarise(ave = sum(points_earned)/sum(points_possible)) %>% 
  mutate(parts = paste0(as.character(chapter_number), ".", as.character(section_number))) %>%
  ungroup() %>% 
  select(chapter_number, parts, ave, review_flag) %>%
  ggplot() +
    geom_col(aes(x = as_factor(parts), y = ave, fill = review_flag)) +
    facet_wrap(~chapter_number, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1))


responses %>%
  select(prompt, response, points_earned, points_possible, lrn_type) %>%
  filter(lrn_type == "plaintext") %>% 
  filter(!is.na(points_possible)) %>% 
  #filter(!is.na(str_extract(response, "null"))) %>% 
  #filter(is.na(response)) %>% 
  filter(!is.na(points_earned)) %>%
  filter(points_earned == 1) %>% 
  view()

#find the chapters they struggled with
responses %>% 
  filter(chapter_number == 14, section_number == 11) %>% 
  select(chapter, page) %>% 
  head(1)

