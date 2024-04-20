install.packages("tidyverse")
install.packages("janitor")
install.packages("palmerpenguins")
library(tidyverse)
library(janitor)
library(palmerpenguins)
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
  filter(lrn_type != "plaintext", !is.na(points_earned), !is.na(points_possible)) %>% 
  filter(chapter_number < 11) %>% 
  summarise(ave = sum(points_earned)/sum(points_possible))

responses %>% 
  filter(lrn_type != "plaintext", !is.na(points_earned), !is.na(points_possible)) %>% 
  group_by(chapter_number, section_number, review_flag) %>% 
  summarise(ave = sum(points_earned)/sum(points_possible)) %>% 
  mutate(parts = paste0(as.character(chapter_number), ".", as.character(section_number))) %>%
  ungroup() %>% 
  filter(ave <= 0.45) %>% 
  select(chapter_number, parts, ave, review_flag) %>%
  mutate(colour_flag = if_else(chapter_number > 13, TRUE, FALSE)) %>% 
  ggplot() +
  geom_col(aes(x = factor(reorder(parts, +ave)), y = ave, fill = colour_flag)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "Chapter & Section", y = "Average Mark (%)") +
  scale_fill_discrete(guide = "none") + 
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 36))


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
  filter(chapter_number == 2, section_number == 1) %>% 
  select(chapter, page) %>% 
  head(1)
responses %>% 
  filter(!is.na(str_extract(prompt, "university"))) %>% 
  select(prompt, response) %>% 
  view()
