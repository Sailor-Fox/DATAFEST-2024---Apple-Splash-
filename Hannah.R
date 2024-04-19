checkpoints_eoc <- read_csv("checkpoints_eoc.csv")
checkpoints_pulse <- read_csv("checkpoints_pulse.csv")
items <- read_csv("items.csv")
media_views <- read_csv("media_views.csv")
page_views <- read_csv("page_views.csv")
responses <- read_csv("responses.csv")

#adding new column to checkpoints_pulse
checkpoints_pulse_previous_chapter <- checkpoints_pulse %>% 
  mutate(Previous_chapter=chapter_number-1)
