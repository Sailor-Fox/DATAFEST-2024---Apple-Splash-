library(tidyverse)
dataset <- rnorm(1000) %>% 
  as_tibble_col("data")
ggplot(dataset) +
  geom_histogram(aes(x=dataset, bin=10))