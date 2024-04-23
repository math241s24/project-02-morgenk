library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(tidyverse)

atla <- read_csv("~/Desktop/Data/project-02-morgenk/EXTRA/data/atla.csv")

summary_stats <- atla %>%
  group_by(director) %>%
  summarise(
    mean_imdb = mean(IMDB, na.rm = TRUE),
    median_imdb = median(IMDB, na.rm = TRUE),
    min_imdb = min(IMDB, na.rm = TRUE),
    max_imdb = max(IMDB, na.rm = TRUE),
    sd_imdb = sd(IMDB, na.rm = TRUE)
  )

print(summary_stats)
