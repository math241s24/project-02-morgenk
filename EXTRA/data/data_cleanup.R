library(tidyverse)
library(dplyr)
library(stringr)
appa <- appa::appa

character_count <- appa %>%
  group_by(chapter, character) %>% 
  summarise(character_count = n(), id, book, book_num, chapter, chapter_num, writer, director, imdb_rating) %>%
  filter(character != "Scene Description")

eps <- appa %>%
  group_by(chapter) %>%
  summarise() %>%
  mutate(ep = row_number())

atla <- left_join(character_count, eps, by = "chapter")

write_csv(atla, "EXTRA/data/atla.csv")



