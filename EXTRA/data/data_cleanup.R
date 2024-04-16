library(tidyverse)
library(dplyr)
library(stringr)
appa <- appa::appa

character_count <- appa %>%
  group_by(chapter, character) %>%
  mutate(character = str_split(character, " and ")) %>%
  unnest(character) %>%
  summarise(Lines = n(), id, Book = book, book_num, chapter, chapter_num, writer, director, imdb_rating) %>%
  filter(character != "Scene Description")  

eps <- appa %>%
  group_by(chapter) %>%
  summarise() %>%
  mutate(Episode = row_number())

atla <- left_join(character_count, eps, by = "chapter") %>%
  arrange(desc(character_count))
 
write_csv(atla, "EXTRA/data/atla.csv")