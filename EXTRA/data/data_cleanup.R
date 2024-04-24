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
  group_by(character) %>%
  mutate(sum(Lines)) %>%
  mutate("IMDB" = imdb_rating) %>%
  arrange(desc(Lines))
 
write_csv(atla, "~/Desktop/Data/project-02-morgenk/EXTRA/data/atla.csv")
write_csv(atla, "~/Desktop/Data/project-02-morgenk/EXTRA/shinyapp/atla.csv")
