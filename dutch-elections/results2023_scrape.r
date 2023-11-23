
library(tidyverse)
library(rvest)


url <-
  "https://en.wikipedia.org/wiki/2023_Dutch_general_election" # nolint

data_raw <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]') %>%
  html_table(fill = TRUE) 

names(data_raw) <- c("party1", "party", "votes","perc","seats","bs")

cleanup <-
    data_raw %>%
    slice(-1) %>%
    select(-party1) %>%
    slice(-c(27L,28L,29L)) %>%
    mutate(votes = as.numeric((str_remove_all(votes, ",")))) %>%
    select(party, votes)

view(cleanup)