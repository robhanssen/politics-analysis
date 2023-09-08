#
# R program to determine the seats distribution in
# the Dutch Tweede Kamer after the 2017 election
#
library(tidyverse)
theme_set(theme_light())

# number of seats in the Tweede Kamer
totalseats <- 150

# election result
election <- read_csv("sources/2021votecount.csv")
# total number of valid votes casts
totalvotes <- sum(election$votes)

kiesdeler <- totalvotes / totalseats

# initial distribution of seats,
#
# Elimination of parties that do not have seats out of
# the residual seats calculation
#
# Parties that did not win seats in the original calculation
# have no rights to residual seats according to law
#
election <- election %>%
  mutate(
    seats = floor(votes / totalvotes * totalseats),
    extraseats = 0,
    totalseats = seats
  ) %>%
  filter(seats != 0)

#
# residual seat algorithm:
# 1. divide party votes by (current # of party seats +1)
# 2. the party with the highest number wins an additional seat
# 3. repeat with new number of seats per party until all
#    residual seats are given away
#

givenseats <- sum(election$seats)

while (givenseats < totalseats) {
  election <- election %>% mutate(residquot = votes / (seats + extraseats + 1))
  maxresid <- max(election$residquot)

  election <- election %>%
    mutate(extraseats = case_when(
      residquot == maxresid ~ extraseats + 1,
      TRUE ~ extraseats
    ))

  election$totalseats <- election$seats + election$extraseats
  givenseats <- sum(election$totalseats)
}

# election <- 
#   read_csv("sources/latestpoll2023.csv",
#       comment = "#",
#       col_types = "cii")

partycount <- election %>%
  count() %>%
  pull(n)

decomp_vector <- function(vector) {
  v <- strsplit(vector, "")[[1]]
  as.numeric(v)
}

partyseat_gen <- function(bitlst, elec) {
  bitvec <- unlist(bitlst)
  sum(bitvec * elec$totalseats)
}

partylist_gen <- function(bitlst, elec) {
  bitvec <- unlist(bitlst)
  l <- elec$parties[which(bitvec == 1)]
  paste(l, collapse = ", ")
}

num_length <- 2^partycount - 1

coalitions <-
  tibble(
    numbers = 1:num_length,
    bitcode = R.utils::intToBin(numbers)
  ) %>%
  mutate(
    bitlist = map(
      numbers,
      ~ decomp_vector(bitcode[.x])
    ),
    seatcount = map_dbl(
      numbers,
      ~ partyseat_gen(bitlist[.x], election)
    ),
    partylist = map_chr(
      numbers,
      ~ partylist_gen(bitlist[.x], election)
    ),
    numparties = map_dbl(
      numbers,
      ~ sum(unlist(bitlist[.x]))
    )
  ) %>%
  select(partylist, numparties, seatcount)

majoritycoalitions <- coalitions %>%
  filter(seatcount >= 75) %>%
  filter(numparties <= 5) %>%
  arrange(numparties, -seatcount)

minoritycoalitions <- coalitions %>%
  filter(seatcount >= 70) %>%
  filter(seatcount <= 75) %>%
  filter(numparties <= 5) %>%
  arrange(numparties, -seatcount)

View(majoritycoalitions)