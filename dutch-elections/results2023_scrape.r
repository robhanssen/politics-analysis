library(tidyverse)
library(rvest)


url <-
  "https://en.wikipedia.org/wiki/2023_Dutch_general_election" # nolint

data_raw <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') %>%
  html_table(fill = TRUE)

names(data_raw) <- c("party1", "party", "votes", "perc", "seats", "bs")

cleanup <-
  data_raw %>%
  slice(-1) %>%
  select(-party1) %>%
  slice(-(27:50)) %>%
  mutate(votes = as.numeric((str_remove_all(votes, ",")))) %>%
  select(party, votes)

election <- cleanup
totalseats <- 150
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
  l <- elec$party[which(bitvec == 1)]
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


coalitions %>%
  filter(seatcount >= 75) %>%
  arrange(numparties, -seatcount) %>%
  slice_head(n = 15) %>%
  rownames_to_column() %>%
  mutate(
    plist = map(partylist, ~ unlist(str_split(.x, ", "))),
    plen = map_dbl(plist, length)
  ) %>%
  unnest_longer(plist) %>%
  inner_join(election, by = join_by(plist == party)) %>%
  unite(pplist, c("plist", "totalseats"), sep = " (", ) %>%
  mutate(pplist = paste0(pplist, ")")) %>%
  group_by(rowname) %>%
  nest(nested_party = pplist) %>%
  ungroup() %>%
  mutate(ppplist = map_chr(
    nested_party,
    ~ paste(unlist(.x), collapse = ", ")
  )) %>%
  select(ppplist, numparties, seatcount) %>%
  select(Coalitions = "ppplist", "Seat Count" = "seatcount") %>%
  knitr::kable(
    caption =
      glue::glue(
        "Possible majority coalitions based on {pollco}",
        " poll on {format(polldate, format = '%b %d, %Y')}"
      )
  )
