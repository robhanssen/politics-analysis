#
# R program to determine the seats distribution in
# the Dutch Tweede Kamer after the 2017 election
#
library(tidyverse)
library(rvest)

theme_set(theme_light())

url <-
  "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2023_Dutch_general_election" # nolint

data_raw <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill = TRUE)

names(data_raw) <- data_raw[1, ]

polldate <- dmy(
  str_remove(data_raw[3, "Fieldwork date"], "^.*–")
)

pollco <-
  data_raw[3, "Polling firm"]

election <-
  data_raw %>%
  janitor::clean_names() %>%
  slice(3) %>%
  select(-polling_firm, -lead, -ref, -fieldwork_date, -samplesize) %>%
  pivot_longer(everything(),
    names_to = "parties",
    values_to = "totalseats"
  ) %>%
  mutate(
    parties = str_remove_all(parties, "\\_"),
    parties = ifelse(parties == "pvda", "PVDA/GL", toupper(parties)),
    parties = str_replace_all(parties, "PVD", "Pvd"),
    totalseats = as.numeric(totalseats),
  ) %>%
  filter(totalseats > 0 & parties != "GL") %>%
  arrange(-totalseats)

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

majoritycoalitions %>%
  slice_head(n = 15) %>%
  select(Coalitions = "partylist", "Seat Count" = "seatcount") %>%
  knitr::kable(
    caption =
      glue::glue(
        "Possible majority coalitions based on {pollco}",
        " poll on {format(polldate, format = '%b %d, %Y')}"
      )
  )
