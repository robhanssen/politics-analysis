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
  inner_join(election, by = join_by(plist == parties)) %>%
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
  inner_join(election, by = join_by(plist == parties)) %>%
  unite(pplist, c("plist", "totalseats"), sep = " (", ) %>%
  mutate(pplist = paste0(pplist, ")")) %>%
  group_by(rowname) %>%
  nest(nested_party = pplist) %>%
  ungroup() %>%
  mutate(ppplist = map_chr(
    nested_party,
    ~ paste(unlist(.x), collapse = ", ")
  )) %>%
  select(partylist, ppplist, numparties, seatcount) %>%
  select(Coalitions = "partylist", "Seat Count" = "seatcount", ppplist) %>%
  mutate(
    Seats = str_trim(
      str_remove_all(ppplist, "X50|D66|\\/|,|\\(|\\)|[A-Za-z]")
    ),
    Seats = str_replace_all(Seats, "  ", ", ")
  ) %>%
  select(-ppplist) %>%
  knitr::kable(
    caption =
      glue::glue(
        "Possible majority coalitions based on {pollco}",
        " poll on {format(polldate, format = '%b %d, %Y')}"
      )
  )
