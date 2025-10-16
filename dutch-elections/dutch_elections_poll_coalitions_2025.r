#
# R program to determine the seats distribution in
# the Dutch Tweede Kamer after the 2017 election
#
library(tidyverse)
library(patchwork)
library(rvest)

theme_set(theme_light())

MIN_SEATS <- 75 # nolint
MAX_PARTIES <- 5 # nolint

url <-
    "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2025_Dutch_general_election"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    slice(-1)

pollco <- data_raw[1, 1]
polldate <- data_raw[1, 2]
pnames <- names(data_raw)[4:ncol(data_raw)] %>% str_remove(., "\\[.?\\]")
pnames <- pnames[1:(length(pnames) - 2)]


election <-
    data_raw %>%
    select(4:(length(pnames) + 3)) %>%
    set_names(pnames) %>%
    slice(1) %>%
    mutate(across(everything(), as.integer)) %>%
    pivot_longer(everything(),
        names_to = "parties",
        values_to = "totalseats"
    ) %>%
    filter(totalseats > 0) %>%
    arrange(desc(totalseats))

partycount <- nrow(election)

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
    filter(seatcount >= MIN_SEATS) %>%
    filter(numparties <= MAX_PARTIES) %>%
    arrange(numparties, -seatcount)

# minoritycoalitions <- coalitions %>%
#     filter(seatcount >= 70) %>%
#     filter(seatcount <= 75) %>%
#     filter(numparties <= 5) %>%
#     arrange(numparties, -seatcount)


maj_party_list <- majoritycoalitions %>%
    pull(partylist) %>%
    lapply(., \(x) strsplit(x, ", ") %>% unlist())

x <- table(unlist(maj_party_list)) / length(maj_party_list)

party_coal_g <- tibble(name = names(x), pct = as.numeric(x)) %>%
    ggplot(
        aes(y = reorder(name, pct), x = pct)
    ) +
    geom_col(fill = "steelblue") +
    labs(
        title = "Waarschijnlijkheid dat een partij in de coalitie zit",
        y = NULL,
        x = NULL,
    ) +
    scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1)
    ) +
    geom_text(aes(x = 0, label = scales::percent(pct, accuracy = 1)),
        hjust = -0.25, color = "white"
    )


# make a heat map from the majoritycoalitions table showing which parties
# are in coalitions with which other parties
party_matrix <- matrix(0,
    nrow = partycount,
    ncol = partycount,
    dimnames = list(election$parties, election$parties)
)

for (i in 1:nrow(majoritycoalitions)) { # nolint
    parties <- strsplit(majoritycoalitions$partylist[i], ", ")[[1]]
    for (p1 in parties) {
        for (p2 in parties) {
            if (p1 != p2) {
                party_matrix[p1, p2] <- party_matrix[p1, p2] + 1
            }
        }
    }
}

party_df <- as.data.frame(as.table(party_matrix)) %>%
    filter(Freq > 0) %>%
    mutate(Freq = round(Freq / nrow(majoritycoalitions), 3))

party_df$Var1 <- factor(party_df$Var1, levels = sort(unique(party_df$Var1), FALSE))
party_df$Var2 <- factor(party_df$Var2, levels = sort(unique(party_df$Var2), TRUE))

heatmap_g <-
    ggplot(party_df, aes(x = Var1, y = Var2, fill = Freq, color = Freq >= 1 / 2)) +
    geom_tile(color = "white", show.legend = FALSE) +
    scale_fill_gradient2(low = "white", mid = "gray90", high = "steelblue") +
    scale_color_manual(values = c("TRUE" = "gray90", "FALSE" = "gray10")) +
    geom_text(
        aes(label = scales::percent(Freq, accuracy = 2)),
        size = 2.5
    ) +
    labs(
        title = "Waarschijnlijkheid dat twee partijen samen in een coalitie zitten",
        x = NULL,
        y = NULL,
        fill = NULL,
        legend = NULL
    ) +
    coord_fixed() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
    )

party_coal_g + heatmap_g + plot_layout(ncol = 2) +
    plot_annotation(
        title = "TK Verkiezingen 2025: Coalitie Analyse",
        caption = glue::glue(
            "Data van Wikipedia; realistische coalities hebben {MIN_SEATS} ",
            "of meer zetels and {MAX_PARTIES} of minder partijen ({nrow(majoritycoalitions)} mogelijkheden)",
            "\nPeiling op {polldate} door {pollco}"
        )
    ) & theme(
    plot.title = element_text(hjust = 0, size = 12.5),
    plot.caption = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot"
)

ggsave("dutch-elections/dutch_elections_poll_coalitions_2025.png", width = 12, height = 6)


coalition_subset <- function(coal, party) {
    coal %>%
        filter(!str_detect(partylist, party)) %>%
        arrange(numparties, -seatcount) %>%
        head(5)
}

coalition_subset(majoritycoalitions, "PVV") 
coalition_subset(majoritycoalitions, "GL/PvdA")
