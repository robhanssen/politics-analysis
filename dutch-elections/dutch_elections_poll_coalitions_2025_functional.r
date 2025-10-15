#
# R program to determine the seats distribution in
# the Dutch Tweede Kamer after the 2017 election
#
library(tidyverse)
library(patchwork)
library(rvest)

theme_set(theme_light())

# define constants
MIN_SEATS <- 75 # nolint
MAX_PARTIES <- 5 # nolint


# Load the coalition analysis functions
source("dutch-elections/coalition-analysis.r")

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

majoritycoalitions <- generate_coalitions(election)


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
partycount <- nrow(election)

party_matrix <- matrix(0,
    nrow = partycount,
    ncol = partycount,
    dimnames = list(election$parties, election$parties)
)

# Split all party lists into a list of vectors
party_lists <- strsplit(majoritycoalitions$partylist, ", ")

# Generate all unique pairs of parties from each coalition
party_pairs <- lapply(party_lists, function(parties) {
    combn(parties, 2, simplify = FALSE)
})

# Flatten the list of lists into a single list of pairs
all_pairs <- unlist(party_pairs, recursive = FALSE)

# Update the party_matrix using Reduce
party_matrix <- Reduce(function(mat, pair) {
    p1 <- pair[1]
    p2 <- pair[2]
    mat[p1, p2] <- mat[p1, p2] + 1
    mat
}, all_pairs, init = party_matrix)

party_df <- as.data.frame(as.table(party_matrix)) %>%
    filter(Freq > 0) %>%
    mutate(Freq = round(Freq / nrow(majoritycoalitions), 3))

party_df$Var1 <- factor(party_df$Var1, levels = sort(unique(party_df$Var1), FALSE))
party_df$Var2 <- factor(party_df$Var2, levels = sort(unique(party_df$Var2), TRUE))

heatmap_g <-
    ggplot(party_df, aes(x = Var1, y = Var2, fill = Freq, color = Freq >= 1 / 2)) +
    geom_tile(color = "white", show.legend = FALSE) +
    scale_fill_gradient2(low = "white", mid = "gray90", high = "steelblue") +
    scale_color_manual(values = c("TRUE" = "gray99", "FALSE" = "gray10")) +
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

ggsave("dutch-elections/dutch_elections_poll_coalitions_2025.png", width = 10, height = 6)
