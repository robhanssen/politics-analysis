#
# R program to determine the seats distribution in
# the Dutch Tweede Kamer after the 2017 election
#
library(tidyverse)
library(patchwork)
library(furrr)
library(rvest)

theme_set(theme_light())

plan(multisession)

# define constants
MIN_SEATS <- 75 # nolint
MAX_PARTIES <- 5 # nolint
TIME_SLICE <- 10 # nolint; highly memory intensive!

# Load the coalition analysis functions
source("dutch-elections/coalition-analysis.r")

url <-
    "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2025_Dutch_general_election"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    slice(-1) %>%
    unite("date", 1:2, sep = "\n")

pollco <- str_split(data_raw[1, 1], "\n")[[1]][1]
polldate <- str_split(data_raw[1, 1], "\n")[[1]][2]
pnames <- names(data_raw)[3:ncol(data_raw)] %>% str_remove(., "\\[.?\\]")
pnames <- pnames[1:(length(pnames) - 2)]


election <-
    data_raw %>%
    select(1, 3:(length(pnames) + 2)) %>%
    set_names(c("d", pnames)) %>%
    mutate(across(!d, as.integer)) %>%
    drop_na() %>%
    pivot_longer(!d,
        names_to = "parties",
        values_to = "totalseats"
    ) %>%
    filter(totalseats > 0) %>%
    nest(election = !d)

election_list <- bind_rows(election$election) %>%
    reframe(
        totalseats = sum(totalseats),
        .by = parties
    ) %>%
    arrange(desc(totalseats))

levs <- election_list$parties


coalitions_over_time <- future_map(election$election[seq_len(TIME_SLICE)], generate_coalitions)
names(coalitions_over_time) <- election$d[seq_len(TIME_SLICE)]

coal_probs <- map(coalitions_over_time, extract_coalition)

all_probs <-
    map_df(coal_probs, \(x) x, .id = "date") %>%
    pivot_longer(!date, names_to = "party", values_to = "prob") %>%
    mutate(date = factor(date, levels = rev(data_raw$date)))

all_probs_g <-
    all_probs %>%
    ggplot(aes(x = date, y = prob, color = party, group = party)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggrepel::geom_text_repel(
        data = all_probs %>% filter(date == first(all_probs)$date),
        aes(x = TIME_SLICE + 0.5, y = prob, label = party), hjust = 0
    ) +
    labs(
        title = "Waarschijnlijkheid dat een partij in een coalitie zit over de tijd",
        y = NULL,
        x = NULL,
    ) +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0, size = 12.5),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.caption.position = "plot"
    )

# ggsave("dutch-elections/dutch_elections_poll_coalitions_2025_time.png",
#     width = 10, height = 6, plot = all_probs_g
# )


cross_corr_coalition <- function(coal_option) {
    party_lists <- strsplit(coal_option$partylist, ", ")

    # Generate all unique pairs of parties from each coalition
    party_pairs <- lapply(party_lists, function(parties) {
        combn(parties, 2, simplify = FALSE)
    })

    # Flatten the list of lists into a single list of pairs
    all_pairs <- unlist(party_pairs, recursive = FALSE)

    pair_df <- tibble(
        Var1 = purrr::map_chr(all_pairs, 1),
        Var2 = purrr::map_chr(all_pairs, 2)
    ) %>%
        count(Var2, Var1, name = "Freq") %>%
        mutate(Freq = round(Freq / nrow(coal_option), 3))

    bind_rows(
        pair_df,
        pair_df %>% rename(Var1 = Var2, Var2 = Var1)
    ) %>%
        tidyr::complete(Var1 = election_list$parties, Var2 = election_list$parties, fill = list(Freq = 0)) %>%
        filter(Var1 != Var2)
}

coalition_probability <- function(coal_option) {
    coal_option |>
        rownames_to_column(var = "coalition_id") |>
        separate_longer_delim(partylist, ", ") |>
        count(partylist, sort = TRUE) |>
        mutate(prob = round(n / nrow(coal_option), 3))
}

last_poll <- coalitions_over_time[[1]]

party_coal_g <-
    coalition_probability(last_poll) %>%
    ggplot(
        aes(y = reorder(partylist, prob), x = prob)
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
    geom_text(aes(x = 0, label = scales::percent(prob, accuracy = 1)),
        hjust = -0.25, color = "white"
    )

party_heat_g <-
    cross_corr_coalition(last_poll) %>%
    mutate(
        Var1 = factor(Var1, levels = levs, ordered = TRUE),
        Var2 = factor(Var2, levels = rev(levs), ordered = TRUE)
    ) %>%
    ggplot(aes(x = Var1, y = Var2, fill = Freq, color = Freq >= 1 / 2)) +
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

p <-
    (party_coal_g + party_heat_g + plot_layout(width = c(1, 1))) / all_probs_g +
        plot_layout(height = c(4 / 3, 1)) &
        theme(
            plot.title = element_text(hjust = 0, size = 12.5),
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            plot.caption.position = "plot"
        )


ggsave("dutch-elections/dutch_elections_poll_coalitions_2025.png", width = 12, height = 10, plot = p)
