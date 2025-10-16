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

coalitions_time <- future_map(election$election[seq_len(TIME_SLICE)], generate_coalitions)
names(coalitions_time) <- election$d[seq_len(TIME_SLICE)]

coal_probs <- map(coalitions_time, extract_coalition)

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
        title = "Waarschijnlijkheid dat een party in een coalitie zit",
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

ggsave("dutch-elections/dutch_elections_poll_coalitions_2025_time.png",
    width = 10, height = 6, plot = all_probs_g
)