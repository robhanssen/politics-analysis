#
# R program to determine the seats distribution in
# the Dutch Tweede Kamer after the 2017 election
#
library(tidyverse)
library(patchwork)
library(rvest)

theme_set(theme_light())

url <-
    "https://nl.wikipedia.org/wiki/Tweede_Kamerverkiezingen_2025/Peilingen" # nolint

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE)

election <-
    data_raw %>%
    janitor::clean_names() %>%
    select(pvv:overig) %>%
    mutate(across(everything(), as.numeric)) %>%
    slice(1) %>%
    pivot_longer(everything(),
        names_to = "parties",
        values_to = "totalseats"
    ) %>%
    filter(totalseats > 0) %>%
    arrange(desc(totalseats))

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


maj_party_list <- majoritycoalitions %>%
    pull(partylist) %>%
    lapply(., \(x) strsplit(x, ", ") %>% unlist())


x <- table(unlist(maj_party_list)) / length(maj_party_list)

party_coal_g <- tibble(name = names(x), pct = as.numeric(x)) %>%
    mutate(
        name = toupper(name),
        name = str_replace_all(name, "X|_", ""),
        name = case_when(
            name == "GLPVDA" ~ "PvdA / GL",
            name == "D66" ~ "D'66",
            name == "PVDD" ~ "PvdD",
            TRUE ~ name
        )
    ) %>%
    ggplot(
        aes(y = reorder(name, pct), x = pct)
    ) +
    geom_col(fill = "steelblue") +
    labs(
        title = "Probability of Parties being in a Majority Coalition",
        y = NULL,
        x = NULL,
        # caption = "Data from Wikipedia; counting coalitions with 75 or more seats and 5 or less parties"
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
seq_along(majoritycoalitions)
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
    filter(Freq > 0)

party_df <- party_df %>%
    mutate(
        Var1 = toupper(Var1),
        Var1 = str_replace_all(Var1, "X|_", ""),
        Var1 = case_when(
            Var1 == "GLPVDA" ~ "PvdA / GL",
            Var1 == "D66" ~ "D'66",
            Var1 == "PVDD" ~ "PvdD",
            TRUE ~ Var1
        ),
        Var2 = toupper(Var2),
        Var2 = str_replace_all(Var2, "X|_", ""),
        Var2 = case_when(
            Var2 == "GLPVDA" ~ "PvdA / GL",
            Var2 == "D66" ~ "D'66",
            Var2 == "PVDD" ~ "PvdD",
            TRUE ~ Var2
        )
    ) %>%
    mutate(Freq = round(Freq / nrow(majoritycoalitions), 3))


party_df$Var1 <- factor(party_df$Var1, levels = sort(unique(party_df$Var1)))
party_df$Var2 <- factor(party_df$Var2, levels = sort(unique(party_df$Var1)))


heatmap_g <-
    ggplot(party_df, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "white", mid = "gray90", high = "steelblue") +
    geom_text(aes(label = scales::percent(Freq, accuracy = 2)),
        color = "black", size = 2.5
    ) +
    labs(
        title = "Probability of Majority Coalitions Parties are in Together",
        x = NULL,
        y = NULL,
        fill = "Count",
        legend = NULL
        # caption = "Data from Wikipedia; counting coalitions with 75 or more seats and 5 or less parties"
    ) +
    coord_fixed() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
    )

party_coal_g + heatmap_g + plot_layout(ncol = 2) +
    plot_annotation(
        title = "Dutch Election 2025: Coalition Analysis",
        caption = "Data from Wikipedia; counting coalitions with 75 or more seats and 5 or less parties"
    ) & theme(
    plot.title = element_text(hjust = 0),
    plot.caption = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot"
)

ggsave("dutch-elections/dutch_elections_poll_coalitions_2025.png", width = 12, height = 6)
