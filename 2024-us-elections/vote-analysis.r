library(tidyverse)
library(patchwork)

theme_set(
    theme_light()
)

datafiles <- list.files("2024-us-elections/sources", pattern = "^results", full.names = TRUE)


elections <-
    map_dfr(datafiles, \(f) {
        read_csv(
            f,
            col_types = "cccdddddd"
        )
    },
    .id = "f"
    ) %>%
    mutate(
        county_fips = str_pad(county_fips, 5, pad = "0"),
        year = ifelse(f == "1", 2020L, 2024L)
    ) %>%
    select(-f)

# rucc %>% distinct(description) %>% write_csv("2024-us-elections/sources/rucc_desc_2023.csv")


rucc <- read_csv("2024-us-elections/sources/rucc2023.csv") %>%
    pivot_wider(names_from = "Attribute", values_from = "Value") %>%
    janitor::clean_names() %>%
    rename(
        county_fips = fips
    ) %>%
    mutate(
        population = as.numeric(population_2020),
        rucc = as.numeric(rucc_2023), # factor(rucc_2023, levels = as.character(1:9), label = 1:9)
        description = case_when(
            rucc == 1 ~ "1 - Metro areas of\n>1 million population",
            rucc == 2 ~ "2 - Metro areas of\n 250,000-1 million population",
            rucc == 3 ~ "3 - Metro areas of\n <250,000 population",
            rucc == 4 ~ "4 - Urban population >20,000\nadjacent to a metro area",
            rucc == 5 ~ "5 - Urban population >20,000\nnot adjacent to a metro area",
            rucc == 6 ~ "6 - Urban population 5,000-20,000\nadjacent to a metro area",
            rucc == 7 ~ "7 - Urban population of 5,000-20,000\nnot adjacent to a metro area",
            rucc == 8 ~ "8 - Urban population <5,000\nadjacent to a metro area",
            rucc == 9 ~ "9 - Urban population <5,000\nnot adjacent to a metro area"
        ),
        description = factor(description)
    )


vote_data <-
    full_join(
        elections, rucc,
        by = join_by(county_fips)
    )

cu_votes_g <-
    vote_data %>%
    summarize(
        votes_gop = sum(votes_gop, na.rm = TRUE),
        votes_dem = sum(votes_dem, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        .by = c("year", "rucc", "description")
    ) %>%
    drop_na() %>%
    arrange(year, rucc) %>%
    group_by(year) %>%
    mutate(across(votes_gop:votes_dem, cumsum)) %>%
    mutate(year = factor(year, levels = c(2020, 2024), labels = c("2020", "2024"))) %>%
    ggplot(
        aes(y = fct_reorder(description, -rucc))
    ) +
    geom_line(
        aes(x = votes_gop, group = year, linetype = year),
        color = "red"
    ) +
    geom_line(
        aes(x = votes_dem, group = year, linetype = year),
        color = "blue"
    ) +
    scale_x_continuous(
        limits = c(35e6, 85e6),
        labels = scales::label_number(
            scale = 1e-6,
            suffix = "M"
        )
    ) +
    scale_linetype_manual(
        breaks = c("2020", "2024"),
        values = c("dashed", "solid"),
    ) +
    labs(
        x = "Cumulative votes",
        y = NULL
    ) +
    annotate(
        "text",
        x = c(73.5e6, 71e6), y = c(7, 5),
        label = c("(2020)", "(2024)"),
        size = 6, size.unit = "pt",
        hjust = 0
    ) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank()
    )


per_gop_g <-
    vote_data %>%
    summarize(
        votes_gop = sum(votes_gop, na.rm = TRUE),
        votes_dem = sum(votes_dem, na.rm = TRUE),
        .by = c("year", "rucc", "description")
    ) %>%
    drop_na() %>%
    arrange(year, rucc) %>%
    pivot_wider(names_from = "year", values_from = c("votes_gop", "votes_dem")) %>%
    mutate(
        per_gop2020 = votes_gop_2020 / (votes_gop_2020 + votes_dem_2020),
        per_gop2024 = votes_gop_2024 / (votes_gop_2024 + votes_dem_2024)
    ) %>%
    ggplot(
        aes(y = fct_reorder(description, -rucc))
    ) +
    geom_vline(xintercept = 0.5, linetype = "solid", color = "gray", alpha = .25, linewidth = 2) +
    geom_segment(
        aes(x = per_gop2020, xend = per_gop2024, yend = description),
        arrow = arrow(type = "closed", length = unit(0.1, "inches"), angle = 10), color = "red",
    ) +
    geom_point(
        aes(x = per_gop2020),
        shape = 1,
        size = 1
    ) +
    geom_point(
        aes(x = per_gop2024),
        shape = 16,
        size = 1
    ) +
    annotate(
        geom = "text",
        x = c(.59, .63),
        y = 5,
        hjust = c(1, 0),
        label = c("(2020)", "(2024)"),
        size = 6, size.unit = "pt"
    ) +
    scale_x_continuous(
        limits = c(.4, .8),
        labels = scales::percent_format()
    ) +
    labs(x = "GOP vote share (in %)", y = NULL)


p <- per_gop_g + cu_votes_g &
    theme(
        axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_blank()
    )


ggsave("2024-us-elections/graphs/vote-analysis.png", p, width = 12, height = 4)


# create a map from the table vote_data where  the color of the county
# is determined by the per_gop measure, with high values in red and low value in blue
# at a midpoint of 0.5
library(ggplot2)
library(sf)
library(tidyverse)

vote_map <-
    vote_data %>%
    mutate(
        county = toupper(county),
        state = toupper(state.x)
    ) %>%
    mutate(
        level = cut(
            per_gop,
            breaks = c(0, .33, 0.40, 0.60, 0.67, 1),
            labels = c("X-Dem", "M-Dem", "Center", "M-Rep", "X-Rep")
        ),
    )




countymapdata <-
    as_tibble(map_data("county")) %>%
    rename(state = region, county = subregion) %>%
    mutate(
        state = toupper(state),
        county = toupper(county)
    ) %>%
    inner_join(vote_map, by = join_by(state, county))


ggplot(data = countymapdata) +
    geom_polygon(
        aes(
            x = long,
            y = lat,
            fill = level,
            group = group
        ),
        color = "white",
        linewidth = 0
    ) +
    coord_fixed(1.4) +
    scale_fill_manual(
        values =
            c("X-Dem" = "blue", "M-Dem" = "dodgerblue", "Center" = "green", "M-Rep" = "pink", "X-Rep" = "red")
    ) +
    # scale_fill_gradient2(
    #     low = "dodgerblue",
    #     mid = "green",
    #     high = "red",
    #     midpoint = 0.5,
    #     name = "GOP vote share"
    # ) +
    labs(
        x = "Long",
        y = "Lat",
    ) +
    theme(legend.position = "none")



vote_data %>%
    # filter(year == 2024) %>%
    arrange(-population) %>%
    mutate(county_count = 1) %>%
    group_by(year) %>%
    mutate(
        cu_vote_dem = cumsum(votes_dem),
        cu_vote_gop = cumsum(votes_gop),
        cu_county = cumsum(county_count),
        cu_pop = cu_vote_dem + cu_vote_gop,
    ) %>%
    ungroup() %>%
    drop_na(year) %>%
    ggplot(
        aes(x = cu_pop, group = factor(year), linetype = factor(year))
    ) +
    geom_line(aes(y = cu_vote_dem), color = "blue") +
    geom_line(aes(y = cu_vote_gop), color = "red") +
    geom_line(aes(y = cu_county * 20e3), color = "black") +
    scale_x_continuous(
        limits = c(0, NA),
        labels = scales::label_number(
            scale = 1e-6,
            suffix = "M"
        )
    ) +
    scale_y_continuous(
        limits = c(0, 90e6),
        labels = scales::label_number(
            scale = 1e-6,
            suffix = "M",
            
        ),
        sec.axis = sec_axis(~ . / 20e3, name = "Cumulative counties", breaks = seq(0, 3000, 1000))
    ) +
    scale_linetype_manual(
        breaks = c("2020", "2024"),
        values = c("dashed", "solid"),
    ) +
    annotate(
        geom = "text",
        x = c(75e6, 80e6), y = c(26e6, 35e6),
        label = c("(2020)", "(2024)"), hjust = c(0, 1)
    ) +
    theme(
        legend.position = "none",
        axis.text.y.right = element_text(color = "black"),
        axis.title.y.right = element_text(color = "black")
    )  +
    labs(
        x = "Cumulative votes",
        y = "Cumulative votes",
        title = "Cumulative votes during 2024 Presidential Election",
        subtitle = "Cumulative votes ordered by size of descending population by county",
    )

ggsave("2024-us-elections/graphs/vote-cum.png", width = 8, height = 6)