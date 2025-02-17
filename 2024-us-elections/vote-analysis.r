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

rucc <- read_csv("2024-us-elections/sources/rucc2023.csv") %>%
    pivot_wider(names_from = "Attribute", values_from = "Value") %>%
    janitor::clean_names() %>%
    rename(
        county_fips = fips
    ) %>%
    mutate(
        population = as.numeric(population_2020),
        rucc = as.numeric(rucc_2023), # factor(rucc_2023, levels = as.character(1:9), label = 1:9)
        description = str_replace(description, ", a", "\n a"),
        description = str_replace(description, ", not", "\n not"),
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
