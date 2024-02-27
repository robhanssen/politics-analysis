# representatives age in office

library(tidyverse)
library(lubridate)
library(rvest)

theme_set(theme_light() +
    theme(
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
    ))

url <- "https://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    rename_with(~"born", starts_with("born")) %>%
    select(district, member, party_2, born, assumed_office)

data_cleaned <-
    data_raw %>%
    mutate(across(everything(), ~ str_remove_all(.x, "\\[[[:lower:]]\\]")),
        born = ymd(str_remove_all(born, "\\).*|\\(")),
        party = factor(str_remove(party_2, "\\(DFL\\)")),
        assumed_office = as.numeric(str_trim(str_remove_all(assumed_office, "\\(.*|\\)"))),
        assumed_office = ymd(glue::glue("{assumed_office}-01-03")),
        state = str_trim(str_remove_all(district, "\\d{1,2}|at-large")),
        state_abb = sapply(state, function(x) state.abb[which(state.name == x)])
    ) %>%
    select(-party_2)


members <-
    data_cleaned %>%
    mutate(
        age = (today() - born) / lubridate::dyears(1),
        age_in_office = (assumed_office - born) / lubridate::dyears(1),
        time_in_office = (today() - assumed_office) / lubridate::dyears(1),
        state_abb = state.abb[state == state]
    ) %>%
    drop_na(age)

mem_plot <-
    members %>%
    mutate(name = fct_reorder(member, age)) %>%
    ggplot() +
    aes(y = name) +
    geom_segment(
        aes(
            yend = name,
            x = age_in_office,
            xend = age,
            color = party
        ),
        linewidth = .5
    ) +
    scale_x_continuous(
        breaks = seq(0, 100, 10),
        limits = c(30, 95),
        sec.axis = sec_axis(~.,
            breaks = seq(0, 100, 10)
        )
    ) +
    scale_color_manual(
        values = c(
            "Democratic" = "blue",
            "Republican" = "red",
            "Independent" = "gray70"
        )
    ) +
    labs(x = "Age", y = "") +
    geom_vline(
        xintercept = 77.27, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    annotate("text",
        x = 78.27, y = 100,
        hjust = 0, label = "US life\nexpectancy"
    ) +
    geom_vline(
        xintercept = 67, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    annotate("text",
        x = 68, y = 75,
        hjust = 0, label = "US age of\nretirement"
    ) +
    geom_vline(
        xintercept = 55, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    annotate("text",
        x = 56, y = 50,
        hjust = 0, label = "AARP membership\nminimum age"
    ) +
    geom_vline(
        xintercept = 39, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    annotate("text",
        x = 40, y = 15,
        hjust = 0, label = "US average\nage"
    ) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()
    )


ggsave("us-politics-ages/rep_ages.png",
    width = 6, height = 12,
    plot = mem_plot
)

time_mem_plot <-
    members %>%
    mutate(name = fct_reorder(member, time_in_office)) %>%
    ggplot() +
    aes(y = name) +
    geom_col(aes(x = time_in_office, fill = party)) +
    scale_x_continuous(
        breaks = seq(0, 100, 10),
        sec.axis = sec_axis(~.,
            breaks = seq(0, 100, 10)
        )
    ) +
    scale_fill_manual(
        values = c(
            "Democratic" = "blue",
            "Republican" = "red",
            "Independent" = "gray70"
        )
    ) +
    labs(x = "Time in office (in years)", y = "") +
    theme(axis.text.y = element_blank())

ggsave("us-politics-ages/rep_time_in_office.png",
    width = 6, height = 12,
    plot = time_mem_plot
)
