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

url <- "https://en.wikipedia.org/wiki/List_of_current_United_States_governors"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
    html_table(fill = TRUE)

names(data_raw) <- data_raw[1, ]

governors <-
    data_raw %>%
    janitor::clean_names() %>%
    filter(state != "State") %>%
    select(state, governor, inauguration, party = party_2, born) %>%
    mutate(
        party = case_when(
            str_detect(party, "Democratic") ~ "Democratic",
            str_detect(party, "Republican") ~ "Republican",
            TRUE ~ party
        ),
        state = str_trim(str_remove(state, "\\(list\\)")),
        inauguration = mdy(inauguration),
        state_abb = state.abb[which(state.name == state)],
        born = ymd(str_remove_all(born, "\\).*|\\(")),
        age = (today() - born) / dyears(1),
        age_in_office = (inauguration - born) / dyears(1),
        time_in_office = (today() - inauguration) / dyears(1)
    )

save(governors, file = "us-politics-ages/governors.RData")

gov_age <-
    governors %>%
    mutate(name = fct_reorder(governor, age)) %>%
    ggplot(aes(y = name)) +
    geom_segment(
        aes(
            yend = name,
            x = age_in_office,
            xend = age,
            color = party
        ),
        linewidth = 3
    ) +
    scale_x_continuous(
        breaks = seq(0, 100, 10),
        limits = c(30, 95),
        sec.axis = sec_axis(~.,
            breaks = seq(0, 100, 10)
        )
    ) +
    labs(x = "Age", y = "") +
    scale_color_manual(
        values =
            c(
                "Democratic" = "blue",
                "Republican" = "red",
                "Independent" = "gray70"
            )
    )

ggsave("us-politics-ages/governors_age.png",
    width = 6, height = 12,
    plot = gov_age
)
