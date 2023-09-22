library(tidyverse)
library(lubridate)
library(rvest)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
    ))

url <- "https://en.wikipedia.org/wiki/Government_shutdowns_in_the_United_States"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>%
    html_table(fill = TRUE)

shutdown <-
    data_raw %>%
    janitor::clean_names() %>%
    select(-refs, -agenciesaffected, -cost_togovernment) %>%
    rename(
        years = "shutdown",
        employees = "employeesfurloughed"
    ) %>%
    mutate(
        days = str_extract(days, "\\d{1,2}"),
        employees = str_remove_all(employees, "\\,"),
    ) %>%
    mutate(across(c("days", "employees"), as.numeric))

names(shutdown)[5:6] <- c("house", "senate")

prezorder <- unique(shutdown$president)

shutdown2 <-
    shutdown %>%
    mutate(house = str_remove_all(house, " \\(.*\\)")) %>%
    mutate(across(house:senate, factor),
        president = factor(president, levels = prezorder)
    )

shut_g <-
    shutdown2 %>%
    ggplot(aes(x = president, fill = house, y = days)) +
    geom_col(color = "black", alpha = .9) +
    scale_y_continuous(breaks = seq(0, 100, 5)) +
    scale_fill_manual(
        values = c(
            "Democrats" = "blue",
            "Republicans" = "red"
        )
    ) +
    labs(
        x = "President during shutdown", y = "Length of shutdown (in days)",
        title = "Length of government shutdowns over the last decades",
        subtitle = "Color indicates party in control of the House",
        caption = "Democrats in blue; Republicans in red\nSource: Wikipedia"
    )

ggsave("gov-shutdowns/gov-shutdowns.png",
    height = 4, width = 6,
    plot = shut_g
)
