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

url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States" # nolint

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
    html_table(fill = TRUE)

scotus <-
    data_raw %>%
    janitor::clean_names() %>%
    filter(justice != "Justice") %>%
    select(justice = justice_3, tenure) %>%
    separate(justice, into = c("name", "ages2"), sep = "\\(") %>%
    separate(ages2, into = c("birth", "death", sep = "\\-")) %>%
    separate(tenure, into = c("term_start", "term_end"), sep = "\\–") %>%
    mutate(
        name = str_remove_all(name, "\\[[[:lower:]]\\]"),
        term_start = str_remove_all(term_start, "\\[[[:lower:]]\\]"),
        term_end = str_remove_all(term_end, "\\(.*\\)"),
        across(term_start:term_end, mdy)
    ) %>%
    mutate(
        birth = ifelse(birth == "born", death, birth),
        death = ifelse(birth == death, NA, death),
        across(birth:death, as.numeric),
        across(birth:death, ~ ymd(.x * 1e4 + 101)),
        active = is.na(term_end),
        alive = is.na(death),
        term_end = as.Date(ifelse(is.na(term_end), today(), term_end),
            origin = "1970-01-01"
        ),
        age_in_office = (term_start - birth) / dyears(1),
        age_out_office = (term_end - birth) / dyears(1),
        term_length = (term_end - term_start) / dyears(1)
    ) %>%
    group_by(name) %>%
    mutate(
        youngest_age_in_office = min(age_in_office),
        oldest_age_out_office = max(age_out_office),
        term_length = sum(term_length)
    ) %>%
    ungroup()

scotus %>%
    mutate(name = fct_reorder(name, youngest_age_in_office)) %>%
    ggplot() +
    aes(y = name) +
    geom_segment(
        aes(
            yend = name,
            x = age_in_office,
            xend = age_out_office,
            color = active
        ),
        linewidth = 3
    ) +
    scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70")) +
    labs(x = "Age", y = "")

ggsave("us-politics-ages/scotus_ages.png", height = 15, width = 6)

scotus %>%
    mutate(name = fct_reorder(name, term_length)) %>%
    distinct(name, term_length, active) %>%
    ggplot() +
    aes(y = name, fill = active) +
    geom_col(aes(x = term_length)) +
    scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70")) +
    labs(x = "Age", y = "") +
    geom_vline(xintercept = median(scotus$term_length))
