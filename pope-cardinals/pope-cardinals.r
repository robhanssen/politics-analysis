library(tidyverse)
library(rvest)

theme_set(theme_light() +
    theme(
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        # legend.position = "none"
    ))

url <- "https://en.wikipedia.org/wiki/List_of_current_cardinals"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names()


data_cleaned <-
    data_raw %>%
    mutate(
        across(
            everything(),
            ~ str_remove_all(.x, "\\[\\d{2}\\]")
        ),
        born = str_remove(born, "\\(.*\\d{2}\\)$")
    ) %>%
    mutate(consist2 = consistory) %>%
    separate(consistory, into = c("date_induction", "pope"), sep = "(19|20)\\d{2}") %>%
    mutate(pope = str_remove(pope, "\\[.*\\]")) %>%
    mutate(year = str_extract(consist2, "\\d{4}")) %>%
    # select(year) %>%
    mutate(date_induction = paste(date_induction, year)) %>%
    mutate(
        across(c(born, date_induction), dmy),
        pope = str_remove(pope, "^.*\\]")
    ) %>%
    select(rank, name, country, born, date_induction, pope) %>%
        mutate(
        age = (today() - born)/dyears(1),
        age_induction = (date_induction - born)/dyears(1)
    ) 


data_cleaned %>%
    ggplot(aes(x = age, y = fct_reorder(name, -age), fill = pope)) + 
    geom_col(show.legend = TRUE)

data_cleaned %>%
    filter(age < 80) %>%
    count(pope) %>%
    mutate(p = n/sum(n)) %>%
    arrange(-n)


data_cleaned %>%
    # filter(age < 80) %>%
    count(pope) %>%
    mutate(p = n/sum(n)) %>%
    arrange(-n)

clist <- data_cleaned %>%
    count(country, sort = TRUE) %>%
    pull(country)


data_cleaned %>%
    mutate(country = factor(country, levels = clist)) %>%
    summarize(
        count = n(), 
        .by = c(pope, country)
    ) %>%
    arrange(pope, -count) %>%
    ggplot(aes(y = country, x = count, color = pope, group = pope)) +
    geom_line()