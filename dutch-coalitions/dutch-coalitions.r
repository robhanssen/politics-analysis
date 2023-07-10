# coalitions via rvest
library(tidyverse)
library(lubridate)
library(rvest)

oldloc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "dutch")


theme_set(theme_light() +
    theme(
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
    ))

url <- "https://nl.wikipedia.org/wiki/Nederlandse_kabinetten_sinds_de_Tweede_Wereldoorlog" # nolint

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    select(
        kabinet, minister_president, partijen,
        aantreden, demissionair, aftreden
    )

cab_data <-
    data_raw %>%
    mutate(
        kabinet = str_remove(kabinet, "\\[\\d{1}\\]"),
        partijen = str_trim(str_remove(partijen, "\\(.*\\)|\\[\\d{1}\\]")),
        partijen = str_replace(partijen, "D'66", "D66"),
        demissionair = str_trim(str_remove(
            demissionair,
            "\\(.*\\)|\\[\\d{1}\\]"
        )),
        across(aantreden:aftreden, dmy),
        partijlist = as.list(strsplit(partijen, ", ")),
        min_pres_partij = str_remove_all(minister_president, ".*\\(|\\)"),
    ) %>%
    replace_na(list(aftreden = ymd(20230707))) %>%
    mutate(cab_length = (aftreden - aantreden) / dyears(1))



in_cab_leng <-
    cab_data %>%
    unnest_longer(partijlist) %>%
    group_by(partijlist) %>%
    summarize(in_cab = sum(cab_length)) %>%
    rename(partij = partijlist, in_kabinet = in_cab) %>%
    mutate(in_kabinet = round(in_kabinet, 1)) %>%
    arrange(-in_kabinet)

cab_data %>%
    filter(aantreden > ymd(19750101)) %>%
    group_by(minister_president) %>%
    summarize(min_pres_tijd = sum(cab_length), kabinetten = n()) %>%
    arrange(-min_pres_tijd) %>%
    mutate(gemiddelde_duur = min_pres_tijd / kabinetten)

cab_data %>%
    filter(aantreden > ymd(19750101)) %>%
    group_by(min_pres_partij) %>%
    summarize(min_pres_tijd = sum(cab_length), kabinetten = n()) %>%
    arrange(-min_pres_tijd) %>%
    mutate(min_pres_tijd = round(min_pres_tijd, 1)) %>%
    knitr::kable(caption = "Partij van de minister-president sinds 1975")


colors <-
    c(
        "KVP" = "darkgreen",
        "CHU" = "darkgreen",
        "CDA" = "darkgreen",
        "ARP" = "darkgreen",
        "VDB" = "red",
        "PvdA" = "red",
        "VVD" = "blue",
        "D66" = "orange",
        "DS'70" = "red",
        "PPR" = "lightgreen",
        "LPF" = "black",
        "ChristenUnie" = "lightblue"
    )


cab_data %>%
    group_by(minister_president) %>%
    mutate(eerst_cab = first(aantreden)) %>%
    ungroup() %>%
    ggplot(aes(y = fct_reorder(minister_president, desc(eerst_cab)))) +
    geom_segment(
        aes(
            x = aantreden, xend = aftreden,
            yend = minister_president, color = min_pres_partij
        ),
        linewidth = 8, alpha = 1
    ) +
    scale_color_manual(values = colors) +
    labs(x = "", y = "")


cab_data %>%
    unnest_longer(partijlist) %>%
    ggplot() +
    aes(y = factor(partijlist,
        ordered = TRUE,
        levels = rev(in_cab_leng$partij)
    )) +
    geom_segment(
        aes(
            x = aantreden, xend = aftreden,
            yend = partijlist, color = partijlist
        ),
        linewidth = 8, alpha = 1
    ) +
    scale_color_manual(values = colors) +
    labs(x = "", y = "")

Sys.setlocale("LC_TIME", oldloc)
