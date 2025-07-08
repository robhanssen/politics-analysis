library(tidyverse)
library(rvest)


url <- "https://catholic-hierarchy.org/bishop/spope0.html"

read_html(url) %>%
    html_element("table") %>%
    html_table()  %>%
    janitor::clean_names() %>%
    slice(-1) %>%
    mutate(
        number = as.integer(number),
        birth_date = dmy(birth_date),
        conclave_start = dmy(conclave_start),
        elected = dmy(elected),
        installed = dmy(installed)
    ) %>%
    mutate(
        century = ceiling(year(elected) / 100),
        length_conclave = (elected - conclave_start)/ddays(1) + 1
    ) %>% drop_na(length_conclave) %>%
    slice_max(number, n = 10) %>%
    select(number, papal_name, length_conclave) %>%
  
    ggplot(aes(x = factor(century), y = length_conclave)) + 
    geom_boxplot()  +
    scale_y_log10()