# presidents age in office

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

url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    select(name_birth_death, term = term_14)

data_cleaned <-
    data_raw %>%
    mutate(across(
        name_birth_death:term,
        ~ str_remove_all(.x, "\\[\\d{2}\\]")
    )) %>%
    mutate(across(
        name_birth_death:term,
        ~ str_remove_all(.x, "\\[\\d{1}\\]")
    )) %>%
    mutate(across(
        name_birth_death:term,
        ~ str_remove_all(.x, "\\[[[:lower:]]\\]")
    )) %>%
    separate(name_birth_death, into = c("name", "birth_death"), sep = "\\(") %>%
    mutate(across(birth_death, ~ str_remove_all(.x, "(\\)|b\\.)"))) %>%
    separate(birth_death, into = c("birth", "death"), sep = "\\–") %>%
    separate(term, into = c("term_start", "term_end"), sep = "\\–") %>%
    mutate(across(starts_with("term"), ~ as.Date(.x, format = "%B %d, %Y"))) %>%
    mutate(across(
        birth:death,
        ~ lubridate::floor_date(as.Date(.x, format = "%Y"), unit = "year")
    ))

presidents <-
    data_cleaned %>%
    mutate(
        age = (death - birth) / lubridate::dyears(1),
        alive = is.na(age),
        age_in_office = (term_start - birth) / lubridate::dyears(1),
        age_out_office = (term_end - birth) / lubridate::dyears(1),
        end_bar = ifelse(is.na(term_end),
            (lubridate::today() - birth) / dyears(1),
            age_out_office
        ),
        current_age = ifelse(!is.na(age), NA,
            (lubridate::today() - birth) / dyears(1)
        )
    ) %>%
    group_by(name) %>%
    mutate(youngest_age_in_office = min(age_in_office)) %>%
    ungroup()

prez_plot <-
    presidents %>%
    mutate(name = fct_reorder(name, youngest_age_in_office)) %>%
    ggplot() +
    aes(y = name) +
    geom_point(aes(x = current_age, color = alive), shape = 5, size = 3) +
    geom_segment(aes(
        yend = name,
        x = age_in_office,
        xend = end_bar,
        color = alive
    ),
    linewidth = 3
    ) +
    # geom_vline(
    #     xintercept = mean(presidents$age_in_office),
    #     lty = 3
    # ) +
    # geom_vline(
    #     xintercept = mean(presidents$age_out_office, na.rm = TRUE),
    #     lty = 3
    # ) +
    scale_x_continuous(
        breaks = seq(0, 100, 10),
        limits = c(35, NA),
        sec.axis = sec_axis(~.,
            breaks = seq(0, 100, 10)
        )
    ) +
    scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70")) +
    labs(x = "Age", y = "")

ggsave("us-politics-ages/president_ages.png", width = 8, height = 6, 
    plot = prez_plot)

#
# distribution
#

pdf_lognorm <- function(x, mu, sig) {
    .5 * (1 + pracma::erf((log(x) - mu) / (sqrt(2) * sig)))
}

logmean_age <- mean(log(presidents$age_in_office))
logsd_age <- sd(log(presidents$age_in_office))

age_cdf <-
    tibble(
        ages = seq(35, 85, 3),
        cdf = map_dbl(ages, ~ nrow(presidents %>% filter(age_in_office < .x))) / nrow(presidents), # nolint
        cdf_simul_log = map_dbl(ages, ~ pdf_lognorm(.x, logmean_age, logsd_age)) # nolint
    )

lognormal_plot <-
    age_cdf %>%
    ggplot() +
    aes(x = ages) +
    geom_point(aes(y = cdf), alpha = .3) +
    geom_line(aes(y = cdf_simul_log), alpha = .2) +
    geom_vline(xintercept = exp(logmean_age), alpha = .2) +
    labs(x = "", y = "", title = "Age log-normal CDF") +
    theme(
        plot.background = element_blank(),
        plot.title = element_text(size = 8, hjust = .5),
        axis.text = element_text(size = 6)
    )

prez_plot_cdf <- prez_plot + 
     patchwork::inset_element(lognormal_plot, .0, .6, .3, .95)


ggsave("us-politics-ages/president_ages_cdf.png", width = 8, height = 6, 
    plot = prez_plot_cdf)
