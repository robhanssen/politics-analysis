library(tidyverse)

euclid <- function(x, y) {
    sqrt((x - y)^2)
}

nl_parties <-
    read_csv("dutch-politics-distance/1999-2019_CHES_dataset_means(v3).csv") %>%
    filter(country == 10, year == 2019) %>%
    mutate(family = case_when(
        party == "SGP" ~ 2,
        party == "CU" ~ 2.1,
        party == "D66" ~ 3.9,
        party == "CDA" ~ 3.8,
        party == "50PLUS" ~ 3,
        party == "DENK" ~ 6,
        TRUE ~ family
    )) %>%
    arrange(family)

lvls <- nl_parties$party

# nl_parties %>%
#     filter(party == p1 | party == p2) %>%
#     select(-(1:7), -(9:12), -14) %>%
#     t(.) %>%
#     as_tibble() %>%
#     set_names(c("a", "b")) %>%
#     slice(-1) %>%
#     mutate(across(everything(), as.numeric)) %>%
#     mutate(dist = euclid(a, b)) %>%
#     reframe(x = sum(dist, na.rm = TRUE))



ccs <- combn(nl_parties$party, 2, function(x) {
    party1 <- nl_parties %>% filter(party == x[1])
    party2 <- nl_parties %>% filter(party == x[2])

    galtan_dist <- euclid(party1$galtan, party2$galtan)
    immigrate_dist <- euclid(party1$immigrate_policy, party2$immigrate_policy)
    total_dist <- galtan_dist + immigrate_dist

    list(
        party1 = x[1],
        party2 = x[2],
        total_dist = total_dist
    )
})


ccs <- combn(nl_parties$party, 2, function(x) {
    party1 <- nl_parties %>% filter(party == x[1])
    party2 <- nl_parties %>% filter(party == x[2])

    yy <- nl_parties %>%
        filter(party == x[1] | party == x[2]) %>%
        select(-(1:7), -(9:12), -14) %>%
        t(.) %>%
        as_tibble() %>%
        set_names(c("a", "b")) %>%
        slice(-1) %>%
        mutate(across(everything(), as.numeric)) %>%
        mutate(dist = euclid(a, b)) %>%
        reframe(x = sum(dist, na.rm = TRUE))



    list(
        party1 = x[1],
        party2 = x[2],
        total_dist = yy$x
    )
})

x <- unlist(ccs)

dist_df <-
    tibble(
        party1 = x[seq(1, length(x), 3)],
        party2 = x[seq(2, length(x), 3)],
        distance = x[seq(3, length(x), 3)]
    ) %>%
    mutate(
        distance = as.numeric(distance),
        dist_measure = scale(distance),
        party1 = factor(party1, levels = lvls),
        party2 = factor(party2, levels = lvls)
    )

dist_df %>%
    ggplot(aes(x = party1, y = party2, fill = dist_measure)) +
    geom_tile() +
    # scale_fill_gradient2(low = "green", mid = "white", high = "red") +
    scale_fill_distiller(palette = "Spectral", direction = -1) +
    labs(
        x = NULL, y = NULL, fill = "Ideologische\nafstand",
        caption = "Source: https://www.chesdata.eu/ches-europe"
    ) +
    coord_equal() +
    theme_minimal() +
    theme(
        legend.position = "inside",
        legend.position.inside = c(.9, .2),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave("dutch-politics-distance/party_distance_heatmap.png",
    width = 6,
    height = 5
)