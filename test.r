library(tidyverse)

# Peiling Verian (14-10-2025)
# fmt:skip
df <- tribble(
    ~party, ~seats,
    "PVV", 31,
    "GL/PvdA", 25,
    "CDA", 23,
    "VVD", 14,
    "JA21", 13,
    "FvD", 5,
    "SP", 4,
    "Volt", 4,
    "BBB", 4,
    "PvdD", 3,
    "CU", 3,
    "SGP", 3,
    "DENK", 2,
    "50+", 2
)

coalitions <- map_dfr(1:5, function(m) {
    combn(df$party, m, simplify = FALSE) |>
        map_df(
            ~ tibble(
                coalition = list(.x),
                total_seats = sum(df |> filter(party %in% .x) |> pull(seats)),
                size = m
            )
        )
}) |>
    filter(total_seats >= 76)

df_plot <- 
    coalitions |>
    mutate(pairs = map(coalition, ~ combn(.x, 2, simplify = FALSE))) |>
    unnest(pairs) |>
    mutate(
        pair = map_chr(pairs, ~ paste(sort(.x), collapse = " + "))
    ) |>
    unnest_wider(pairs, names_sep = "_") |>
    add_count(pair) |>
    arrange(desc(n)) |>
    mutate(
        pairs_1 = fct_inorder(pairs_1),
        pairs_2 = factor(pairs_2, levels = levels(pairs_1)),
    ) |>
    drop_na()#%>%
    # mutate(n = round(n / nrow(coalitions), 3)) -> df_plot

labs <- distinct(df_plot, pairs_1, pairs_2, n)

ggplot(df_plot) +
    geom_tile(aes(x = pairs_1, y = pairs_2, fill = n)) +
    geom_text(
        data = labs,
        aes(
            x = pairs_1,
            y = pairs_2,
            label = n,
            color = ifelse(n > 100, "white", "black")
        )
    ) +
    scale_y_discrete(limits = rev(levels(df_plot$pairs_2)[-1])) +
    scale_fill_gradient(low = "#e0e2e4", high = "#4681b3") +
    scale_color_identity() +
    theme_void() +
    theme(
        axis.text = element_text()
    )
