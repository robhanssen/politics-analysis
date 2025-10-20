
# generate_coalitions <- function(election, min_seats = MIN_SEATS, max_parties = MAX_PARTIES) {
#     partycount <- nrow(election)

#     decomp_vector <- function(vector) {
#         v <- strsplit(vector, "")[[1]]
#         as.numeric(v)
#     }

#     partyseat_gen <- function(bitlst, elec) {
#         bitvec <- unlist(bitlst)
#         sum(bitvec * elec$totalseats)
#     }

#     partylist_gen <- function(bitlst, elec) {
#         bitvec <- unlist(bitlst)
#         l <- elec$parties[which(bitvec == 1)]
#         paste(l, collapse = ", ")
#     }

#     num_length <- 2^partycount - 1

#     coalitions <-
#         tibble(
#             numbers = 1:num_length,
#             bitcode = R.utils::intToBin(numbers)
#         ) %>%
#         mutate(
#             bitlist = map(
#                 numbers,
#                 ~ decomp_vector(bitcode[.x])
#             ),
#             seatcount = map_dbl(
#                 numbers,
#                 ~ partyseat_gen(bitlist[.x], election)
#             ),
#             partylist = map_chr(
#                 numbers,
#                 ~ partylist_gen(bitlist[.x], election)
#             ),
#             numparties = map_dbl(
#                 numbers,
#                 ~ sum(unlist(bitlist[.x]))
#             )
#         ) %>%
#         select(partylist, numparties, seatcount)

#     majoritycoalitions <- coalitions %>%
#         filter(seatcount >= MIN_SEATS) %>%
#         filter(numparties <= MAX_PARTIES) %>%
#         arrange(numparties, -seatcount)

#     majoritycoalitions
# }

generate_coalitions <- function(poll, min_seats = MIN_SEATS, max_parties = MAX_PARTIES) {
    seats <- set_names(poll$totalseats, poll$parties)
    coalitions_df <- map_dfr(
        seq_len(min(MAX_PARTIES, length(seats))),
        function(n) {
            combos <- combn(
                names(seats), n,
                FUN = function(parts) {
                    total <- sum(seats[parts])
                    if (total < MIN_SEATS) {
                        return(NULL)
                    }
                    tibble(
                        partylist = paste(parts, collapse = ", "),
                        numparties = n,
                        seatcount = total
                    )
                },
                simplify = FALSE
            )
            bind_rows(combos)
        }
    )
}


extract_coalition <- function(majoritycoalitions) {
    maj_party_list <- majoritycoalitions %>%
        pull(partylist) %>%
        lapply(., \(x) strsplit(x, ", ") %>% unlist())

    x <- table(unlist(maj_party_list)) / length(maj_party_list)
    x
}
