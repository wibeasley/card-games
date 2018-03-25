rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr            , quietly=TRUE)

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("dequer"       )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("VeryLargeIntegers") # For 64-bit integers, coutning the large number of turns.
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------

possible_suites <- c("h", "c", "d", "s") # heart, club, diamond, spade
possible_number <- c(2:10, "j", "q", "k", "a")
# possible_number <- factor(possible_number, levels=possible_number, ordered = T)

ds_deck <- possible_number %>%
  tidyr::nesting(number=., rank=seq_along(.)) %>%
  tidyr::crossing(., suite=possible_suites) %>%
  dplyr::mutate(
    card  = sprintf("%2s of %s", number, suite)
  ) %>%
  dplyr::select(
    -number, -suite
  )
# ds_deck
deck_count <- nrow(ds_deck)
testit::assert("The deck should always have 52 cards", deck_count==52L)
rm(possible_number, possible_suites)

determine_rank <- function( card_name ) {
  ds_selected_card <- ds_deck %>%
    dplyr::filter(card == card_name)
  testit::assert(nrow(ds_selected_card) == 1L)

  return( ds_selected_card$rank )
}

print_draw <- function( c1, c2, r1, r2 ) {
  message(sprintf(
    "Turn %s\n\tPlayer 1 card: %7s (rank %2i); %2i cards remaining\n\tPlayer 2 card: %7s (rank %2i); %2i cards remaining\n\tWinner: %s",
    as.character(turn_index),
    c1, r1, length(deck_1),
    c2, r2, length(deck_2),
    determine_winner(r1, r2)
  ))
}

determine_winner <- function( r1, r2 ) {
  winner <- dplyr::case_when(
    r1  < r2 ~ "p2",
    r2  < r1 ~ "p1",
    r1 == r2 ~ "tie",
    TRUE ~ NA_character_
  )
  testit::assert(!is.na(winner))
  return( winner )
}
determine_escrow_size <- function( rank_both ) {
  as.integer(min(rank_both+1L, length(deck_1), length(deck_2)))
}
# determine_rank("d2")
# determine_rank("sj")

# ---- load-data ---------------------------------------------------------------
# indices_player_1 <- sample(deck_count, size=deck_count/2, replace=F) %>%
#   sample()
# indices_player_2 <- setdiff(seq_len(deck_count), indices_player_1) %>%
#   sample() %>%
#   sample()

ds_deck <- ds_deck %>%
  dplyr::sample_n(nrow(.)) %>%
  dplyr::sample_n(nrow(.))

# ---- tweak-data --------------------------------------------------------------
ds_player_1 <- ds_deck %>%
  dplyr::slice(1:26) %>%
  dplyr::sample_n(nrow(.))

ds_player_2 <- ds_deck %>%
  dplyr::slice(27:52) %>%
  dplyr::sample_n(nrow(.))

# rm(indices_player_1, indices_player_2)

# plot(ds_player_1$rank, type="l")
# lines(ds_player_2$rank, col="blue")

deck_1   <- dequer::queue() # FIFO
deck_2   <- dequer::queue() # FIFO
purrr::walk(ds_player_1$card, ~dequer::pushback(deck_1, .))
purrr::walk(ds_player_2$card, ~dequer::pushback(deck_2, .))
# deck_1
# str(deck_1)


# ---- run ---------------------------------------------------------------------
# determine_winner(5,3)

tie <- function( rank_both ) {
  escrow_1 <- list()
  escrow_2 <- list()
  for( i in seq_len(determine_escrow_size) ) {
    escrow_1[i] <- dequer::pop(deck_1)
    escrow_2[i] <- dequer::pop(deck_2)
  }
  rank_1 <- determine_rank(escrow_1[length(escrow_1)])
  rank_2 <- determine_rank(escrow_2[length(escrow_2)])

  winner <- determine_winner(rank_1, rank_2)
  if( winner == "p1") {
    purrr::walk(escrow_1, ~dequer::pushback(deck_1, .))
    purrr::walk(escrow_2, ~dequer::pushback(deck_1, .))
    dequer::pushback(deck_1, card_1)
    dequer::pushback(deck_1, card_2)
  } else if( winner == "p2") {
    purrr::walk(escrow_1, ~dequer::pushback(deck_1, .))
    purrr::walk(escrow_2, ~dequer::pushback(deck_2, .))
    dequer::pushback(deck_2, card_1)
    dequer::pushback(deck_2, card_2)
  } else if ( sample(c(T, F), size=1) ) {
    message("Tie again, and randomly give to player 1.")
    purrr::walk(escrow_1, ~dequer::pushback(deck_1, .))
    purrr::walk(escrow_2, ~dequer::pushback(deck_1, .))
    dequer::pushback(deck_1, card_1)
    dequer::pushback(deck_1, card_2)
  } else {
    message("Tie again, and randomly give to player 2.")
    purrr::walk(escrow_1, ~dequer::pushback(deck_1, .))
    purrr::walk(escrow_2, ~dequer::pushback(deck_2, .))
    dequer::pushback(deck_2, card_1)
    dequer::pushback(deck_2, card_2)
  }
  Sys.sleep(2)
}

turn_index <- VeryLargeIntegers::as.vli(0L)
while( 0<length(deck_1) && 0<length(deck_2) ) {
  turn_index <- turn_index + VeryLargeIntegers::as.vli(1)
  card_1 <- dequer::pop(deck_1)
  card_2 <- dequer::pop(deck_2)
  rank_1 <- determine_rank(card_1)
  rank_2 <- determine_rank(card_2)

  print_draw(card_1, card_2, rank_1, rank_2)

  winner <- determine_winner(rank_1, rank_2)
  if( winner == "p1") {
    dequer::pushback(deck_1, card_1)
    dequer::pushback(deck_1, card_2)
  } else if( winner == "p2") {
    dequer::pushback(deck_2, card_1)
    dequer::pushback(deck_2, card_2)
  } else {
    testit::assert(winner=="tie")
    # dequer::pushback(deck_1, card_1)
    # dequer::pushback(deck_2, card_2)
  }
  Sys.sleep(1/16)
}

if( length(deck_1) == 0L ) {
  message("Player 2 Wins !!!!!!")
} else if( length(deck_2) == 0L ) {
  message("Player 1 Wins !!!!!!")
} else {
  message("Both players lost on the final turn.")
}

# ---- verify-values -----------------------------------------------------------
# Sniff out problems
# OuhscMunge::verify_value_headstart(ds)

# ---- specify-columns-to-upload -----------------------------------------------
# dput(colnames(ds)) # Print colnames for line below.
# columns_to_write <- c(
# )
# ds_slim <- ds %>%
#   dplyr::select_(.dots=columns_to_write) %>%
#   # dplyr::slice(1:100) %>%
#   dplyr::mutate(
#     fte_approximated <- as.integer(fte_approximated)
#   )
# ds_slim
#
# rm(columns_to_write)

# ---- save-to-disk ------------------------------------------------------------
# If there's no PHI, a rectangular CSV is usually adequate, and it's portable to other machines and software.
# readr::write_csv(ds, path_out_unified)
# readr::write_rds(ds, path_out_unified, compress="gz") # Save as a compressed R-binary file if it's large or has a lot of factors.
