rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr            , quietly=TRUE)
library(flifo)

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
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

ds_deck <- tidyr::crossing(tidyr::nesting(number=possible_number, rank=seq_along(possible_number)), suite=possible_suites) %>%
  dplyr::mutate(
    card  = paste0(number, " of ", suite)
    # rank  =
  ) %>%
  dplyr::select(
    -number, -suite
  )
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
    "Turn %s\n\tPlayer 1 card: %7s (rank %2i); %.0f cards remaining\n\tPlayer 2 card: %7s (rank %2i); %.0f cards remaining\n\tWinner: %s",
    as.character(turn_index),
    c1, r1, round(flifo::size(deck_1)/92L, 2),
    c2, r2, round(flifo::size(deck_2)/92L, 2),
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

# determine_rank("d2")
# determine_rank("sj")

# ---- load-data ---------------------------------------------------------------

indices_player_1 <- sample(deck_count, size=deck_count/2, replace=F)
indices_player_2 <- setdiff(seq_len(deck_count), indices_player_1) %>%
  sample()



# ---- tweak-data --------------------------------------------------------------
ds_player_1 <- ds_deck %>%
  dplyr::slice(indices_player_1)

ds_player_2 <- ds_deck %>%
  dplyr::slice(indices_player_2)

rm(indices_player_1, indices_player_2)

plot(ds_player_1$rank, type="l")
lines(ds_player_2$rank, col="blue")

deck_1   <- fifo(max_length = deck_count)
deck_2   <- fifo(max_length = deck_count)

for( i in 1:26 ) {
  flifo::push(deck_1, ds_player_1$card[i])
  flifo::push(deck_2, ds_player_2$card[i])
}

as.list(ds_player_1$card) %>%
  purrr::walk(function(x) flifo::push(deck_1, x))

# ---- run ---------------------------------------------------------------------
warning("currently, all cards are lost in a tie.  There are no winners in war.  (Actually, I've been too lazy to program ties.)")
# determine_winner(5,3)

# escrow_1 <- list()
#
# for( i in 1:4 ) {
#   escrow_1[i] <- flifo::pop(deck_1)
# }
#
# tie <- function( rank_both ) {
#
# }
# bit64::
turn_index <- VeryLargeIntegers::as.vli(0L)
while( !flifo::is.empty(deck_1) && !flifo::is.empty(deck_2) ) {
  turn_index <- turn_index + VeryLargeIntegers::as.vli(1)
  card_1 <- flifo::pop(deck_1)
  card_2 <- flifo::pop(deck_2)
  rank_1 <- determine_rank(card_1)
  rank_2 <- determine_rank(card_2)

  print_draw(card_1, card_2, rank_1, rank_2)

  winner <- determine_winner(rank_1, rank_2)
  if( winner == "p1") {
    flifo::push(deck_1, card_1)
    flifo::push(deck_1, card_2)

  } else if( winner == "p2") {
    flifo::push(deck_2, card_1)
    flifo::push(deck_2, card_2)

  } else {
    testit::assert(winner=="tie")
    # flifo::push(deck_1, card_1)
    # flifo::push(deck_2, card_2)
  }
  Sys.sleep(1/16)
}

if( flifo::is.empty(deck_1) ) {
  message("Player 2 Wins !!!!!!")
} else if( flifo::is.empty(deck_2) ) {
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
