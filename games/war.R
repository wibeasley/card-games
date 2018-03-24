# knitr::stitch_rmd(script="./manipulation/te-ellis.R", output="./stitched-output/manipulation/te-ellis.md") # dir.create("./stitched-output/manipulation/", recursive=T)
# For a brief description of this file see the presentation at
#   - slides: https://rawgit.com/wibeasley/RAnalysisSkeleton/master/documentation/time-and-effort-synthesis.html#/
#   - code: https://github.com/wibeasley/RAnalysisSkeleton/blob/master/documentation/time-and-effort-synthesis.Rpres
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr            , quietly=TRUE)

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------

possible_suites <- c("h", "c", "d", "s") # heart, club, diamond, spade
possible_number <- c(2:10, "j", "q", "k", "a")
# possible_number <- factor(possible_number, levels=possible_number, ordered = T)

ds_deck <- tidyr::crossing(tidyr::nesting(number=possible_number, rank=seq_along(possible_number)), suite=possible_suites) %>%
  dplyr::mutate(
    card  = paste0(suite, number)
    # rank  =
  ) %>%
  dplyr::select(
    -number, -suite
  )
deck_count <- nrow(ds_deck)
testit::assert("The deck should always have 52 cards", deck_count==52L)
rm(possible_number, possible_suites)


# ---- load-data ---------------------------------------------------------------

indices_player_1 <- sample(deck_count, size=deck_count/2, replace=F)
indices_player_2 <- setdiff(seq_len(deck_count), indices_player_1) %>%
  sample()


# ---- tweak-data --------------------------------------------------------------
ds_player_1 <- ds_deck %>%
  dplyr::slice(indices_player_1)

ds_player_2 <- ds_deck %>%
  dplyr::slice(indices_player_2)

rm(ds_deck, indices_player_1, indices_player_2)

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