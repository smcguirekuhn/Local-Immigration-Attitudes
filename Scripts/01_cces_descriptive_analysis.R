
# Script 01: Compile and Describe CCES Datasets

# reset global environment ----
rm(list = ls())

# import packages ----
library(haven)
library(purrr)
library(dplyr)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/"
ccesCodedPath <- "CCES/Coded"
ccesCombinedFilename <- "CCES/CCESCombined.rds"

# combine cces data frames ----
ccesCombined <- list.files(
  path = file.path(dataPath, ccesCodedPath),
  full.names = TRUE
) |>
  purrr::map_dfr(
    .f = \(ccesFile) readRDS(ccesFile) |>
      dplyr::mutate(
        Year = strsplit(x = ccesFile, split = "_")[[1]][2] |> as.numeric(),
        .before = 1
      )
  )

# save combined cces data ---
ccesCombined |> saveRDS(file = file.path(dataPath, ccesCombinedFilename))
