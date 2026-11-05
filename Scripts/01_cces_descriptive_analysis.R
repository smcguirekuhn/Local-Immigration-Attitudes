
# Script 01: Compile and Describe CCES Datasets

# reset global environment ----
rm(list = ls())

# import packages ----
library(haven)
library(purrr)
library(dplyr)
library(ggplot2)

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
) |> purrr::map_dfr(.f = \(ccesFile) readRDS(file = ccesFile))

# save combined cces data ---
ccesCombined |> saveRDS(file = file.path(dataPath, ccesCombinedFilename))

ccesCombined |>
  dplyr::group_by(Year, Democrat) |>
  dplyr::summarise(`Mean Support` = mean(`Increase Patrols`, na.rm = TRUE)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = `Mean Support`, group = factor(Democrat))) +
  ggplot2::geom_line(mapping = ggplot2::aes(color = factor(Democrat))) +
  ggplot2::labs(title = "Support for Increasing Border Patrols", color = "Democrats") +
  ggplot2::theme_minimal()
