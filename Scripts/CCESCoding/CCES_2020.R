
# Recode 2020 CCES Responses

# reset global environment ----
rm(list = ls())

# import packages ----
library(haven)
library(purrr)
library(dplyr)
library(haven)
library(labelled)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/"
cces2020Filename <- "CCES/Original/CCES20.dta"
cces2020CodedFilename <- "CCES/Coded/CCES20Coded.rds"

# import 2020 cces file ----
cces2020 <- haven::read_dta(file = file.path(dataPath, cces2020Filename))

## assign display names of relevant variables for 2020 ----
relevantVariables2020 <- c(
  "Weight" = "commonweight",
  "End Time" = "endtime",
  "Age" = "birthyr",
  "White" = "race",
  "Black" = "race",
  "Hispanic" = "race",
  "Male" = "gender",
  "Education" = "educ",
  "Income" = "faminc_new",
  "Immigrant" = "immstat",
  "State" = "inputstate",
  "County" = "countyfips",
  "Rurality" = "urbancity",
  "Democrat" = "pid7",
  "Liberal" = "ideo5",
  "Republican" = "pid7",
  "Conservative" = "ideo5",
  "Presidential Approval" = "CC20_320a",
  "Recent Incumbent Vote" = "presvote16post",
  "Grant Legal Status" = "CC20_331a",
  "Increase Patrols" = "CC20_331b",
  "Withold Federal Funds" = "CC20_331c",
  "Eliminate Lottery" = "CC20_331d",
  "Increase Spending" = "CC20_331e",
  "Dreamer Residence" = "CC20_350e"
)

## initialize coded cces data frame ----
cces2020Coded <- cces2020

## recode questionnaire variables ----
cces2020Coded <- cces2020Coded |>
  dplyr::mutate(Weight = commonweight, `End Time` = as.POSIXct(endtime))

## recode demographic variables ----
cces2020Coded <- cces2020Coded |>
  dplyr::mutate(Age = 2020 - birthyr) |>
  recodeBinary(
    columnName = "race",
    newColumnName = "White",
    qualifyingValues = 1,
    disqualifyingValues = 2:8
  ) |>
  recodeBinary(
    columnName = "race",
    newColumnName = "Black",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:8)
  ) |>
  recodeBinary(
    columnName = "race",
    newColumnName = "Hispanic",
    qualifyingValues = 3,
    disqualifyingValues = c(1:2, 4:8)
  ) |>
  recodeBinary(
    columnName = "gender",
    newColumnName = "Male",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeScale(
    columnName = "educ",
    newColumnName = "Education",
    scaleMax = 6,
    scaleMin = 1
  ) |>
  recodeScale(
    columnName = "faminc_new",
    newColumnName = "Income",
    scaleMax = 16,
    scaleMin = 1
  ) |>
  recodeBinary(
    columnName = "immstat",
    newColumnName = "Immigrant",
    qualifyingValues = 1:2,
    disqualifyingValues = 3:5
  )

## recode geographic variables ----
cces2020Coded <- cces2020Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips) |>
  dplyr::mutate(Rurality = haven::as_factor(urbancity))

## recode political variables ----
cces2020Coded <- cces2020Coded |>
  recodeBinary(
    columnName = "pid7",
    newColumnName = "Democrat",
    qualifyingValues = 1:3,
    disqualifyingValues = 4:9
  ) |>
  recodeBinary(
    columnName = "ideo5",
    newColumnName = "Liberal",
    qualifyingValues = 1:2,
    disqualifyingValues = 3:6
  ) |>
  recodeBinary(
    columnName = "pid7",
    newColumnName = "Republican",
    qualifyingValues = 5:7,
    disqualifyingValues = c(1:4, 8:9)
  ) |>
  recodeBinary(
    columnName = "ideo5",
    newColumnName = "Conservative",
    qualifyingValues = 4:5,
    disqualifyingValues = c(1:3, 6)
  ) |>
  recodeScale(
    columnName = "CC20_320a",
    newColumnName = "Presidential Approval",
    scaleMax = 4,
    scaleMin = 1,
    reverse = TRUE
  ) |>
  recodeBinary(
    columnName = "presvote16post",
    newColumnName = "Recent Incumbent Vote",
    qualifyingValues = 1,
    disqualifyingValues = c(2:7)
  )

## recode immigration variables ----
cces2020Coded <- cces2020Coded |>
  recodeBinary(
    columnName = "CC20_331a",
    newColumnName = "Grant Legal Status",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_331b",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_331c",
    newColumnName = "Withold Federal Funds",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_331d",
    newColumnName = "Eliminate Lottery",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_331e",
    newColumnName = "Increase Spending",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_350e",
    newColumnName = "Dreamer Residence",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## select recoded variables ----
cces2020Coded <- cces2020Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2020))) |>
  dplyr::mutate(Year = "2020", .before = 1)

## save coded cces ----
cces2020Coded |> saveRDS(file = file.path(dataPath, cces2020CodedFilename))
