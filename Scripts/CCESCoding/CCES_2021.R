
# Recode 2021 CCES Responses

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
cces2021Filename <- "CCES/Original/CCES21.dta"
cces2021CodedFilename <- "CCES/Coded/CCES21Coded.rds"

# import 2021 cces file ----
cces2021 <- haven::read_dta(file = file.path(dataPath, cces2021Filename))

## initialize coded cces data frame ----
cces2021Coded <- cces2021

## recode questionnaire variables ----
cces2021Coded <- cces2021Coded |>
  dplyr::mutate(Weight = commonweight, `End Time` = as.POSIXct(endtime))

## recode demographic variables ----
cces2021Coded <- cces2021Coded |>
  dplyr::mutate(Age = 2021 - birthyr) |>
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
    columnName = "gender4",
    newColumnName = "Male",
    qualifyingValues = 1,
    disqualifyingValues = 2:4
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
cces2021Coded <- cces2021Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips) |>
  recodeScale(
    columnName = "urbancity",
    newColumnName = "Rurality",
    scaleMin = 1,
    scaleMax = 4
  )

## recode political identity variables ----
cces2021Coded <- cces2021Coded |>
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
  )

## recode political participation variables ----
cces2021Coded <- cces2021Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC21_315a",
    newColumnName = "Presidential Approval",
    scaleMax = 4,
    scaleMin = 1,
    reverse = TRUE
  ) |>
  recodeBinary(
    columnName = "presvote20post",
    newColumnName = "Republican President Vote",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:6)
  ) |>
  recodeBinary(
    columnName = "presvote20post",
    newColumnName = "Democrat President Vote",
    qualifyingValues = 1,
    disqualifyingValues = 2:6
  )

## recode immigration variables ----
cces2021Coded <- cces2021Coded |>
  recodeBinary(
    columnName = "CC21_322a",
    newColumnName = "Legal Status Amnesty",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC21_322b",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC21_322c",
    newColumnName = "Withold Federal Funds",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC21_322d",
    newColumnName = "Build a Wall",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC21_322e",
    newColumnName = "Legal Status for Dreamers",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2021 ----
relevantVariables2021 <- c(
  "Weight" = "commonweight",
  "End Time" = "endtime",
  "Age" = "birthyr",
  "White" = "race",
  "Black" = "race",
  "Hispanic" = "race",
  "Male" = "gender4",
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
  "Registered Voter" = "votereg",
  "Presidential Approval" = "CC22_315a",
  "Republican President Vote" = "presvote20post",
  "Democrat President Vote" = "presvote20post",
  "Legal Status Amnesty" = "CC21_322a",
  "Increase Patrols" = "CC21_322b",
  "Withold Federal Funds" = "CC21_322c",
  "Build a Wall" = "CC21_322d",
  "Legal Status for Dreamers" = "CC21_322e"
)

## select recoded variables ----
cces2021Coded <- cces2021Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2021))) |>
  dplyr::mutate(Year = "2021", .before = 1)

## save coded cces ----
cces2021Coded |> saveRDS(file = file.path(dataPath, cces2021CodedFilename))
