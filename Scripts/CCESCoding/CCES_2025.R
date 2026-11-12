
# Recode 2025 CCES Responses

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
cces2025Filename <- "CCES/Original/CCES25.dta"
cces2025CodedFilename <- "CCES/Coded/CCES25Coded.rds"

# import 2025 cces file ----
cces2025 <- haven::read_dta(file = file.path(dataPath, cces2025Filename))

## initialize coded cces data frame ----
cces2025Coded <- cces2025

## recode questionnaire variables ----
cces2025Coded <- cces2025Coded |>
  dplyr::mutate(
    Weight = commonweight,
    `End Time` = as.POSIXct(endtime/1000, origin = "1960-01-01", tz = "UTC")
  )

## recode demographic variables ----
cces2025Coded <- cces2025Coded |>
  dplyr::mutate(Age = 2025 - birthyr) |>
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
cces2025Coded <- cces2025Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips) |>
  recodeScale(
    columnName = "urbancity",
    newColumnName = "Rurality",
    scaleMin = 1,
    scaleMax = 4
  )

## recode political identity variables ----
cces2025Coded <- cces2025Coded |>
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
cces2025Coded <- cces2025Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC25_312a",
    newColumnName = "Presidential Approval",
    scaleMax = 4,
    scaleMin = 1,
    reverse = TRUE
  ) |>
  recodeBinary(
    columnName = "presvote20post",
    newColumnName = "Republican President Vote",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:9)
  ) |>
  recodeBinary(
    columnName = "presvote20post",
    newColumnName = "Democrat President Vote",
    qualifyingValues = 1,
    disqualifyingValues = 2:9
  )

## recode immigration variables ----
cces2025Coded <- cces2025Coded |>
  recodeBinary(
    columnName = "CC25_323a",
    newColumnName = "Legal Status Amnesty",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC25_323b",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC25_323c",
    newColumnName = "Build a Wall",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC25_323d",
    newColumnName = "Legal Status for Dreamers",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC25_340c",
    newColumnName = "Require Detention for Theft",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC25_342a",
    newColumnName = "End Birthright",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC25_342b",
    newColumnName = "Ban Muslim Countries",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2025 ----
relevantVariables2025 <- c(
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
  "Presidential Approval" = "CC25_312a",
  "Republican President Vote" = "presvote24post",
  "Democrat President Vote" = "presvote24post",
  "Legal Status Amnesty" = "CC25_323a",
  "Increase Patrols" = "CC25_323b",
  "Build a Wall" = "CC25_323c",
  "Legal Status for Dreamers" = "CC25_323d",
  "Require Detention for Theft" = "CC25_340c",
  "End Birthright" = "CC25_342a",
  "Ban Muslim Countries" = "CC25_342b"
)

## select recoded variables ----
cces2025Coded <- cces2025Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2025))) |>
  dplyr::mutate(Year = "2025", .before = 1)

## save coded cces ----
cces2025Coded |> saveRDS(file = file.path(dataPath, cces2025CodedFilename))
