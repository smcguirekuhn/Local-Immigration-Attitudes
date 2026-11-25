
# Recode 2015 CCES Responses

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
cces2015Filename <- "CCES/Original/CCES15.dta"
cces2015CodedFilename <- "CCES/Coded/CCES15Coded.rds"

# import 2015 cces file ----
cces2015 <- haven::read_dta(file = file.path(dataPath, cces2015Filename))

## initialize coded cces data frame ----
cces2015Coded <- cces2015

## recode questionnaire variables ----
cces2015Coded <- cces2015Coded |>
  dplyr::mutate(Weight = weight, `End Time` = as.POSIXct(endtime))

## recode demographic variables ----
cces2015Coded <- cces2015Coded |>
  dplyr::mutate(Age = 2015 - birthyr) |>
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
  dplyr::mutate(
    faminc = dplyr::case_when(
      faminc == 31 ~ 17,
      faminc == 32 ~ 18,
      .default = faminc
    )
  ) |>
  recodeScale(
    columnName = "faminc",
    newColumnName = "Income",
    scaleMax = 18,
    scaleMin = 1
  )

## recode geographic variables ----
cces2015Coded <- cces2015Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips)

## recode political identity variables ----
cces2015Coded <- cces2015Coded |>
  recodeBinary(
    columnName = "pid7",
    newColumnName = "Democrat",
    qualifyingValues = 1:3,
    disqualifyingValues = 4:8
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
    disqualifyingValues = c(1:4, 8)
  ) |>
  recodeBinary(
    columnName = "ideo5",
    newColumnName = "Conservative",
    qualifyingValues = 4:5,
    disqualifyingValues = c(1:3, 6)
  )

## recode political participation variables ----
cces2015Coded <- cces2015Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC15_312a",
    newColumnName = "Presidential Approval",
    scaleMax = 4,
    scaleMin = 1,
    reverse = TRUE
  ) |>
  recodeBinary(
    columnName = "CC15_315",
    newColumnName = "Republican President Vote",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:5)
  ) |>
  recodeBinary(
    columnName = "CC15_315",
    newColumnName = "Democrat President Vote",
    qualifyingValues = 1,
    disqualifyingValues = 2:5
  )

## recode immigration variables ----
cces2015Coded <- cces2015Coded |>
  recodeBinary(
    columnName = "CC15_321_1",
    newColumnName = "Legal Status Amnesty",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC15_321_2",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC15_321_3",
    newColumnName = "Police Questioning",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC15_321_4",
    newColumnName = "Fine Businesses",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC15_321_5",
    newColumnName = "Identify and Deport",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC15_321_6",
    newColumnName = "Increase Visas",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2015 ----
relevantVariables2015 <- c(
  "Weight" = "weight",
  "End Time" = "endtime",
  "Age" = "birthyr",
  "White" = "race",
  "Black" = "race",
  "Hispanic" = "race",
  "Male" = "gender",
  "Education" = "educ",
  "Income" = "faminc",
  "State" = "inputstate",
  "Democrat" = "pid7",
  "Liberal" = "ideo5",
  "Republican" = "pid7",
  "Conservative" = "ideo5",
  "Registered Voter" = "votereg",
  "Presidential Approval" = "CC15_312a",
  "Republican President Vote" = "CC15_315",
  "Democrat President Vote" = "CC15_315",
  "Legal Status Amnesty" = "CC15_321_1",
  "Increase Patrols" = "CC15_321_2",
  "Police Questioning" = "CC15_321_3",
  "Fine Businesses" = "CC15_321_4",
  "Identify and Deport" = "CC15_321_5",
  "Increase Visas" = "CC15_321_6"
)

## select recoded variables ----
cces2015Coded <- cces2015Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2015))) |>
  dplyr::mutate(Year = "2015", .before = 1)

## save coded cces ----
cces2015Coded |> saveRDS(file = file.path(dataPath, cces2015CodedFilename))
