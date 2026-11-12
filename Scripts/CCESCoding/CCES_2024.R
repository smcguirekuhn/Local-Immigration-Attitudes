
# Recode 2024 CCES Responses

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
cces2024Filename <- "CCES/Original/CCES24.dta"
cces2024CodedFilename <- "CCES/Coded/CCES24Coded.rds"

# import 2024 cces file ----
cces2024 <- haven::read_dta(file = file.path(dataPath, cces2024Filename))

## initialize coded cces data frame ----
cces2024Coded <- cces2024

## recode questionnaire variables ----
cces2024Coded <- cces2024Coded |>
  dplyr::mutate(Weight = commonweight, `End Time` = endtime)

## recode demographic variables ----
cces2024Coded <- cces2024Coded |>
  dplyr::mutate(Age = 2024 - birthyr) |>
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
cces2024Coded <- cces2024Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips) |>
  recodeScale(
    columnName = "urbancity",
    newColumnName = "Rurality",
    scaleMin = 1,
    scaleMax = 4
  )

## recode political identity variables ----
cces2024Coded <- cces2024Coded |>
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
cces2024Coded <- cces2024Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC24_312a",
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
  ) |>
  recodeBinary(
    columnName = "CC24_430a_1",
    newColumnName = "Attended Meetings",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_430a_2",
    newColumnName = "Put Up Sign",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_430a_3",
    newColumnName = "Worked for Candidate",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_430a_4",
    newColumnName = "Protested",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_430a_5",
    newColumnName = "Contacted Official",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_430a_6",
    newColumnName = "Donated Money",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## recode immigration variables ----
cces2024Coded <- cces2024Coded |>
  recodeBinary(
    columnName = "CC24_323a",
    newColumnName = "Legal Status Amnesty",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_323b",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_323c",
    newColumnName = "Build a Wall",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_323d",
    newColumnName = "Legal Status for Dreamers",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC24_340f",
    newColumnName = "Deny Asylum",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2024 ----
relevantVariables2024 <- c(
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
  "Presidential Approval" = "CC24_312a",
  "Republican President Vote" = "presvote20post",
  "Democrat President Vote" = "presvote20post",
  "Attended Meetings" = "CC24_430a_1",
  "Put Up Sign" = "CC24_430a_2",
  "Worked for Candidate" = "CC24_430a_3",
  "Protested" = "CC24_430a_4",
  "Contacted Official" = "CC24_430a_5",
  "Donated Money" = "CC24_430a_6",
  "Legal Status Amnesty" = "CC24_323a",
  "Increase Patrols" = "CC24_323b",
  "Build a Wall" = "CC24_323c",
  "Legal Status for Dreamers" = "CC24_323d",
  "Deny Asylum" = "CC24_340f"
)

## select recoded variables ----
cces2024Coded <- cces2024Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2024))) |>
  dplyr::mutate(Year = "2024", .before = 1)

## save coded cces ----
cces2024Coded |> saveRDS(file = file.path(dataPath, cces2024CodedFilename))
