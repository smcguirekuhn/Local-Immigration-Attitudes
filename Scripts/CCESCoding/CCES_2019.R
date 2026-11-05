
# Recode 2019 CCES Responses

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
cces2019Filename <- "CCES/Original/CCES19.dta"
cces2019CodedFilename <- "CCES/Coded/CCES19Coded.rds"

# import 2019 cces file ----
cces2019 <- haven::read_dta(file = file.path(dataPath, cces2019Filename))

## assign display names of relevant variables for 2019 ----
relevantVariables2019 <- c(
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
  "Democrat" = "pid7",
  "Liberal" = "ideo5",
  "Republican" = "pid7",
  "Conservative" = "ideo5",
  "Presidential Approval" = "CC19_308a",
  "Recent Incumbent Vote" = "presvote16post",
  "Grant Legal Status" = "CC19_322d",
  "Increase Patrols" = "CC19_322e",
  "Overturn Wall Funds" = "CC19_322a",
  "Withold Federal Funds" = "CC19_322b",
  "Reduce by Half" = "CC19_322c",
  "Dreamer Residence" = "CC19_328e"
)

## initialize coded cces data frame ----
cces2019Coded <- cces2019

## recode questionnaire variables ----
cces2019Coded <- cces2019Coded |>
  dplyr::mutate(Weight = commonweight, `End Time` = as.POSIXct(endtime))

## recode demographic variables ----
cces2019Coded <- cces2019Coded |>
  dplyr::mutate(Age = 2019 - birthyr) |>
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
cces2019Coded <- cces2019Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips)

## recode political variables ----
cces2019Coded <- cces2019Coded |>
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
    columnName = "CC19_308a",
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
cces2019Coded <- cces2019Coded |>
  recodeBinary(
    columnName = "CC19_322d",
    newColumnName = "Grant Legal Status",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC19_322e",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC19_322a",
    newColumnName = "Overturn Wall Funds",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC19_322b",
    newColumnName = "Withold Federal Funds",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC19_322c",
    newColumnName = "Reduce by Half",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC19_328e",
    newColumnName = "Dreamer Residence",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## select recoded variables ----
cces2019Coded <- cces2019Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2019))) |>
  dplyr::mutate(Year = "2019", .before = 1)

## save coded cces ----
cces2019Coded |> saveRDS(file = file.path(dataPath, cces2019CodedFilename))
