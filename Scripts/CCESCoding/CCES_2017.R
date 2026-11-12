
# Recode 2017 CCES Responses

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
cces2017Filename <- "CCES/Original/CCES17.dta"
cces2017CodedFilename <- "CCES/Coded/CCES17Coded.rds"

# import 2017 cces file ----
cces2017 <- haven::read_dta(file = file.path(dataPath, cces2017Filename))

## initialize coded cces data frame ----
cces2017Coded <- cces2017

## recode questionnaire variables ----
cces2017Coded <- cces2017Coded |>
  dplyr::mutate(Weight = weights_common, `End Time` = as.POSIXct(endtime))

## recode demographic variables ----
cces2017Coded <- cces2017Coded |>
  dplyr::mutate(Age = 2017 - birthyr) |>
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
cces2017Coded <- cces2017Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate))

## recode political identity variables ----
cces2017Coded <- cces2017Coded |>
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
cces2017Coded <- cces2017Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC17_322a",
    newColumnName = "Presidential Approval",
    scaleMax = 4,
    scaleMin = 1,
    reverse = TRUE
  ) |>
  recodeBinary(
    columnName = "CC17_327",
    newColumnName = "Republican President Vote",
    qualifyingValues = 1,
    disqualifyingValues = 2:5
  ) |>
  recodeBinary(
    columnName = "CC17_327",
    newColumnName = "Democrat President Vote",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:5)
  ) |>
  recodeBinary(
    columnName = "CC17_304_12",
    newColumnName = "Protested",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_304_13",
    newColumnName = "Contacted Official",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_304_14",
    newColumnName = "Volunteered",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_304_15",
    newColumnName = "Donated Money",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## recode immigration variables ----
cces2017Coded <- cces2017Coded |>
  recodeBinary(
    columnName = "CC17_331_1",
    newColumnName = "Legal Status Amnesty",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_331_2",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_331_3",
    newColumnName = "Police Questioning",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_331_4",
    newColumnName = "Fine Businesses",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_331_5",
    newColumnName = "Identify and Deport",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_331_6",
    newColumnName = "Increase Visas",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_331_7",
    newColumnName = "Build a Wall",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_331_8",
    newColumnName = "Require Local Reporting",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_340E",
    newColumnName = "Reentry Detention",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_340G",
    newColumnName = "Withold Federal Funds",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_341A",
    newColumnName = "Ban Muslim Countries",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC17_341B",
    newColumnName = "Increase Deportations",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2017 ----
relevantVariables2017 <- c(
  "Weight" = "weights_common",
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
  "Democrat" = "pid7",
  "Liberal" = "ideo5",
  "Republican" = "pid7",
  "Conservative" = "ideo5",
  "Registered Voter" = "votereg",
  "Presidential Approval" = "CC17_322a",
  "Republican President Vote" = "CC17_327",
  "Democrat President Vote" = "CC17_327",
  "Protested" = "CC17_304_12",
  "Contacted Official" = "CC17_304_13",
  "Volunteered" = "CC17_304_14",
  "Donated Money" = "CC17_304_15",
  "Legal Status Amnesty" = "CC17_331_1",
  "Increase Patrols" = "CC17_331_2",
  "Police Questioning" = "CC17_331_3",
  "Fine Businesses" = "CC17_331_4",
  "Identify and Deport" = "CC17_331_5",
  "Increase Visas" = "CC17_331_6",
  "Build a Wall" = "CC17_331_7",
  "Require Local Reporting" = "CC17_331_8",
  "Reentry Detention" = "CC17_340E",
  "Withold Federal Funds" = "CC17_340G",
  "Ban Muslim Countries" = "CC17_341A",
  "Increase Deportations" = "CC17_341B"
)

## select recoded variables ----
cces2017Coded <- cces2017Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2017))) |>
  dplyr::mutate(Year = "2017", .before = 1)

## save coded cces ----
cces2017Coded |> saveRDS(file = file.path(dataPath, cces2017CodedFilename))
