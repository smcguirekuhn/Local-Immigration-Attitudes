
# Recode 2016 CCES Responses

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
cces2016Filename <- "CCES/Original/CCES16.dta"
cces2016CodedFilename <- "CCES/Coded/CCES16Coded.rds"

# import 2016 cces file ----
cces2016 <- haven::read_dta(file = file.path(dataPath, cces2016Filename))

## initialize coded cces data frame ----
cces2016Coded <- cces2016

## recode questionnaire variables ----
cces2016Coded <- cces2016Coded |>
  dplyr::mutate(Weight = commonweight, `End Time` = as.POSIXct(endtime_pre))

## recode demographic variables ----
cces2016Coded <- cces2016Coded |>
  dplyr::mutate(Age = 2016 - birthyr) |>
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
  dplyr::mutate(faminc = ifelse(test = faminc == 31, yes = 17, no = faminc)) |>
  recodeScale(
    columnName = "faminc",
    newColumnName = "Income",
    scaleMax = 17,
    scaleMin = 1
  ) |>
  recodeBinary(
    columnName = "immstat",
    newColumnName = "Immigrant",
    qualifyingValues = 1:2,
    disqualifyingValues = 3:5
  )

## recode geographic variables ----
cces2016Coded <- cces2016Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate))

## recode political identity variables ----
cces2016Coded <- cces2016Coded |>
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
cces2016Coded <- cces2016Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC16_320a",
    newColumnName = "Presidential Approval",
    scaleMax = 4,
    scaleMin = 1,
    reverse = TRUE
  ) |>
  recodeBinary(
    columnName = "CC16_326",
    newColumnName = "Republican President Vote",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:5)
  ) |>
  recodeBinary(
    columnName = "CC16_326",
    newColumnName = "Democrat President Vote",
    qualifyingValues = 1,
    disqualifyingValues = 2:5
  ) |>
  recodeBinary(
    columnName = "CC16_417a_1",
    newColumnName = "Attended Meetings",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_417a_2",
    newColumnName = "Put Up Sign",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_417a_3",
    newColumnName = "Worked for Candidate",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_417a_4",
    newColumnName = "Donated Money",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## recode immigration variables ----
cces2016Coded <- cces2016Coded |>
  recodeBinary(
    columnName = "CC16_331_1",
    newColumnName = "Legal Status Amnesty",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_331_2",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_331_3",
    newColumnName = "Legal Status for Dreamers",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_331_4",
    newColumnName = "Fine Businesses",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_331_5",
    newColumnName = "Ban Muslim Countries",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_331_6",
    newColumnName = "Increase Visas",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_331_7",
    newColumnName = "Identify and Deport",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC16_331_8",
    newColumnName = "Ban Muslims",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2016 ----
relevantVariables2016 <- c(
  "Weight" = "commonweight",
  "End Time" = "endtime_pre",
  "Age" = "birthyr",
  "White" = "race",
  "Black" = "race",
  "Hispanic" = "race",
  "Male" = "gender",
  "Education" = "educ",
  "Income" = "faminc",
  "Immigrant" = "immstat",
  "State" = "inputstate",
  "Democrat" = "pid7",
  "Liberal" = "ideo5",
  "Republican" = "pid7",
  "Conservative" = "ideo5",
  "Registered Voter" = "votereg",
  "Presidential Approval" = "CC16_320a",
  "Republican President Vote" = "CC16_326",
  "Democrat President Vote" = "CC16_326",
  "Attended Meetings" = "CC16_417a_1",
  "Put Up Sign" = "CC16_417a_2",
  "Worked for Candidate" = "CC16_417a_3",
  "Donated Money" = "CC16_417a_4",
  "Legal Status Amnesty" = "CC16_331_1",
  "Increase Patrols" = "CC16_331_2",
  "Legal Status for Dreamers" = "CC16_331_3",
  "Fine Businesses" = "CC16_331_4",
  "Ban Muslim Countries" = "CC16_331_5",
  "Increase Visas" = "CC16_331_6",
  "Identify and Deport" = "CC16_331_7",
  "Ban Muslims" = "CC16_331_8"
)

## select recoded variables ----
cces2016Coded <- cces2016Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2016))) |>
  dplyr::mutate(Year = "2016", .before = 1)

## save coded cces ----
cces2016Coded |> saveRDS(file = file.path(dataPath, cces2016CodedFilename))
