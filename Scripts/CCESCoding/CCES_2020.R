
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
  recodeScale(
    columnName = "urbancity",
    newColumnName = "Rurality",
    scaleMin = 1,
    scaleMax = 4
  )

## recode political identity variables ----
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
  )

## recode political participation variables ----
cces2020Coded <- cces2020Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
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
    newColumnName = "Republican President Vote",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:7)
  ) |>
  recodeBinary(
    columnName = "presvote16post",
    newColumnName = "Democrat President Vote",
    qualifyingValues = 1,
    disqualifyingValues = 2:7
  ) |>
  recodeBinary(
    columnName = "CC20_430a_1",
    newColumnName = "Attended Meetings",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_430a_2",
    newColumnName = "Put Up Sign",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_430a_3",
    newColumnName = "Worked for Candidate",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_430a_4",
    newColumnName = "Protested",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_430a_5",
    newColumnName = "Contacted Official",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_430a_6",
    newColumnName = "Donated Money",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## recode immigration variables ----
cces2020Coded <- cces2020Coded |>
  recodeBinary(
    columnName = "CC20_331a",
    newColumnName = "Legal Status Amnesty",
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
    newColumnName = "Increase Spending Plus Wall",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC20_350e",
    newColumnName = "Legal Status for Dreamers",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2020 ----
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
  "Registered Voter" = "votereg",
  "Presidential Approval" = "CC20_320a",
  "Republican President Vote" = "presvote16post",
  "Democrat President Vote" = "presvote16post",
  "Attended Meetings" = "CC20_430a_1",
  "Put Up Sign" = "CC20_430a_2",
  "Worked for Candidate" = "CC20_430a_3",
  "Protested" = "CC20_430a_4",
  "Contacted Official" = "CC20_430a_5",
  "Donated Money" = "CC20_430a_6",
  "Legal Status Amnesty" = "CC20_331a",
  "Increase Patrols" = "CC20_331b",
  "Withold Federal Funds" = "CC20_331c",
  "Eliminate Lottery" = "CC20_331d",
  "Increase Spending Plus Wall" = "CC20_331e",
  "Legal Status for Dreamers" = "CC20_350e"
)

## select recoded variables ----
cces2020Coded <- cces2020Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2020))) |>
  dplyr::mutate(Year = "2020", .before = 1)

## save coded cces ----
cces2020Coded |> saveRDS(file = file.path(dataPath, cces2020CodedFilename))
