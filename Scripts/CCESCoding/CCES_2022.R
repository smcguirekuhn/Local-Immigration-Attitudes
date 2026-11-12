
# Recode 2022 CCES Responses

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
cces2022Filename <- "CCES/Original/CCES22.dta"
cces2022CodedFilename <- "CCES/Coded/CCES22Coded.rds"

# import 2022 cces file ----
cces2022 <- haven::read_dta(file = file.path(dataPath, cces2022Filename))

## initialize coded cces data frame ----
cces2022Coded <- cces2022

## recode questionnaire variables ----
cces2022Coded <- cces2022Coded |>
  dplyr::mutate(Weight = commonweight, `End Time` = as.POSIXct(endtime))

## recode demographic variables ----
cces2022Coded <- cces2022Coded |>
  dplyr::mutate(Age = 2022 - birthyr) |>
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
cces2022Coded <- cces2022Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips) |>
  recodeScale(
    columnName = "urbancity",
    newColumnName = "Rurality",
    scaleMin = 1,
    scaleMax = 4
  )

## recode political identity variables ----
cces2022Coded <- cces2022Coded |>
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
cces2022Coded <- cces2022Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC22_320a",
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
    columnName = "CC22_430a_1",
    newColumnName = "Attended Meetings",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_430a_2",
    newColumnName = "Put Up Sign",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_430a_3",
    newColumnName = "Worked for Candidate",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_430a_4",
    newColumnName = "Protested",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_430a_5",
    newColumnName = "Contacted Official",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_430a_6",
    newColumnName = "Donated Money",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## recode immigration variables ----
cces2022Coded <- cces2022Coded |>
  recodeBinary(
    columnName = "CC22_331a",
    newColumnName = "Legal Status Amnesty",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_331b",
    newColumnName = "Increase Patrols",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_331c",
    newColumnName = "Eliminate Lottery",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC22_331d",
    newColumnName = "Increase Spending Plus Wall",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2022 ----
relevantVariables2022 <- c(
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
  "Presidential Approval" = "CC22_320a",
  "Republican President Vote" = "presvote20post",
  "Democrat President Vote" = "presvote20post",
  "Attended Meetings" = "CC22_430a_1",
  "Put Up Sign" = "CC22_430a_2",
  "Worked for Candidate" = "CC22_430a_3",
  "Protested" = "CC22_430a_4",
  "Contacted Official" = "CC22_430a_5",
  "Donated Money" = "CC22_430a_6",
  "Legal Status Amnesty" = "CC22_331a",
  "Increase Patrols" = "CC22_331b",
  "Eliminate Lottery" = "CC22_331c",
  "Increase Spending Plus Wall" = "CC22_331d"
)

## select recoded variables ----
cces2022Coded <- cces2022Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2022))) |>
  dplyr::mutate(Year = "2022", .before = 1)

## save coded cces ----
cces2022Coded |> saveRDS(file = file.path(dataPath, cces2022CodedFilename))
