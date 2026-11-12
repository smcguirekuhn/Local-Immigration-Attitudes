
# Recode 2018 CCES Responses

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
cces2018Filename <- "CCES/Original/CCES18.dta"
cces2018CodedFilename <- "CCES/Coded/CCES18Coded.rds"

# import 2018 cces file ----
cces2018 <- haven::read_dta(file = file.path(dataPath, cces2018Filename))

## initialize coded cces data frame ----
cces2018Coded <- cces2018

## recode questionnaire variables ----
cces2018Coded <- cces2018Coded |>
  dplyr::mutate(
    Weight = commonweight,
    `End Time` = as.POSIXct(endtime/1000, origin = "1960-01-01", tz = "UTC")
  )

## recode demographic variables ----
cces2018Coded <- cces2018Coded |>
  dplyr::mutate(Age = 2018 - birthyr) |>
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
cces2018Coded <- cces2018Coded |>
  dplyr::mutate(State = haven::as_factor(inputstate)) |>
  dplyr::mutate(County = countyfips) |>
  recodeScale(
    columnName = "urbancity",
    newColumnName = "Rurality",
    scaleMin = 1,
    scaleMax = 4
  )

## recode political identity variables ----
cces2018Coded <- cces2018Coded |>
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
cces2018Coded <- cces2018Coded |>
  recodeBinary(
    columnName = "votereg",
    newColumnName = "Registered Voter",
    qualifyingValues = 1,
    disqualifyingValues = 2:3
  ) |>
  recodeScale(
    columnName = "CC18_308a",
    newColumnName = "Presidential Approval",
    scaleMax = 4,
    scaleMin = 1,
    reverse = TRUE
  ) |>
  recodeBinary(
    columnName = "CC18_317",
    newColumnName = "Republican President Vote",
    qualifyingValues = 1,
    disqualifyingValues = 2:5
  ) |>
  recodeBinary(
    columnName = "CC18_317",
    newColumnName = "Democrat President Vote",
    qualifyingValues = 2,
    disqualifyingValues = c(1, 3:5)
  ) |>
  recodeBinary(
    columnName = "CC18_417a_1",
    newColumnName = "Attended Meetings",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_417a_2",
    newColumnName = "Put Up Sign",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_417a_3",
    newColumnName = "Worked for Candidate",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_417a_4",
    newColumnName = "Protested",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_417a_5",
    newColumnName = "Contacted Official",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_417a_6",
    newColumnName = "Donated Money",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## recode immigration variables ----
cces2018Coded <- cces2018Coded |>
  recodeBinary(
    columnName = "CC18_322a",
    newColumnName = "Increase Spending Plus Wall",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_322b",
    newColumnName = "Legal Status for Dreamers",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_322c_new",
    newColumnName = "Eliminate Lottery",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_322d_new",
    newColumnName = "Dreamers Plus Build a Wall Plus Lottery",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_322c",
    newColumnName = "Withold Federal Funds",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_322f",
    newColumnName = "Reentry Detention",
    qualifyingValues = 1,
    disqualifyingValues = 2
  ) |>
  recodeBinary(
    columnName = "CC18_417c",
    newColumnName = "Ban Muslim Countries",
    qualifyingValues = 1,
    disqualifyingValues = 2
  )

## check new names of relevant variables for 2018 ----
relevantVariables2018 <- c(
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
  "Presidential Approval" = "CC18_308a",
  "Republican President Vote" = "CC18_317",
  "Democrat President Vote" = "CC18_317",
  "Attended Meetings" = "CC18_417a_1",
  "Put Up Sign" = "CC18_417a_2",
  "Worked for Candidate" = "CC18_417a_3",
  "Protested" = "CC18_417a_4",
  "Contacted Official" = "CC18_417a_5",
  "Donated Money" = "CC18_417a_6",
  "Increase Spending Plus Wall" = "CC18_322a",
  "Legal Status for Dreamers" = "CC18_322b",
  "Eliminate Lottery" = "CC18_322c_new",
  "Dreamers Plus Build a Wall Plus Lottery" = "CC18_322d_new",
  "Withold Federal Funds" = "CC18_322c",
  "Reentry Detention" = "CC18_322f",
  "Ban Muslim Countries" = "CC18_417c"
)

## select recoded variables ----
cces2018Coded <- cces2018Coded |>
  dplyr::select(dplyr::all_of(names(relevantVariables2018))) |>
  dplyr::mutate(Year = "2018", .before = 1)

## save coded cces ----
cces2018Coded |> saveRDS(file = file.path(dataPath, cces2018CodedFilename))
