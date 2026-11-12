
# Script 01: Compile and Describe CCES Datasets

# reset global environment ----
rm(list = ls())

# import packages ----
library(haven)
library(purrr)
library(dplyr)
library(ggplot2)

# source helper functions ----
list.files(path = "./Functions", full.names = TRUE) |> purrr::walk(.f = source)

# assign import and export destinations ----
dataPath <- "./Data/"
ccesCodedPath <- "CCES/Coded"
ccesCombinedFilename <- "CCES/CCESCombined.rds"

# combine cces data frames ----
ccesCombined <- list.files(
  path = file.path(dataPath, ccesCodedPath),
  full.names = TRUE
) |> purrr::map_dfr(.f = \(ccesFile) readRDS(file = ccesFile))

# save combined cces data ---
ccesCombined |> saveRDS(file = file.path(dataPath, ccesCombinedFilename))

immigrationQuestions <- list(
  `Legal Status Amnesty` = list(
    Wordings = "Grant legal status to all illegal immigrants who have held jobs 
    and paid taxes for at least 3 years, and not been convicted of any felony crimes.",
    Years = c(2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
  ),
  `Increase Patrols` = list(
    Wordings = "Increase the number of border patrols on the US-Mexican border.",
    Years = c(2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
  ),
  `Legal Status for Dreamers` = list(
    Wordings = c(
      "Provide legal status to children of immigrants who were already in the United States
      and were brought to the United States by their parents. Provide these children the option of 
      citizenship in 10 years if they meet citizenship requirements and commit no crimes (DACA).",
      
      "Provide permanent resident status to children of immigrants who were brought to 
      the United States by their parents (also known as Dreamers). Provide these immigrants a 
      pathway to citizenship if they meet the citizenship requirements and commit no crimes."
    ),
    Years = c(2018, 2019, 2020, 2021, 2023, 2024, 2025)
  ),
  `Build a Wall` = list(
    Wordings = "Build a wall between the US and Mexico.",
    Years = c(2017, 2021, 2023, 2024, 2025)
  ),
  `Increase Spending Plus Wall` = list(
    Wordings = "Increase spending on border security by $25 billion, 
    including building a wall between the US and Mexico.",
    Years = c(2018, 2020, 2022)
  ),
  `Withold Federal Funds` = list(
    Wordings = c(
      "Withholds federal funds from states and localities that do not follow federal immigration laws.",
      
      "Withhold federal funds from any local police department that does not report to the 
      federal government anyone they identify as an illegal immigrant."
    ),
    Years = c(2017, 2018, 2019, 2020, 2021)
  ),
  `Eliminate Lottery` = list(
    Wordings = c(
      "Reduce legal immigration by eliminating the visa lottery and ending family-based migration.",
      "Reduce legal immigration by 50 percent over the next 10 years by eliminating the 
      visa lottery and ending family-based migration."
    ),
    Years = c(2018, 2020, 2022)
  ),
  `Ban Muslim Countries` = list(
    Wordings = c(
      "Ban individuals from 12 countries, including Afghanistan, Iran, Libya, Somalia, Sudan, and Yemen 
      from entering the United States, while partially restricting entry from 7 additional countries.",
      
      "Ban immigrants from Iran, Somalia, Sudan, Yemen, Syria, and Libya from coming to the 
      United States for 90 days. Permanently prohibits Syrian refugees from entering country."
    ),
    Years = c(2017, 2018, 2025)
  ),
  `Reentry Detention` = list(
    Wordings = c(
      "Increases criminal penalties for individuals in the country illegally who are 
      convicted of certain crimes, deported, and then re-enter the U.S. illegally.",
      
      "Send to prison any person who has been deported from the 
      United States and reenters the United States."
    ),
    Years = c(2017, 2018)
  ),
  `Police Questioning` = list(
    Wordings = "Allow police to question anyone they think may be in the country illegally.",
    Years = 2017
  ),
  `Require Local Reporting` = list(
    Wordings = "Require local police departments to report to the federal government anyone 
    they identify as an illegal immigrant.",
    Years = 2017
  ),
  `Fine Businesses` = list(
    Wordings = "Fine U.S. businesses that hire illegal immigrants.",
    Years = 2017
  ),
  `Increase Visas` = list(
    Wordings = "Increase the number of visas for overseas workers to work in the U.S.",
    Years = 2017
  ),
  `End Birthright` = list(
    Wordings = "End birthright citizenship, or automatic citizenship for infants born in the 
    United States to parents who are in the country illegally or on temporary visas.",
    Years = 2025
  ),
  `Deny Asylum` = list(
    Wordings = "Deny access to asylum for immigrants who cross the US-Mexico border illegally",
    Years = 2024
  ),
  `Identify and Deport` = list(
    Wordings = "Identify and deport illegal immigrants.",
    Years = 2017
  ),
  `Require Detention for Theft` = list(
    Wordings = "Require detention of undocumented immigrants charged with theft-related crimes and 
    allow states to sue the federal government for immigration failures.",
    Years = 2025
  ),
  `Dreamers Plus Build a Wall Plus Lottery` = list(
    Wordings = "Grant legal status to DACA children, spend $25 billion on construction of the border wall, 
    and reduce legal immigration by eliminating the visa lottery and ending family-based migration.",
    Years = 2018
  ),
  `Increase Deportations` = list(
    Wordings = "Calls for immigration laws to be followed without exemption, including 
    detention by local and state police, for any 'classes or categories of removable aliens'.",
    Years = 2017
  ),
  `Reduce Legal Immigration` = list(
    Wordings = "Reduce legal immigration by 50 percent.",
    Years = 2019
  ),
  `Overturn Wall Funds` = list(
    Wordings = "Overturn President Trump’s order to use $6 billion of defense funds 
    to pay for the construction of a wall.",
    Years = 2019
  )
)

ccesCombined |>
  dplyr::group_by(Year, Democrat) |>
  dplyr::summarise(`Mean Support` = mean(`Increase Patrols`, na.rm = TRUE)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = `Mean Support`, group = factor(Democrat))) +
  ggplot2::geom_line(mapping = ggplot2::aes(color = factor(Democrat))) +
  ggplot2::labs(title = "Support for Increasing Border Patrols", color = "Democrats") +
  ggplot2::theme_minimal()
