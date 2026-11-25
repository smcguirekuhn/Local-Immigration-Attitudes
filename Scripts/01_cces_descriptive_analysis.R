
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

# compile history of immigration question appearance on ces surveys ----
immigrationQuestions <- list(
  `Increase Patrols` = list(
    Wordings = "Increase the number of border patrols on the US-Mexican border.",
    Years = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
  ),
  `Legal Status Amnesty` = list(
    Wordings = "Grant legal status to all illegal immigrants who have held jobs 
    and paid taxes for at least 3 years, and not been convicted of any felony crimes.",
    Years = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
  ),
  `Legal Status for Dreamers` = list(
    Wordings = c(
      "Provide permanent resident status to children of immigrants who were brought to 
      the United States by their parents (also known as Dreamers). Provide these immigrants a 
      pathway to citizenship if they meet the citizenship requirements and commit no crimes 
      (2019, 2020, 2021, 2023, 2024, 2025, 2026).",
      
      "Provide legal status to children of immigrants who were already in the United States
      and were brought to the United States by their parents. Provide these children the option of 
      citizenship in 10 years if they meet citizenship requirements and commit no crimes (DACA) (2018).",
      
      "Grant legal status to people who were brought to the US illegally as children, 
      but who have graduated from a US high school (2016)."
    ),
    Years = c(2016, 2018, 2019, 2020, 2021, 2023, 2024, 2025, 2026)
  ),
  `Police Questioning` = list(
    Wordings = "Allow police to question anyone they think may be in the country illegally.",
    Years = c(2010, 2011, 2012, 2013, 2014, 2015, 2017)
  ),
  `Build a Wall` = list(
    Wordings = "Build a wall between the US and Mexico.",
    Years = c(2017, 2021, 2023, 2024, 2025, 2026)
  ),
  `Increase Spending Plus Wall` = list(
    Wordings = "Increase spending on border security by $25 billion, 
    including building a wall between the US and Mexico.",
    Years = c(2018, 2020, 2022)
  ),
  `Fine Businesses` = list(
    Wordings = c(
      "Fine US businesses that hire illegal immigrants (2012, 2013, 2014, 2015, 2016, 2017).",
      
      "Fine businesses (2010)."
    ),
    Years = c(2010, 2012, 2013, 2014, 2015, 2016, 2017)
  ),
  `Identify and Deport` = list(
    Wordings = "Identify and deport illegal immigrants.",
    Years = c(2014, 2015, 2016, 2017, 2026)
  ),
  `Withold Federal Funds` = list(
    Wordings = c(
      "Withhold federal funds from any local police department that does not report to the 
      federal government anyone they identify as an illegal immigrant (2018, 2019, 2020, 2021).",
      
      "Withholds federal funds from states and localities that do not follow federal immigration laws (2017)."
    ),
    Years = c(2017, 2018, 2019, 2020, 2021)
  ),
  `End Birthright` = list(
    Wordings = c(
      "Deny automatic citizenship to American-born children of illegal immigrants (2012, 2013).",
      
      "End birthright citizenship, or automatic citizenship for infants born in the 
      United States to parents who are in the country illegally or on temporary visas (2025, 2026)."
    ),
    Years = c(2012, 2013, 2025, 2026)
  ),
  `Ban Muslim Countries` = list(
    Wordings = c(
      "Ban immigrants from Iran, Somalia, Sudan, Yemen, Syria, and Libya from coming to the 
      United States for 90 days. Permanently prohibits Syrian refugees from entering country (2017, 2018).",
      
      "Ban individuals from 12 countries, including Afghanistan, Iran, Libya, Somalia, Sudan, and Yemen 
      from entering the United States, while partially restricting entry from 7 additional countries (2025).",
      
      "Admit no refugees from Syria (2016)."
    ),
    Years = c(2016, 2017, 2018, 2025)
  ),
  `Increase Visas` = list(
    Wordings = c(
      "Increase the number of visas for overseas workers to work in the US (2015, 2016, 2017).",
      
      "Increase the number of guest workers allowed to come legally to the US (2010)."
    ),
    Years = c(2010, 2015, 2016, 2017)
  ),
  `Eliminate Lottery` = list(
    Wordings = c(
      "Reduce legal immigration by 50 percent over the next 10 years by eliminating the 
      visa lottery and ending family-based migration (2020, 2022).",
      
      "Reduce legal immigration by eliminating the visa lottery and ending family-based migration (2018)."
    ),
    Years = c(2018, 2020, 2022)
  ),
  `Require Detention for Theft` = list(
    Wordings = "Require detention of undocumented immigrants charged with theft-related crimes and 
    allow states to sue the federal government for immigration failures.",
    Years = c(2025, 2026)
  ),
  `Reentry Detention` = list(
    Wordings = c(
      "Send to prison any person who has been deported from the United States and 
      reenters the United States (2018).",
      
      "Increases criminal penalties for individuals in the country illegally who are 
      convicted of certain crimes, deported, and then re-enter the US illegally (2017)."
    ),
    Years = c(2017, 2018)
  ),
  `Prohibit Hospital and School Usage` = list(
    Wordings = "Prohibit illegal immigrants from using emergency hospital care and public schools.",
    Years = c(2012, 2013)
  ),
  `Ban Muslims` = list(
    Wordings = "Ban Muslims from immigrating to the US.",
    Years = 2016
  ),
  `Deny Asylum` = list(
    Wordings = "Deny access to asylum for immigrants who cross the US-Mexico border illegally",
    Years = 2024
  ),
  `Overturn Wall Funds` = list(
    Wordings = "Overturn President Trump’s order to use $6 billion of defense funds 
    to pay for the construction of a wall.",
    Years = 2019
  ),
  `Reduce Legal Immigration` = list(
    Wordings = "Reduce legal immigration by 50 percent.",
    Years = 2019
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
  `Require Local Reporting` = list(
    Wordings = "Require local police departments to report to the federal government anyone 
    they identify as an illegal immigrant.",
    Years = 2017
  )
)

# create immigration question map ----
immigrationQuestions |>
  purrr::map(
    .f = \(question) {
      table(factor(question$Years, levels = 2010:2026)) |>
        tibble::enframe(name = "Year", value = "Asked") 
    }
  ) |>
  purrr::list_rbind(names_to = "Question") |>
  dplyr::mutate(
    Question = factor(Question, levels = rev(names(immigrationQuestions))),
    Asked = ifelse(test = Asked == 1, yes = "Yes", no = "No")
  ) |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = Question, fill = Asked)) +
  ggplot2::geom_tile(color = NA, linewidth = 0) +
  ggplot2::scale_fill_manual(values = c("Yes" = "#580A22", "No" = "#E1E1E1")) +
  ggplot2::labs(
    title = "Immigration Question History on the CES",
    x = "Year",
    y = "Question",
    fill = "Asked"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16))

# create figure tracking immigration policy support among democrats and republicans ----
ccesCombined |>
  dplyr::mutate(Party = ifelse(test = Democrat == 1, yes = "Democrat", no = "Republican")) |>
  dplyr::select(Year, Party, `Increase Patrols`, `Legal Status Amnesty`) |>
  tidyr::pivot_longer(
    cols = c(`Increase Patrols`, `Legal Status Amnesty`),
    names_to = "Policy",
    values_to = "Support"
  ) |>
  dplyr::group_by(Year, Party, Policy) |>
  dplyr::summarise(`Mean Support` = mean(Support, na.rm = TRUE)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = `Mean Support`, group = factor(Party))) +
  ggplot2::geom_line(mapping = ggplot2::aes(color = factor(Party), linetype = factor(Party))) +
  ggplot2::scale_color_manual(values = c("Democrat" = "#A6C8FF", "Republican" = "#580A22")) +
  ggplot2::facet_wrap(facets = ~Policy, nrow = 2) +
  ggplot2::labs(title = "Policy Support Over Time", color = "Party ID", linetype = "Party ID") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16))
