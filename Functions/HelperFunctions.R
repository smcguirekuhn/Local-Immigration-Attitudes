# anes helper functions ----

## recode binary variable ----
recodeBinary <- function(
    dataFrame,
    columnName,
    newColumnName = NULL,
    qualifyingValues,
    disqualifyingValues,
    after = NULL
) {
  
  ### set new column name if applicable ----
  if (is.null(newColumnName)) newColumnName <- columnName
  
  ### recode by qualifying and disqualifying values ----
  dataFrame <- dataFrame |>
    dplyr::mutate(
      {{newColumnName}} := dplyr::case_when(
        is.na(.data[[columnName]]) ~ NA,
        .data[[columnName]] %in% qualifyingValues ~ 1,
        .data[[columnName]] %in% disqualifyingValues ~ 0,
        .default = NA
      ),
      .after = {{after}}
    )
  
  ### return data frame ----
  return(dataFrame)
}

## recode ordinal scale ----
recodeScale <- function(
    dataFrame,
    columnName,
    newColumnName = NULL,
    scaleMax,
    scaleMin = 1,
    reverse = FALSE,
    after = NULL
) {
  
  ### set new column name if applicable ----
  if (is.null(newColumnName)) newColumnName <- columnName
  
  ### scale ordinal values according to scale maximum and scale minimum ----
  dataFrame <- dataFrame |>
    dplyr::mutate(
      {{newColumnName}} := dplyr::case_when(
        is.na(.data[[columnName]]) ~ NA,
        reverse & .data[[columnName]] <= scaleMax & .data[[columnName]] >= scaleMin ~
          1 - (.data[[columnName]] - scaleMin)/(scaleMax - scaleMin),
        !reverse & .data[[columnName]] <= scaleMax & .data[[columnName]] >= scaleMin ~
          (.data[[columnName]] - scaleMin)/(scaleMax - scaleMin),
        .default = NA
      ),
      .after = {{after}}
    )
  
  ### return data frame ----
  return(dataFrame)
}

## aggregate mean of ordinal scales ----
aggregateScales <- function(
    dataFrame,
    columnNames,
    aggregateName,
    reverse = FALSE,
    after = NULL
) {
  
  ### scale ordinal values according to scale maximum and scale minimum ----
  dataFrame <- dataFrame |>
    dplyr::rowwise() |>
    dplyr::mutate(
      {{aggregateName}} := dplyr::case_when(
        anyNA(dplyr::c_across(dplyr::all_of(columnNames))) ~ NA,
        !reverse ~ mean(dplyr::c_across(dplyr::all_of(columnNames))),
        reverse ~ 1 - mean(dplyr::c_across(dplyr::all_of(columnNames)))
      ),
      .after = {{after}}
    ) |>
    dplyr::ungroup()
  
  ### return data frame ----
  return(dataFrame)
}