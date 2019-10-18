#' Maps Coded Variable Names to Human Readable Names
#'
#' @description This function - specific to this analysis - changes coded
#' variable names to be human readable using a predefined look-up.
#' @param df A dataframe with a variable field.
#'
#' @return A datafame with human readable variable entries.
#' @export
#' @importFrom dplyr mutate case_when
#' @examples
#'
#' ## Code
#' map_variable_names
map_variable_names <- function(df) {

  dplyr::mutate(df, variable = case_when(
    variable %in% "year" ~ "Year",
    variable %in% "sex" ~ "Sex",
    variable %in% "age" ~ "Age",
    variable %in% "phec" ~ "Public Health England Centre",
    variable %in% "occat" ~ "Occupation",
    variable %in% "ethgrp" ~ "Ethnic group",
    variable %in% "ukborn" ~ "UK birth status",
    variable %in% "timesinceent" ~ "Time since entry",
    variable %in% "symptonset" ~ "Date of symptom onset",
    variable %in% "datediag" ~ "Date of diagnosis",
    variable %in% "startedtreat" ~ "Started treatment",
    variable %in% "starttreatdate" ~ "Date of starting treatment",
    variable %in% "txenddate" ~ "Treatment end date",
    variable %in% "pulmextrapulm" ~ "Pulmonary or extra-pulmonary TB",
    variable %in% "culture" ~ "Culture",
    variable %in% "startedtreat" ~ "Started treatment",
    variable %in% "sputsmear" ~ "Sputum smear status",
    variable %in% "anyres" ~ "Drug resistance",
    variable %in% "prevdiag" ~ "Previous diagnosis",
    variable %in% "bcgvacc" ~ "BCG status",
    variable %in% "bcgvaccyr" ~ "Year of BCG vaccination",
    variable %in% "overalloutcome" ~ "Overall outcome",
    variable %in% "tomdeathrelat" ~ "Cause of death",
    variable %in% "natquintile" ~ "Socio-economic status (quintiles)",
    variable %in% "dateofdeath" ~ "Date of death",
    variable %in% "caserepdate" ~ "Date of notification",
  ))
}
