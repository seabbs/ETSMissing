#' Summarise Missingness by Variable
#'
#'
#' @description This function filters the ETS so that only entries that could have been missing are
#' evaluated for each variable.
#' @inheritParams account_for_nested_missing
#' @return A named list summarising missing data for the following variables: date of starting treatment,
#' date of ending treatment, date of death, and cause of death.
#' @export
#'
#' @importFrom dplyr filter count add_count filter mutate pull summarise
#' @importFrom prettypublisher pretty_percentage
#' @examples
#'
#' ## Code
#' summarise_missingness
summarise_missingness <- function(df) {
  ## Started treatment date
  date_treatment <- df %>%
    filter(startedtreat %in% "Started") %>%
    count(starttreatdate) %>%
    add_count(wt = n, name = "nn") %>%
    filter(is.na(starttreatdate)) %>%
    mutate(per = pretty_percentage(n, nn, 1)) %>%
    pull(per)

  ## Ended treatment
  date_treatment_end <- df %>%
    filter(startedtreat %in% "Started") %>%
    filter(overalloutcome %in% "Treatment completed") %>%
    count(txenddate) %>%
    add_count(wt = n, name = "nn") %>%
    filter(is.na(txenddate)) %>%
    mutate(per = pretty_percentage(n, nn, 1)) %>%
    pull(per)

  ## Date of death
  date_death_missing <- df %>%
    filter(overalloutcome == "Died") %>%
    summarise(missing = sum(is.na(dateofdeath)), n = n()) %>%
    mutate(per = pretty_percentage(missing, n, 1)) %>%
    pull(per)

  ## Cause of death proportion missing
  tomdeathrelat_missing <- df %>%
    filter(overalloutcome == "Died") %>%
    count(tomdeathrelat) %>%
    add_count(wt = n, name = "nn") %>%
    mutate(pretty_per = pretty_percentage(n, nn, 1)) %>%
    filter(is.na(tomdeathrelat)) %>%
    pull(pretty_per)

  missing_stats <- c(date_treatment, date_death_missing, tomdeathrelat_missing,
                     date_treatment_end)
  names(missing_stats) <- c("date_treat", "date_death", "cause_death", "date_treat_end")

  return(missing_stats)
}
