#' Account for Data that is Missing Due to Nesting
#'
#' @description This function adds entries for variables that are only missing due to nesting - rather
#' than truely being missing. Variables that are adjusted are: cause of death, date of death, starting treatment date,
#' treatment end date, year of BCG vaccination, and time since entry.
#' @param df A dataframe containing cleaned ETS data as produced by \code{tbinenglanddataclean}.
#' @return A dataframe that has nested missingness corrected for a subset of variables
#' @export
#' @importFrom dplyr select mutate
#'
#' @examples
#'
#'
#' ## Code
#' account_for_nested_missing
account_for_nested_missing <- function(df) {

  # Select variables of interest --------------------------------------------
  filt_df <- df %>%
    select(caserepdate, year, sex, age, phec,
           occat, ethgrp, ukborn, timesinceent,
           symptonset, datediag, startedtreat, starttreatdate,
           txenddate, pulmextrapulm, culture, sputsmear, anyres,
           prevdiag, bcgvacc, bcgvaccyr, overalloutcome, tomdeathrelat,
           dateofdeath, txenddate, natquintile)


  # Account for nested data -------------------------------------------------
  nest_filt_df <- filt_df %>%
    mutate(tomdeathrelat = ifelse(overalloutcome == 'Died',
                                  tomdeathrelat %>% as.character,
                                  overalloutcome %>% as.character) %>%
             factor) %>%
    mutate(dateofdeath = ifelse(overalloutcome == 'Died',
                                dateofdeath, 100000)) %>%
    mutate(starttreatdate = ifelse(startedtreat == 'Started',
                                   starttreatdate,
                                   startedtreat)) %>%
    mutate(txenddate = ifelse(overalloutcome == 'Treatment completed',
                              txenddate,
                              overalloutcome)) %>%
    mutate(bcgvaccyr = ifelse(bcgvacc == 'Yes',
                              bcgvaccyr %>% as.character,
                              bcgvacc) %>% factor)  %>%
    mutate(dateofdeath = ifelse(overalloutcome %in% "Died",
                                dateofdeath,
                                "N/A") %>%
             as.factor) %>%
    mutate(timesinceent = ifelse(ukborn %in% "UK born", "N/A", timesinceent))
}
