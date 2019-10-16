#' Tabulate Nested Missing Data
#'
#' @description This function tabulates nested missing data using a slightly altered (for presentation) version of
#' \code{\link[naniar]{miss_var_summary}}.
#' @inheritParams plot_nested_missing
#' @return A dataframe summarising missing data
#' @export
#' @importFrom naniar miss_var_summary
#' @importFrom dplyr mutate select
#' @importFrom prettypublisher pretty_round
#' @examples
#'
#' ## Code
#' nested_missing_table
nested_missing_table <- function(df) {

  pct_miss <- NULL; Variable <- NULL; variable <- NULL;
  `Missing (N)` <- NULL; `Missing (%)` <- NULL; percent <- NULL;
  n_miss <- NULL;

  df %>%
    miss_var_summary %>%
    mutate(percent = pretty_round(pct_miss, digits = 1)) %>%
    select(Variable = variable, `Missing (N)` = n_miss, `Missing (%)` = percent)
}
