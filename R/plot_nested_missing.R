#' Plot Nested Missing Data
#'
#'
#' @description This function plots nested missing data using a slightly altered version of
#' \code{\link[naniar]{vis_miss}}.
#' @param df A dataframe as produced by \code{\link[ETSMissing]{account_for_nested_missing}}
#' @param down_sample Numeric, the fraction by which to downsample the data. Defaults to 0.2.
#'
#' @return A plot of nested missingness
#' @export
#' @importFrom dplyr sample_frac arrange
#' @importFrom naniar vis_miss
#' @importFrom ggplot2 coord_flip theme element_text
#' @seealso account_for_nested_missing
#' @examples
#'
#' ## Code
#' plot_nested_missing
plot_nested_missing <- function(df, down_sample = 0.2) {

  df %>%
    sample_frac(down_sample) %>%
    arrange(desc(caserepdate)) %>%
    vis_miss(warn_large_data = FALSE, sort_miss = TRUE,
             show_perc = TRUE, cluster = FALSE) +
    coord_flip() +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          text = element_text(size = 22))
}
