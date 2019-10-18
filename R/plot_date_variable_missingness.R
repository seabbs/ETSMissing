#' Plot Missingness for a Date Variable
#'
#' @param var A character string indicating the name of the date variable
#' to explore missingness within.
#' @param start_year Numeric, indicating which year to start including data from (inclusive)
#' @param end_year Numeric, indicating which year to stop including data from (exclusive)
#' @param facet_by_year Logical defaults to \code{TRUE}. Should the plots be facetted by a year midpoint
#' @param split_year The year to use as a splitting point when facetting. By default uses the mean year.
#' @inheritParams account_for_nested_missing
#' @return A list of plots including: missing data by month and by day.
#' @export
#'
#' @importFrom dplyr count mutate add_count filter group_by
#' @importFrom tidyr replace_na drop_na
#' @importFrom lubridate floor_date year month mday
#' @importFrom ggplot2 ggplot aes geom_violin geom_jitter scale_y_sqrt scale_x_continuous theme_minimal labs facet_wrap
#' @importFrom scales percent
#' @importFrom purrr map
#' @examples
#'
#' ## Code
#' plot_date_variable_missingness
plot_date_variable_missingness <- function(df = NULL, var = NULL,
                                           start_year = NULL, end_year = NULL,
                                           split_year = NULL,
                                           facet_by_year = TRUE) {


  year_facet <- NULL; notifications <- NULL; year_strat <- NULL;
  nn <- NULL;

  ## Add default variable for standardised programming use
  df$date <- df[[var]]


  ## Specify date filters if not given
  if (is.null(start_year)) {
    start_year <- df$date %>%
      year %>%
      min(na.rm = TRUE)
  }

  if (is.null(end_year)) {
    end_year <- df$date %>%
      year %>%
      max(na.rm = TRUE)
  }
  ## Count by day adding in zero counts when none detected
  df_count <- df %>%
    filter(year(date) >= start_year, year(date) <= end_year) %>%
    drop_na(date) %>%
    count(date, .drop = FALSE,
          name = "notifications") %>%
    mutate(notifications = notifications %>%
             replace_na(0))


  ## Add a date split variable - splitting based on them ean year
  years_of_data <- df_count$date %>%
    year() %>%
    unique() %>%
    as.numeric()


   if (is.null(split_year)) {
     split_year <- years_of_data %>%
       mean(na.rm = TRUE) %>%
       floor
   }


  df_count <- df_count %>%
    dplyr::mutate(year_strat = cut(year(date) %>% as.integer,
                                   breaks = c(min(years_of_data) - 1,
                                              split_year,
                                              max(years_of_data) + 1),
                                   labels = c(
                                     paste0(min(years_of_data), "-", split_year - 1),
                                     paste0(split_year, "-", max(years_of_data))
                                   ),
                                    right = FALSE
    ))

  if (facet_by_year) {
    df_count <- df_count %>%
      group_by(year_strat)
  }
  ## plot normalised incidence by month
  month_plot <- df_count %>%
    mutate(date = floor_date(date, "month")) %>%
    count(date, wt = notifications) %>%
    add_count(year(date),
              wt = n, name = "nn") %>%
    mutate(n = n / nn) %>%
    mutate(month = month(date, label = TRUE)) %>%
    ggplot(aes(x= month, y = n)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(alpha = 0.2) +
    scale_y_sqrt(labels = percent) +
    theme_minimal() +
    labs(x = "Month",
         y = "Percentage of annual notifications (sqrt)")

  ## Plot normalised incidencce by day
  day_plot <- df_count %>%
    mutate(date = floor_date(date, "day")) %>%
    count(date, wt = notifications) %>%
    add_count(floor_date(date, "month"),
              wt = n, name = "nn") %>%
    mutate(n = n / nn) %>%
    mutate(mday = mday(date)) %>%
    ggplot(aes(x= mday, y = n, group = mday)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(alpha = 0.05) +
    scale_y_sqrt(labels = percent) +
    scale_x_continuous(minor_breaks = NULL, breaks = seq(1, 31, 2)) +
    theme_minimal() +
    labs(x = "Day of the month",
         y = "Percentage of monthly notifications (sqrt)")


  plots <- list(month_plot, day_plot)

  if (facet_by_year) {
    plots <- plots %>%
      map(~ . + facet_wrap(~year_strat, scales = "free_y"))
  }


  names(plots) <- c("by_month", "by_day")

  return(plots)
}
