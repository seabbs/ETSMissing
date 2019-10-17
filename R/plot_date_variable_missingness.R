#' Plot Missingness for a Date Variable
#'
#' @param var A character string indicating the name of the date variable
#' to explore missingness within.
#' @param start_date A date indicating when to include data from (inclusive)
#' @param end_date A date indicating when to exclude data from (exclusive)
#' @inheritParams account_for_nested_missing
#' @return A list of plots including: missing data by month and by day.
#' @export
#'
#' @importFrom dplyr count mutate add_count filter
#' @importFrom tidyr replace_na drop_na
#' @importFrom lubridate floor_date year month mday
#' @importFrom ggplot2 ggplot aes geom_violin geom_jitter scale_y_continuous theme_minimal labs
#' @examples
#'
#' ## Code
#' plot_date_variable_missingness
plot_date_variable_missingness <- function(df = NULL, var = NULL,
                                           start_date = NULL, end_date = NULL) {


  ## Add default variable for standardised programming use
  df$date <- df[[var]]

  ## Count by day adding in zero counts when none detected
  df_count <- df %>%
    filter(date >= start_date, year <= end_date) %>%
    drop_na(date) %>%
    count(date, .drop = FALSE,
          name = "notifications") %>%
    mutate(notifications = notifications %>%
             replace_na(0))


  ## plot normalised incidence by month
  month_plot <- df_count %>%
    count(floor_date(date, "month"), wt = notifications) %>%
    add_count(year(`floor_date(date, "month")`),
              wt = n, name = "nn") %>%
    mutate(n = n / nn) %>%
    mutate(month = month(`floor_date(date, "month")`, label = TRUE)) %>%
    ggplot(aes(x= month, y = n)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(alpha = 0.4) +
    scale_y_continuous(labels = percent) +
    theme_minimal() +
    labs(x = "Month",
         y = "Percentage of annual notifications")

  ## Plot normalised incidencce by day
  day_plot <- df_count %>%
    count(floor_date(date, "day"), wt = notifications) %>%
    add_count(floor_date(`floor_date(date, "day")`, "month"),
              wt = n, name = "nn") %>%
    mutate(n = n / nn) %>%
    mutate(mday = mday(`floor_date(date, "day")`)) %>%
    ggplot(aes(x= mday, y = n, group = mday)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(alpha = 0.05) +
    scale_y_continuous(labels = percent) +
    theme_minimal() +
    labs(x = "Day of the month",
         y = "Percentage of monthly notifications")

  plots <- c(sum_month_plot, sum_day_lot)
  names(plots) <- c("by_month", "by_day")

  return(plots)
}
