#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Save results
#'
#' @param df An object
#' @param name Character string to use to save the object
#' @param path Character string giving the folder path
#'
#' @return Nothing returned
#' @export
#'
#' @examples
#'
#' ##Code
#' save_data
save_data <- function(df, name, path = NULL) {

  path <- file.path(path, paste0(name, ".rds"))

  saveRDS(df, path)

  invisible()
}

## Reading results
#' Read in results
#'
#' @param name Character string identifying the data
#' @inheritParams save_data
#'
#' @return The saved object
#' @export
#'
#'
#' @examples
#'
#' ##Code
#' read_data
read_data <- function(name, path = NULL) {
  path <- file.path(path, paste0(name, ".rds"))

  readRDS(path)
}

## Saving figures
#' Save Figures a PNGs
#'
#' @param fig \code{ggplot2} object.
#' @param name A character string giving the name for the figure
#' @inheritParams read_data
#' @return Nothing returned
#' @export
#' @importFrom ggplot2 ggsave
#' @examples
#'
#' ## Code
#' save_figure
save_figure <- function(fig, name, path = NULL) {

  path <- file.path(path, paste0(name, ".png"))

  ggplot2::ggsave(path, fig, dpi = 320, width = 8, height = 8)

  invisible()
}

#' Load and Show Saved Figures
#'
#' @param name Character string identifying the figure
#' @inheritParams save_data
#'
#' @return Prints a saved figure
#' @export
#' @importFrom knitr include_graphics
#'
#' @examples
#'
#' ## Code
#' show_figure
show_figure <- function(name, path = NULL) {
  path <- file.path(path, paste0(name, ".png"))

  plot <- knitr::include_graphics(path)

  return(plot)
}

#' Pull out Results by Variable
#'
#' @param df A dataframe containing an outcome variable (character) and a missing_table variable.
#' @param var A character string identifying which variable to pull results for.
#'
#' @return A dataframe of results
#' @export
#'
#' @importFrom dplyr filter pull first
#' @examples
#'
#' ## Code
#' pull_results
pull_results <- function(df, var) {
  outcome <- NULL; missing_table <- NULL;

  df %>%
    dplyr::filter(outcome %in% var) %>%
    dplyr::pull(missing_table) %>%
    dplyr::first()

}
