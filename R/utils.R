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
#' @inheritParams knitr include_graphics
#'
#' @examples
#'
#' ## Code
#' show_figure
show_figure <- function(name, path = fig_path) {
  path <- file.path(path, paste0(name, ".png"))

  plot <- knitr::include_graphics(path)

  return(plot)
}
