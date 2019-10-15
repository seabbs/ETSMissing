#' Make a Results Folder
#'
#' @description This utility function makes a results folder with the specified name within a results folder. It will
#' also overwrite old results if required. Returns a path to the results folder.
#' @param prefix A character string containing the name of the results folder to create
#' @param regen Logical - defaulting to \code{FALSE}. Should the results folders be regenerated.
#'
#' @return A path to the results folder.
#' @export
#'
#' @importFrom here here
#' @examples
#'
#' ## Code
#' make_results_folder
make_results_folder <- function(prefix, regen = FALSE) {

  if (!dir.exists("results")) {
    dir.create(here("vignettes", "results"))
  }

  if (!dir.exists(file.path("results", prefix)) | regen) {
    if (regen) {
      unlink(here(file.path("vignettes/results", prefix)), recursive = TRUE)
    }
    dir.create(here(file.path("vignettes", "results", prefix)))
  }

  path <- here(file.path("vignettes", "results", prefix))

  return(path)
}
