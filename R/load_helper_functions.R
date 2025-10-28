#' Load Helper Functions
#'
#' Loads all R scripts located in a specified folder on the user's computer.
#' Each script is sourced into the *global environment*, making all functions
#' and objects defined in those scripts immediately available for use.
#' 
#' @param folder A character string specifying the folder containing the R scripts to load. Default is "helper_functions".
#' @return None. The function loads the R scripts into the current environment.
#' @export
load_helper_functions <- function(folder = "helper_functions") {
  if (!dir.exists(folder)) {
    stop("The specified folder does not exist: ", folder)
  }

  r_files <- list.files(path = folder, pattern = "\\.R$", full.names = TRUE)

  if (length(r_files) == 0) {
    message("No R files found in folder: ", folder)
    return(invisible(NULL))
  }

  for (file in r_files) {
    message("Sourcing: ", basename(file))
    source(file, local = globalenv())
  }
  invisible(NULL)
}
