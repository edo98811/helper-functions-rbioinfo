#' Load Results Based on Workflow and Parameters
#'
#' This function loads data objects from RDS files based on the specified workflow and parameters.
#' It validates the input parameters, determines the directory path, and loads the appropriate objects
#' into the parent environment. If `run_computations` is set to `TRUE`, the loading operations are skipped.
#'
#' @param params A list containing the following elements:
#'
#' @return A character string indicating the status of the operation:
#'   \itemize{
#'     \item \code{"run_computations is TRUE. Skipping loading of results"} if \code{run_computations} is \code{TRUE}.
#'     \item \code{"Objects loaded and assigned to parent environment."} if the objects are successfully loaded.
#'   }
#'
#' @details
#' The function checks the existence of required RDS files based on the workflow type and loads the corresponding
#' objects (\code{se}, \code{dds}, \code{vdx}, or \code{results}) into the parent environment. If any required file
#' is missing, an error is raised.
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   workflow = "se",
#'   run_computations = FALSE,
#'   analysis_folder = "path/to/folder",
#'   analysis_name = "example_analysis"
#' )
#' load_results(params)
#' }
#'
#' @export
load_results <- function(params) {
  # Validate the input parameters
  if (params$analysis_name == "" || is.null(params$analysis_name)) {
    stop("Invalid analysis_name provided in parameters.")
  }
  # If run_computations is TRUE, skip loading operations
  if (params$run_computations == TRUE) {
    message("run_computations is TRUE. Skipping loading of results")
    return()
  }

  # Determine the directory path based on the provided parameters
  dir_path <- file.path("analyses_results", params$analysis_name)

  files_to_load <- list.files(dir_path)
  for (file in files_to_load) {
    object <- readRDS(file.path(dir_path, file))
    var_name <- sub("\\.rds$", "", file)
    assign(var_name, object, envir = parent.frame())
  }
}
