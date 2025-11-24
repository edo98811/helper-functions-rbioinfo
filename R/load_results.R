#' Load Results Based on Workflow and Parameters
#'
#' This function loads data objects from RDS files based on the specified workflow and parameters.
#' It validates the input parameters, determines the directory path, and loads the appropriate objects
#' into the parent environment. If `run_computations` is set to `TRUE`, the loading operations are skipped.
#'
#' @param params A list containing the following elements:
#'
#' @return NULL
#'
#' @details
#' The function checks the existence of required RDS files in the analized_data subfolder of the analysis into the parent environment.
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   run_computations = TRUE,
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
  return(invisible(NULL))
}
