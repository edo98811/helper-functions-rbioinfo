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
  if (!check_params(params)) stop("load_results: wrong parameters provided.")

  # If run_computations is TRUE, skip loading operations
  if (params$run_computations == TRUE) {
    message("run_computations is TRUE. Skipping loading of results")
    return()
  }

  # Determine the directory path based on the provided parameters
  dir_path <- file.path("analyses_results", params$analysis_name)

  # Load se object if the workflow requires it
  if (params$workflow %in% c("se", "se_vdx", "se_dds", "se_dde", "dde")) {
    object_path <- file.path(dir_path, "se.rds")
    if (!file.exists(object_path)) {
      warning("The se file does not exist: ", object_path)
    } else {
      se <- readRDS(object_path)
      assign("se", se, envir = parent.frame())
      message(sprintf("Loaded 'se' object from %s", object_path))
    }
  }

  # Load the dds object if the workflow requires it
  if (params$workflow %in% c("dds", "se_dds")) {
    object_path <- file.path(dir_path, "dds.rds")
    if (!file.exists(object_path)) {
      warning("The dds file does not exist: ", object_path)
    } else {
      dds <- readRDS(object_path)
      assign("dds", dds, envir = parent.frame())
      message(sprintf("Loaded 'dds' object from %s", object_path))
    }
  }

  # Load dde object if the workflow requires it
  if (params$workflow %in% c("dde", "se_dde")) {
    object_path <- file.path(dir_path, "dde.rds")
    if (!file.exists(object_path)) {
      warning("The dde file does not exist: ", object_path)
    } else {
      dde <- readRDS(object_path)
      assign("dde", dde, envir = parent.frame())
      message(sprintf("Loaded 'dde' object from %s", object_path))
    }
  }

  # Load vdx object if the workflow requires it
  if (params$workflow == "vdx") {
    object_path <- file.path(dir_path, "vdx.rds")
    if (!file.exists(object_path)) {
      warning("The vdx file does not exist: ", object_path)
    } else {
      vdx <- readRDS(object_path)
      assign("vdx", vdx, envir = parent.frame())
      message(sprintf("Loaded 'vdx' object from %s", object_path))
    }
  }

  # Load results if not running computations or if workflow is not dde or se_dde
  if (params$run_computations == FALSE && !params$workflow %in% c("dde", "se_dde")) {
    object_path <- file.path(dir_path, "results.rds")
    if (!file.exists(object_path)) {
      warning("The results file does not exist: ", object_path)
    } else {
      results <- readRDS(object_path)
      assign("results", results, envir = parent.frame())
      message(sprintf("Loaded 'results' object from %s", object_path))
    }
  }
}

