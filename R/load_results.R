#' Load Results Based on Workflow and Parameters
#'
#' This function loads data objects from RDS files based on the specified workflow and parameters.
#' It validates the input parameters, determines the directory path, and loads the appropriate objects
#' into the parent environment. If `run_computations` is set to `TRUE`, the loading operations are skipped.
#'
#' @param params A list containing the following elements:
#'   \itemize{
#'     \item \code{workflow}: A character string specifying the workflow type. Accepted values are 
#'     \code{"se"}, \code{"se_vdx"}, \code{"se_dds"}, \code{"dds"}, or \code{"vdx"}.
#'     \item \code{run_computations}: A logical value indicating whether computations should be run 
#'     instead of loading results.
#'     \item \code{analysis_folder}: A character string specifying the folder containing analysis results.
#'     \item \code{analysis_name}: A character string specifying the name of the analysis.
#'   }
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
# Function to check multiple conditions and return a message based on them
load_results <- function(params) {

  # Validate the input parameters
  if (!is.list(params) || !all(c("workflow", "run_computations", "analysis_folder", "analysis_name") %in% names(params))) {
    stop("Invalid parameters provided. Please provide a list with 'workflow', 'run_computations', 'analysis_folder', and 'analysis_name'.")
  }

  # If run_computations is TRUE, skip loading operations
  if (params$run_computations == TRUE) {
    return("run_computations is TRUE. Skipping loading of results")
  }

  # Determine the directory path based on the provided parameters
  dir_path <- ifelse(params$analysis_folder, 
                     file.path(params$analysis_folder, params$analysis_name), 
                     file.path("analysis_results", params$analysis_name))

  # Validate workflow parameter
  valid_workflows <- c("se", "se_vdx", "se_dds", "dds", "vdx", "se_dde", "dde")
  if (!params$workflow %in% valid_workflows) {
    stop("Invalid workflow type. Accepted values are: ", paste(valid_workflows, collapse = ", "))
  }

  # Load se object if the workflow requires it
  if (params$workflow %in% c("se", "se_vdx", "se_dds", "se_dde", "dde")) {
    object_path <- file.path(dir_path, "se.rds")
    if (!file.exists(object_path)) {
      stop("The se file does not exist: ", object_path)
    }
    se <- readRDS(object_path)
    assign("se", se, envir = .parent.frame())
  }

  # Load the dds object if the workflow requires it
  if (params$workflow %in% c("dds", "se_dds")) {
    object_path <- file.path(dir_path, "dds.rds")
    if (!file.exists(object_path)) {
      stop("The dds file does not exist: ", object_path)
    }
    dds <- readRDS(object_path)
    assign("dds", dds, envir = .parent.frame())
  }

  # Load dde object if the workflow requires it
  if (params$workflow %in% c("dde", "se_dde")) {
    object_path <- file.path(dir_path, "dde.rds")
    if (!file.exists(object_path)) {
      stop("The dde file does not exist: ", object_path)
    }
    dds <- readRDS(object_path)
    assign("dde", dds, envir = .parent.frame())
  }

  # Load vdx object if the workflow requires it
  if (params$workflow == "vdx") {
    object_path <- file.path(dir_path, "vdx.rds")
    if (!file.exists(object_path)) {
      stop("The vdx file does not exist: ", object_path)
    }
    vdx <- readRDS(object_path)
    assign("vdx", vdx, envir = .parent.frame())
  }

  # Load results if not running computations or if workflow is not dde or se_dde
  if (params$run_computations == FALSE %% !params$workflow %in% c("dde", "se_dde")) {
    object_path <- file.path(dir_path, "results.rds")
    if (!file.exists(object_path)) {
      stop("The results file does not exist: ", object_path)
    }
    results <- readRDS(object_path)
    assign("results", results, envir = .parent.frame())
  }
  
  return("Objects loaded and assigned to parent environment.")
}