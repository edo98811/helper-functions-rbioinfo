#' Save Multiple Objects Based on Conditions
#'
#' This function saves multiple R objects to disk based on specified conditions and parameters.
#' It validates the input parameters and objects, creates necessary directories, and saves the objects
#' in `.rds` format according to the workflow and other conditions provided.
#'
#' @param params A list containing the following elements:
#'   \describe{
#'     \item{\code{workflow}}{A string specifying the workflow type. Valid values are "se", "dds", "se_vdx", "se_dds".}
#'     \item{\code{run_computations}}{A logical value indicating whether computations are run.}
#'     \item{\code{analysis_folder}}{A string specifying the folder where analysis results should be saved.}
#'     \item{\code{analysis_name}}{A string specifying the name of the analysis.}
#'     \item{\code{save_results}}{A logical value indicating whether to save the results object.}
#'   }
#' @param objects A named list of objects to save. Valid object names are:
#'   \describe{
#'     \item{\code{se}}{SummarizedExperiment object.}
#'     \item{\code{dds}}{DESeqDataSet object.}
#'     \item{\code{vdx}}{VDX object.}
#'     \item{\code{results}}{Results object.}
#'   }
#'
#' @return A message indicating that the objects were saved successfully.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates the input parameters and objects.
#'   \item Creates the directory for saving objects if it does not exist.
#'   \item Saves objects based on the specified workflow and conditions.
#' }
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   workflow = "se_dds",
#'   run_computations = TRUE,
#'   analysis_folder = "output",
#'   analysis_name = "experiment_1",
#'   save_results = TRUE
#' )
#' objects <- list(
#'   se = se_object,
#'   dds = dds_object,
#'   vdx = vdx_object,
#'   results = results_object
#' )
#' save_results(params, objects)
#' }
#'
#' @export
save_results <- function(params, objects) {

  # Validate the input parameters
  if (!is.list(params) || !all(c("workflow", "run_computations", "analysis_folder", "analysis_name") %in% names(params))) {
    stop("Invalid parameters provided. Please provide a list with 'workflow', 'run_computations', 'analysis_folder', and 'analysis_name'.")
  }

  # Validate the input objects
  if (!is.list(objects)) {
    stop("Invalid objects provided. Please provide a list of objects to save.")
  }

  # Validate that object names are valid
  valid_object_names <- c("se", "dds", "vdx", "results")
  invalid_objects <- setdiff(names(objects), valid_object_names)
  
  if (length(invalid_objects) > 0) {
    warning(sprintf("The following objects are not valid and will not be saved: %s", paste(invalid_objects, collapse = ", ")))
  }

  # Determine the directory path based on the provided parameters
  dir_path <- ifelse(params$analysis_folder, 
                     file.path(params$analysis_folder, params$analysis_name), 
                     file.path("analysis_results", params$analysis_name))

  # Create the directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Save the se object if the workflow matches specific conditions
  if (params$workflow == "se" || params$workflow == "se_vdx" || params$workflow == "se_dds") {
    if (!is.null(objects$se)) {
      object_path <- file.path(dir_path, "se.rds")
      saveRDS(objects$se, object_path)
    } else {
      stop("The 'se' object is missing in the provided objects.")
    }
  }

  # Save the dds object if the workflow matches specific conditions
  if (params$workflow == "dds" || params$workflow == "se_dds") {
    if (!is.null(objects$dds)) {
      object_path <- file.path(dir_path, "dds.rds")
      saveRDS(objects$dds, object_path)
    } else {
      stop("The 'dds' object is missing in the provided objects.")
    }
  }

  # Save the vdx object if the workflow matches specific conditions
  if (params$workflow == "se_vdx") {
    if (!is.null(objects$vdx)) {
      object_path <- file.path(dir_path, "vdx.rds")
      saveRDS(objects$vdx, object_path)
    } else {
      stop("The 'vdx' object is missing in the provided objects.")
    }
  }

  # Save the results object if computations are not run
  if (params$save_results == TRUE) {
    if (!is.null(objects$results)) {
      object_path <- file.path(dir_path, "results.rds")
      saveRDS(objects$results, object_path)
    } else {
      stop("The 'results' object is missing in the provided objects.")
    }
  }
  
  return("Objects saved successfully.")
}