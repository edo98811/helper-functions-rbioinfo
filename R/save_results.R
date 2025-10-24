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

  if (params$analysis_name == "" || is.null(params$analysis_name)) {
    stop("Invalid analysis_name provided in parameters.")
  }

  # Validate the input objects
  if (!is.list(objects)) {
    stop("Invalid objects provided. Please provide a list of objects to save.")
  }

  # Determine the directory path based on the provided parameters
  dir_path <- file.path("analyses_results", params$analysis_name)

  # Create the directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  for (obj_name in names(objects)) {
    object_path <- file.path(dir_path, paste0(obj_name, ".rds"))
    saveRDS(objects[[obj_name]], object_path)
    message(sprintf("Saved '%s' object to %s", obj_name, object_path))
  }
}
