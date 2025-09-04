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
  if (!check_params(params)) stop("save_results: wrong parameters provided.")

  # Validate the input objects
  if (!is.list(objects)) {
    stop("Invalid objects provided. Please provide a list of objects to save.")
  }

  # # Validate that object names are valid
  # valid_object_names <- c("se", "dds", "vdx", "results", "dde", "anns")
  # invalid_objects <- setdiff(names(objects), valid_object_names)

  # if (length(invalid_objects) > 0) {
  #   warning(sprintf("The following objects are not valid and will not be saved: %s", paste(invalid_objects, collapse = ", ")))
  # }

  # Determine the directory path based on the provided parameters
  dir_path <- file.path("analyses_results", params$analysis_name)

  # Create the directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # # Save the se object if the workflow matches specific conditions
  # if (params$workflow == "se" || params$workflow == "se_vdx" || params$workflow == "se_dds") {
  #   if (!is.null(objects$se)) {
  #     object_path <- file.path(dir_path, "se.rds")
  #     saveRDS(objects$se, object_path)
  #     message(sprintf("Saved 'se' object to %s", object_path))
  #   } else {
  #     warning("The 'se' object is missing in the provided objects.")
  #   }
  # }

  # # Save the dds object if the workflow matches specific conditions
  # if (params$workflow == "dds" || params$workflow == "se_dds") {
  #   if (!is.null(objects$dds)) {
  #     object_path <- file.path(dir_path, "dds.rds")
  #     saveRDS(objects$dds, object_path)
  #     message(sprintf("Saved 'dds' object to %s", object_path))
  #   } else {
  #     warning("The 'dds' object is missing in the provided objects.")
  #   }
  # }

  # # Save the vdx object if the workflow matches specific conditions
  # if (params$workflow == "se_vdx") {
  #   if (!is.null(objects$vdx)) {
  #     object_path <- file.path(dir_path, "vdx.rds")
  #     saveRDS(objects$vdx, object_path)
  #     message(sprintf("Saved 'vdx' object to %s", object_path))
  #   } else {
  #     warning("The 'vdx' object is missing in the provided objects.")
  #   }
  # }

  # # Save the vdx object if the workflow matches specific conditions
  # if (params$workflow == "dde" || params$workflow == "se_dde") {
  #   if (!is.null(objects$dde)) {
  #     object_path <- file.path(dir_path, "dde.rds")
  #     saveRDS(objects$dde, object_path)
  #     message(sprintf("Saved 'dde' object to %s", object_path))
  #   } else {
  #     warning("The 'dde' object is missing in the provided objects.")
  #   }
  # }

  # # Save the results object if computations are not run
  # if (params$workflow != "dde" && params$workflow != "se_dde") {
  #   if (!is.null(objects$results)) {
  #     object_path <- file.path(dir_path, "results.rds")
  #     saveRDS(objects$results, object_path)
  #     message(sprintf("Saved 'results' object to %s", object_path))
  #   } else {
  #     warning("The 'results' object is missing in the provided objects.")
  #   }
  # }

  # # Save the results object if computations are not run
  # if (TRUE) {
  #   if (!is.null(objects$anns)) {
  #     object_path <- file.path(dir_path, "anns.rds")
  #     saveRDS(objects$anns, object_path)
  #     message(sprintf("Saved 'anns' object to %s", object_path))
  #   } else {
  #     warning("The 'anns' object is missing in the provided objects.")
  #   }
  # }

  # # Save any remaining objects that haven't been handled explicitly
  # handled_objects <- c("se", "dds", "vdx", "dde", "results", "anns")
  # remaining_objects <- setdiff(names(objects), handled_objects)

  for (obj_name in names(objects)) {
    object_path <- file.path(dir_path, paste0(obj_name, ".rds"))
    saveRDS(objects[[obj_name]], object_path)
    message(sprintf("Saved '%s' object to %s", obj_name, object_path))
  }
}
