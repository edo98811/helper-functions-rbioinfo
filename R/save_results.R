#' Save Multiple Objects Based on Conditions
#'
#' This function saves multiple R objects to disk based on a list given as argument.
#' It ccreates necessary directories, and saves the objects in `.rds` format according to their name in the list.
#'
#' @param params A list containing the following elements:
#'   \describe{
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
#' @export
save_results <- function(params, objects) {

  if (params$analysis_name == "" || is.null(params$analysis_name)) {
    stop("Invalid analysis_name provided in parameters.")
  }

  if (!params$save_results) {
    message("save_results is FALSE. Skipping saving of results")
    return(invisible(NULL))
  }


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

  return(invisible(NULL))
}
