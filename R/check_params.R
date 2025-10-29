#' Check that the parameters in params are valid (deprecated)
#'
#' This function validates that a given list contains specific required names
#' and checks the values of certain parameters for correctness.
#'
#' @param params A named list containing parameters to be validated.
#'   The list must include the following required names:
#'   \itemize{
#'     \item \code{"run_computations"}: Logical value indicating whether computations should be run.
#'     \item \code{"analysis_name"}: Character string specifying the name of the analysis.
#'     \item \code{"source_se"}: Source object for the analysis.
#'     \item \code{"subset_object"}: Logical value indicating whether to subset the object.
#'     \item \code{"species"}: Character string specifying the species, must be either \code{"Hs"} or \code{"Mm"}.
#'     \item \code{"workflow"}: Workflow configuration for the analysis.
#'     \item \code{"save_results"}: Logical value indicating whether to save results.
#'   }
#'
#' @return Returns \code{TRUE} if all required parameters are present and valid.
#'
#' @examples
#' params <- list(
#'   run_computations = TRUE,
#'   analysis_name = "Example Analysis",
#'   source_se = "source",
#'   subset_object = FALSE,
#'   species = "Hs",
#'   workflow = "default",
#'   save_results = TRUE
#' )
#' check_params(params) # Returns TRUE
#'
#' @export
# Function to check that a list contains specific required names
check_params <- function(params_list) {
  #  to use:   # if (!check_params(params)) stop("save_results: wrong parameters provided.")
  # Define the required names
  required_names <- c(
    "run_computations",
    "analysis_name",
    "source_object",
    "metadata_file",
    # "subset_object",
    "species",
    "create_annotation_df",
    # "workflow",
    "save_results"
  )

  # Check if all required names are present in the list
  missing_names <- setdiff(required_names, names(params_list))

  if (length(missing_names) > 0) {
    warning(paste("The following required parameters are missing:", paste(missing_names, collapse = ", ")))
    return(FALSE)
  }

  # Validate workflow parameter
  # valid_workflows <- c("se", "se_vdx", "se_dds", "dds", "vdx", "se_dde", "dde")
  # if (!params$workflow %in% valid_workflows) {
  #   stop("Invalid workflow type. Accepted values are: ", paste(valid_workflows, collapse = ", "))
  # }


  if (!params_list$species %in% c("Hs", "Mm")) {
    warning("The 'species' parameter must be either 'Hs' or 'Mm'.")
    return(FALSE)
  }

  return(TRUE)
}
