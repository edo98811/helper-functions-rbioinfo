#' Check that the parameters in params are valid
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
#' @throws An error if any required parameter is missing or if the values of 
#'   \code{subset_object} or \code{species} are invalid.
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
check_params <- function(params) {
  # Define the required names
  required_names <- c(
    "run_computations",
    "analysis_name",
    "source_se",
    "metadata_file",
    "subset_object",
    "species",
    "workflow",
    "save_results"
  )
  
  # Check if all required names are present in the list
  missing_names <- setdiff(required_names, names(params_list))
  
  if (length(missing_names) > 0) {
    stop(paste("The following required parameters are missing:", paste(missing_names, collapse = ", ")))
  }
  
  # Validate specific values for subset_object and species
  if (!is.logical(params_list$subset_object)) {
    stop("The 'subset_object' parameter must be TRUE or FALSE.")
  }
  
  if (!params_list$species %in% c("Hs", "Mm")) {
    stop("The 'species' parameter must be either 'Hs' or 'Mm'.")
  }
  
  return(TRUE)
}