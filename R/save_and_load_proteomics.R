#' Save Proteomics Data
#'
#' This function saves proteomics data objects to specified directories.
#'
#' @param vdx A data object to be saved as an RDS file.
#' @param se A data object to be saved as an RDS file.
#' @param analysis_name A character string specifying the name of the experiment. This will be used to create a directory under "Analyses_data".
#' @param results_object An optional data object to be saved as an RDS file. Default is NULL.
#'
#' @details
#' The function creates a directory named after the `analysis_name` under "Analyses_data" if it does not already exist. It then saves the `vdx` and `se` objects as RDS files in this directory. If a `results_object` is provided, it is also saved as an RDS file in the same directory.
#'
#' @return
#' This function does not return a value. It is called for its side effects of saving data to disk.
#'
#' @examples
#' \dontrun{
#' vdx <- data.frame()  # Example data
#' se <- data.frame()   # Example data
#' results_object <- list()  # Example data
#' save_proteomics_data(vdx, se, "experiment_1", results_object)
#' }
#'
#' @export
save_proteomics_data <- function(vdx, se, analysis_name, params = list(), results_object = NULL) {

    # Create the directory if it doesn't exist
    dir_path <- file.path("Analyses_data", analysis_name)
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
    }
    # Save the results_object as RDS if it is provided
    if (!is.null(results_object)) {
        results_path <- file.path(dir_path, "results_object.rds")
        saveRDS(results_object, file = results_path)
    }
    # Save the vdx object as RDS
    vdx_path <- file.path(dir_path, "vdx.rds")
    saveRDS(vdx, file = vdx_path)
    
    # Save the se object as RDS
    se_path <- file.path(dir_path, "se.rds")
    saveRDS(se, file = se_path)
    
    message("Data saved successfully in ", dir_path)
}

#' Load Proteomics Data
#'
#' This function loads proteomics data for a given experiment from specified RDS files.
#'
#' @param analysis_name A character string specifying the name of the experiment.
#'
#' @return This function does not return a value but loads the following objects into the parent environment:
#' \itemize{
#'   \item \code{vdx}: The vdx object loaded from "vdx.rds".
#'   \item \code{se}: The se object loaded from "se.rds".
#'   \item \code{myresuSet} (optional): The results object loaded from "results_object.rds" if it exists.
#' }
#'
#' @details
#' The function checks for the existence of the specified directory and the required RDS files.
#' If any of the required files are missing, the function stops with an error message.
#' If the optional results object file is not found, a message is displayed.
#'
#' @examples
#' \dontrun{
#' load_proteomics_data("experiment_1")
#' }
#'
#' @export
load_proteomics_data <- function(params) {
    
    analysis_name <- params$analysis_name
    # Define the directory path
    dir_path <- file.path("Analyses_data", analysis_name)
    
    # Check if the directory exists
    if (!dir.exists(dir_path)) {
        stop("The directory does not exist: ", dir_path)
    }
    
    # Load the vdx object from RDS
    vdx_path <- file.path(dir_path, "vdx.rds")
    if (!file.exists(vdx_path)) {
        stop("The vdx file does not exist: ", vdx_path)
    }
    vdx <- readRDS(vdx_path)
    
    # Load the se object from RDS
    se_path <- file.path(dir_path, "se.rds")
    if (!file.exists(se_path)) {
        stop("The se file does not exist: ", se_path)
    }
    se <- readRDS(se_path)
    
    # Load the results_object from RDS if it exists
    results_path <- file.path(dir_path, "results_object.rds")
    if (file.exists(results_path) && (purrr::pluck(params, "load_results_objects", .default = FALSE))) {
        results_object <- readRDS(results_path)
        assign("myresuSet", results_object, envir = , envir = parent.frame)
    } else {
        message("No results object found")
    }
    
    message("Data loaded successfully from ", dir_path)
    
    assign("vdx", vdx, envir = , envir = parent.frame)
    assign("se", se, envir = , envir = parent.frame)
}