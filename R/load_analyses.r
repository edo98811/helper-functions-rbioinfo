#' Load Analysis Results
#'
#' This function loads analysis results from specified files based on the provided parameters.
#'
#' @param params A list containing the parameters for loading the analysis results. 
#'               It must include the following element:
#'               \itemize{
#'                 \item \code{experiment_name} (optional): A character string specifying the name of the experiment. 
#'                       If provided, the function will attempt to load files from the corresponding folder 
#'                       within the "analyses_data" directory. If not provided, the function will attempt to load 
#'                       files from the current working directory.
#'               }
#'
#' @return This function does not return a value. Instead, it assigns the loaded objects to the parent environment:
#'         \itemize{
#'           \item \code{myresuSet}: The result set object loaded from "myResuSet.RDS".
#'           \item \code{dds}: The DESeq2 results object loaded from "dds_DE_results.RDS".
#'           \item \code{dds_releveled}: The releveled DESeq2 results object loaded from "dds_DE_results_releveled.RDS".
#'           \item \code{dds_interaction_term}: The interaction term DESeq2 results object loaded from "dds_DE_results_interaction_term.RDS".
#'         }
#'
#' @details The function first checks if the \code{experiment_name} parameter is provided and if the corresponding 
#'          directory exists within the "analyses_data" folder. If so, it attempts to load the specified RDS files 
#'          from that directory. If any file is not found, a message is displayed. If \code{experiment_name} is not 
#'          provided or the directory does not exist, the function attempts to load the files from the current working 
#'          directory.
#'
#' @examples
#' \dontrun{
#' params <- list(experiment_name = "experiment1")
#' load_analyses(params)
#' }
#'
#' @export

load_analyses <- function(params) {

    if (is.null(params$experiment_name)) {
        stop("The parameter 'experiment_name' is missing from the params and required.")
    }
    if ((is.character(params$experiment_name) & length(params$experiment_name)) && dir.exists(file.path("analyses_data", params$experiment_name))) {
        tryCatch({
            myresuSet <- readRDS(file.path("analyses_data", params$experiment_name, "myResuSet.RDS"))
            assign("myresuSet", myresuSet, envir = parent.frame())
        }, error = function(e) {
            message("File 'myResuSet.RDS' not found in experiment folder.")
        })
        tryCatch({
            dds <- readRDS(file.path("analyses_data", params$experiment_name, "dds_DE_results.RDS"))
            assign("dds", dds, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results.RDS' not found in experiment folder.")
        })
        tryCatch({
            dds_releveled <- readRDS(file.path("analyses_data", params$experiment_name, "dds_DE_results_releveled.RDS"))
            assign("dds_releveled", dds_releveled, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_releveled.RDS' not found in experiment folder.")
        })
        tryCatch({
            dds_interaction_term <- readRDS(file.path("analyses_data", params$experiment_name, "dds_DE_results_interaction_term.RDS"))
            assign("dds_interaction_term", dds_interaction_term, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_interaction_term.RDS' not found in experiment folder.")
        })
    } else {
        message("no experiment folder defined, trying to load from work folder...")
        tryCatch({
            myresuSet <- readRDS("myResuSet.RDS")
            assign("myresuSet", myresuSet, envir = parent.frame())
        }, error = function(e) {
            message("File 'myResuSet.RDS' not found in work folder.")
        })
        tryCatch({
            dds <- readRDS("dds_DE_results.RDS")
            assign("dds", dds, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results.RDS' not found in work folder.")
        })
        tryCatch({
            dds_releveled <- readRDS("dds_DE_results_releveled.RDS")
            assign("dds_releveled", dds_releveled, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_releveled.RDS' not found in work folder.")
        })
        tryCatch({
            dds_interaction_term <- readRDS("dds_DE_results_interaction_term.RDS")
            assign("dds_interaction_term", dds_interaction_term, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_interaction_term.RDS' not found in work folder.")
        })
    }
}


