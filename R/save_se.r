#' Save SummarizedExperiment Object
#'
#' This function saves a SummarizedExperiment (SE) object to a specified file path.
#'
#' @param se A SummarizedExperiment object to be saved.
#' @param params A list of parameters, which should include an element named "analysis_name" 
#'               that specifies the name of the experiment. This name will be used to construct 
#'               the file path where the SE object will be saved.
#'
#' @return None. This function is used for its side effect of saving the SE object to a file.
#'
#' @examples
#' \dontrun{
#' se <- SummarizedExperiment::SummarizedExperiment()
#' params <- list(analysis_name = "my_experiment")
#' save_se(se, params)
#' }
#'
#' @importFrom purrr pluck
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @export
save_se <- function(se, params) {

    # Construct the file path
    file_path <- file.path("analyses_data", purrr::pluck(params, "analysis_name", default = ""), "se.rds")
    
    # Save the se object
    saveRDS(se, file_path)
}