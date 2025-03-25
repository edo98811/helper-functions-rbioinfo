#' Save Analysis Objects
#'
#' This function saves various analysis objects to disk. If an experiment name is provided in the `params` list, 
#' the objects are saved in a directory named after the experiment within the `analyses_data` directory. 
#' Otherwise, the objects are saved in the current working directory.
#'
#' @param params A list containing parameters for the analysis. Must include `analysis_name` if saving to a specific directory.
#' @param myresuSet The main results set to be saved.
#' @param dds The DESeq2 results object to be saved.
#' @param dds_releveled Optional. A DESeq2 results object with releveled factors to be saved.
#' @param dds_interaction_term Optional. A DESeq2 results object with interaction terms to be saved.
#' @param gtl Optional. A gene-to-locus mapping object to be saved.
#'
#' @return None. This function is called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' params <- list(analysis_name = "experiment_1")
#' save_analysis_objects(params, myresuSet, dds, dds_releveled, dds_interaction_term, gtl)
#' }
save_analysis_objects <- function(
    params, 
    myresuSet, 
    dds, 
    dds_releveled = NULL, 
    dds_interaction_term = NULL, 
    gtl = NULL) {

    if (is.character(params$analysis_name) & length(params$analysis_name)) {
        dir.create(file.path("analyses_data", params$analysis_name), showWarnings = FALSE, recursive = TRUE)
        saveRDS(myresuSet, file.path("analyses_data", params$analysis_name, "myResuSet.RDS"))
        message("myResuSet saved successfully.")
        saveRDS(dds, file.path("analyses_data", params$analysis_name, "dds_DE_results.RDS"))
        message("dds saved successfully.")
        if (!is.null(dds_releveled)) {
            saveRDS(dds_releveled, file.path("analyses_data", params$analysis_name, "dds_DE_results_releveled.RDS"))
            message("dds_releveled saved successfully.")
        }
        if (!is.null(dds_interaction_term)) {
            saveRDS(dds_interaction_term, file.path("analyses_data", params$analysis_name, "dds_DE_results_interaction_term.RDS"))
            message("dds_interaction_term saved successfully.")
        }
        if (!is.null(gtl)) {
            saveRDS(gtl, file.path("analyses_data", params$analysis_name, "gtl.RDS"))
            message("gtl saved successfully.")
        }
    } else {
        saveRDS(myresuSet, "myResuSet.RDS")
        message("myResuSet saved successfully.")
        saveRDS(dds, "dds_DE_results.RDS")
        message("dds saved successfully.")
        if (!is.null(dds_releveled)) {
            saveRDS(dds_releveled, "dds_DE_results_releveled.RDS")
            message("dds_releveled saved successfully.")
        }
        if (!is.null(dds_interaction_term)) {
            saveRDS(dds_interaction_term, "dds_DE_results_interaction_term.RDS")
            message("dds_interaction_term saved successfully.")
        }
        if (!is.null(gtl)) {
            saveRDS(gtl, "gtl.RDS")
            message("gtl saved successfully.")
        }
    }
}
