save_analysis_objects <- function(
    params, 
    myresuSet, 
    dds, 
    dds_releveled = NULL, 
    dds_interaction_term = NULL) {

    if (is.character(params$experiment_name) & length(params$experiment_name)) {
        dir.create(file.path("analyses_data", params$experiment_name), showWarnings = FALSE, recursive = TRUE)
        saveRDS(myresuSet, file.path("analyses_data", params$experiment_name, "myResuSet.RDS"))
        message("myResuSet saved successfully.")
        saveRDS(dds, file.path("analyses_data", params$experiment_name, "dds_DE_results.RDS"))
        message("dds saved successfully.")
        if (!is.null(dds_releveled)) {
            saveRDS(dds_releveled, file.path("analyses_data", params$experiment_name, "dds_DE_results_releveled.RDS"))
            message("dds_releveled saved successfully.")
        }
        if (!is.null(dds_interaction_term)) {
            saveRDS(dds_interaction_term, file.path("analyses_data", params$experiment_name, "dds_DE_results_interaction_term.RDS"))
            message("dds_interaction_term saved successfully.")
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
    }
}
