source("save_dd_as_arrow.r")

save_analysis_objects <- function(
    params, 
    myresuSet, 
    dds, 
    dds_releveled = NULL, 
    dds_interaction_term = NULL, 
    gtl = NULL,
    arrow = FALSE) {

    if (is.character(params$experiment_name) & length(params$experiment_name)) {
        dir.create(file.path("analyses_data", params$experiment_name), showWarnings = FALSE, recursive = TRUE)
        ifelse(arrow,
            saveRDS(myresuSet, file.path("analyses_data", params$experiment_name, "myResuSet.RDS")),
            save_dds_as_arrow(myresuSet, file.path("analyses_data", params$experiment_name, "myResuSet.RDS"))
        )
        message("myResuSet saved successfully.")
        ifelse(arrow,
            saveRDS(dds, file.path("analyses_data", params$experiment_name, "dds_DE_results.RDS")),
            save_dds_as_arrow(dds, file.path("analyses_data", params$experiment_name, "dds_DE_results.RDS"))
        )
        message("dds saved successfully.")
        if (!is.null(dds_releveled)) {
            ifelse(arrow,
                saveRDS(dds_releveled, file.path("analyses_data", params$experiment_name, "dds_DE_results_releveled.RDS")),
                save_dds_as_arrow(dds_releveled, file.path("analyses_data", params$experiment_name, "dds_DE_results_releveled.RDS"))
            )
            message("dds_releveled saved successfully.")
        }
        if (!is.null(dds_interaction_term)) {
            ifelse(arrow,
                saveRDS(dds_interaction_term, file.path("analyses_data", params$experiment_name, "dds_DE_results_interaction_term.RDS")),
                save_dds_as_arrow(dds_interaction_term, file.path("analyses_data", params$experiment_name, "dds_DE_results_interaction_term.RDS"))
            )
            message("dds_interaction_term saved successfully.")
        }
        if (!is.null(gtl)) {
            ifelse(arrow,
                saveRDS(gtl, file.path("analyses_data", params$experiment_name, "gtl.RDS")),
                save_dds_as_arrow(gtl, file.path("analyses_data", params$experiment_name, "gtl.RDS"))
            )
            message("gtl saved successfully.")
        }
    } else {
        ifelse(arrow,
            saveRDS(myresuSet, "myResuSet.RDS"),
            save_dds_as_arrow(myresuSet, "myResuSet.RDS")
        )
        message("myResuSet saved successfully.")
        ifelse(arrow,
            saveRDS(dds, "dds_DE_results.RDS"),
            save_dds_as_arrow(dds, "dds_DE_results.RDS")
        )
        message("dds saved successfully.")
        if (!is.null(dds_releveled)) {
            ifelse(arrow,
                saveRDS(dds_releveled, "dds_DE_results_releveled.RDS"),
                save_dds_as_arrow(dds_releveled, "dds_DE_results_releveled.RDS")
            )
            message("dds_releveled saved successfully.")
        }
        if (!is.null(dds_interaction_term)) {
            ifelse(arrow,
                saveRDS(dds_interaction_term, "dds_DE_results_interaction_term.RDS"),
                save_dds_as_arrow(dds_interaction_term, "dds_DE_results_interaction_term.RDS")
            )
            message("dds_interaction_term saved successfully.")
        }
        if (!is.null(gtl)) {
            ifelse(arrow,
                saveRDS(gtl, "gtl.RDS"),
                save_dds_as_arrow(gtl, "gtl.RDS")
            )
            message("gtl saved successfully.")
        }
    }
}
