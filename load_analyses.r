source("load_dds_arrow.r")


load_analyses <- function(params, arrow = FALSE) {

    if (is.null(params$experiment_name)) {
        stop("The parameter 'experiment_name' is missing from the params and required.")
    }
    if ((is.character(params$experiment_name) & length(params$experiment_name)) && dir.exists(file.path("analyses_data", params$experiment_name))) {
        tryCatch({
            myresuSet <- ifelse(arrow, 
                readRDS(file.path("analyses_data", params$experiment_name, "myResuSet.RDS")),
                stop("not yet implemented"))# read_dds_arrow(file.path("analyses_data", params$experiment_name, "myResuSet.RDS"))
            assign("myresuSet", myresuSet, envir = parent.frame())
        }, error = function(e) {
            message("File 'myResuSet.RDS' not found in experiment folder.")
        })
        tryCatch({
            dds <- ifelse(arrow, 
                readRDS(file.path("analyses_data", params$experiment_name, "dds_DE_results.RDS")),
                stop("not yet implemented"))# read_dds_arrow(file.path("analyses_data", params$experiment_name, "dds_DE_results.RDS"))
            assign("dds", dds, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results.RDS' not found in experiment folder.")
        })
        tryCatch({
            dds_releveled <- ifelse(arrow, 
                readRDS(file.path("analyses_data", params$experiment_name, "dds_DE_results_releveled.RDS")),
                stop("not yet implemented"))# read_dds_arrow(file.path("analyses_data", params$experiment_name, "dds_DE_results_releveled.RDS"))
            assign("dds_releveled", dds_releveled, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_releveled.RDS' not found in experiment folder.")
        })
        tryCatch({
            dds_interaction_term <- ifelse(arrow, 
                readRDS(file.path("analyses_data", params$experiment_name, "dds_DE_results_interaction_term.RDS")),
                stop("not yet implemented"))# read_dds_arrow(file.path("analyses_data", params$experiment_name, "dds_DE_results_interaction_term.RDS"))
            assign("dds_interaction_term", dds_interaction_term, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_interaction_term.RDS' not found in experiment folder.")
        })
    } else {
        message("no experiment folder defined, trying to load from work folder...")
        tryCatch({
            myresuSet <- ifelse(arrow, 
                readRDS("myResuSet.RDS"),
                stop("not yet implemented"))# read_dds_arrow("myResuSet.RDS")
            assign("myresuSet", myresuSet, envir = parent.frame())
        }, error = function(e) {
            message("File 'myResuSet.RDS' not found in work folder.")
        })
        tryCatch({
            dds <- ifelse(arrow, 
                readRDS("dds_DE_results.RDS"),
                stop("not yet implemented"))# read_dds_arrow("dds_DE_results.RDS")
            assign("dds", dds, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results.RDS' not found in work folder.")
        })
        tryCatch({
            dds_releveled <- ifelse(arrow, 
                readRDS("dds_DE_results_releveled.RDS"),
                stop("not yet implemented"))# read_dds_arrow("dds_DE_results_releveled.RDS")
            assign("dds_releveled", dds_releveled, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_releveled.RDS' not found in work folder.")
        })
        tryCatch({
            dds_interaction_term <- ifelse(arrow, 
                readRDS("dds_DE_results_interaction_term.RDS"),
                stop("not yet implemented"))# read_dds_arrow("dds_DE_results_interaction_term.RDS")
            assign("dds_interaction_term", dds_interaction_term, envir = parent.frame())
        }, error = function(e) {
            message("File 'dds_DE_results_interaction_term.RDS' not found in work folder.")
        })
    }
}


