#' Handling Missing Values and Merging Technical Replicates
#'
#' This function processes a `SummarizedExperiment` object by handling missing values and merging technical replicates.
#'
#' @param se A `SummarizedExperiment` object containing the count matrix and experiment metadata.
#' @param handling_of_missing_values A character string specifying how to handle missing values. 
#'        Options are "no" (default), "delete", or "infer".
#'
#' @return A `SummarizedExperiment` object with merged technical replicates and handled missing values.
#'
#' @details
#' The function performs the following steps:
#' 1. Extracts the count matrix and experiment metadata from the input `SummarizedExperiment` object.
#' 2. Identifies unique biological samples by concatenating the group and biological replicate information.
#' 3. Sorts technical replicates for each biological sample.
#' 4. Merges the count matrix by averaging technical replicates and assigning NA when all technical replicates are missing.
#' 5. Creates merged experiment metadata.
#' 6. Creates a new `SummarizedExperiment` object with the merged data.
#' 7. Handles missing values based on the specified method:
#'    - "delete": Removes rows with any missing values.
#'    - "infer": Infers missing values by replacing them with the minimum value in the row, if the proportion of missing values is less than 30%.
#'
#' @examples
#' # Example usage:
#' # merged_se <- handling_missing_values_and_merging(se, handling_of_missing_values = "delete")
#'
#' @import SummarizedExperiment
#' 
#' @export

handling_missing_values_and_merging <- function(se, handling_of_missing_values = "no") {
    count_matrix <- assay(se)
    experiment_metadata <- colData(se)
    
    # Get unique biological samples by concatenating group and biological replicate
    biological_samples <- unique(apply(experiment_metadata, 1, function (x) paste0(x[["group"]], "_", x[["biological_replicate"]])))

    # Sort technical replicates for each biological sample
    technical_replicates_sorted <- sapply(biological_samples, function(sample) {
        pattern <- paste0("^", gsub("\\+", "\\\\+", sample, "_"))
        grep(pattern, colnames(count_matrix), value = TRUE)   
    }, simplify = FALSE)

    # Merge count matrix by averaging technical replicates and assigning NA when all the technical replicates are missing
    merged_count_matrix <- sapply(technical_replicates_sorted, function(replicates) {
        technical_replicates_matrix <- count_matrix[, replicates]
        apply(technical_replicates_matrix, 1, function (x) {
            if (all(is.na(x))) {
                return(NA)
            } else {
                return(mean(x, na.rm = TRUE))
            }
        })
    }, USE.NAMES = TRUE)

    # Create merged experiment metadata
    experiment_metadata_merged <- do.call(rbind, lapply(names(technical_replicates_sorted), function(sample) {
        parts <- strsplit(sample, "_")[[1]]
        data.frame(row.names = sample, group = parts[[1]], biological_replicate = parts[[2]], stringsAsFactors = FALSE)
    }))

    row_data <- rowData(se)

    # Create a new SummarizedExperiment object with merged data
    merged_se <- SummarizedExperiment(
        assays = list(counts = as.matrix(merged_count_matrix)),
        colData = experiment_metadata_merged,
        rowData = row_data
    )

    # Handle missing values based on the specified method
    if (handling_of_missing_values == "delete") 
        merged_se <- delete_missing_proteins(merged_se)
    else if (handling_of_missing_values == "infer") 
        merged_se <- infer_missing_proteins(merged_se)
    
    return(merged_se)
}

delete_missing_proteins <- function(se) {
    count_matrix <- assay(se)
    row_data <- rowData(se)

    # Filter out rows with any missing values
    count_matrix_filtered <- count_matrix[rowSums(is.na(count_matrix)) == 0, , drop = FALSE]
    
    # Update row_data to match the filtered count_matrix
    row_data <- row_data[rownames(row_data) %in% rownames(count_matrix_filtered),]

    # Create a new SummarizedExperiment object with the filtered data
    se <- SummarizedExperiment(
        assays = list(counts = count_matrix_filtered),
        colData = colData(se),
        rowData = row_data
    )
    return(se)
}

infer_missing_proteins <- function(se) {
    count_matrix <- assay(se)
    row_data <- rowData(se)

    count_matrix_filtered <- t(apply(count_matrix, 1, function (x) { # iterate through rows with 1
        if (sum(is.na(x)) / length(x) > 0.3) {
            return(rep(NA, length(x)))
        } else {
            x[is.na(x)] <- min(x, na.rm = TRUE)
            # is.na(x) <- min(x, na.rm = TRUE)
            return(x)
        }
    }))

    # Filter out rows with any missing values
    colnames(count_matrix_filtered) <- rownames(colData(se))
    count_matrix_filtered <- count_matrix_filtered[rowSums(is.na(count_matrix_filtered)) == 0, , drop = FALSE]
    
    row_data <- row_data[rownames(row_data) %in% rownames(count_matrix_filtered), ]

    se <- SummarizedExperiment(
        assays = list(counts = as.matrix(count_matrix_filtered)),
        colData = colData(se),
        rowData = row_data
    )
    return(se)
}