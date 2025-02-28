library(arrow)
#' Save SummarizedExperiment Object to Feather Files
#'
#' This function saves the rowData, colData, and assay(counts) of a 
#' SummarizedExperiment object to separate Feather files.
#'
#' @param se A SummarizedExperiment object.
#' @param file_path A character string specifying the base file path 
#'   (without extension) where the Feather files will be saved.
#'
#' @return None. This function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' library(SummarizedExperiment)
#' library(arrow)
#' 
#' # Create a SummarizedExperiment object
#' se <- SummarizedExperiment(assays = list(counts = matrix(1:4, ncol = 2)),
#'                            rowData = DataFrame(gene = c("gene1", "gene2")),
#'                            colData = DataFrame(sample = c("sample1", "sample2")))
#' 
#' # Save the SummarizedExperiment object to Feather files
#' save_se_arrow(se, "/path/to/save/se_data")
#' }
#' 
#' @importFrom SummarizedExperiment rowData colData assay
#' @importFrom arrow write_feather
#' @export

save_se_arrow <- function(se, file_path) {

    # Extract rowData, colData, and assay(counts) from the se object
    row_data <- as.data.frame(rowData(se))
    col_data <- as.data.frame(colData(se))
    counts_data <- as.data.frame(assay(se, "counts"))
    
    # Save the extracted data as separate Feather files
    arrow::write_feather(row_data, paste0(file_path, "_rowData.feather"))
    arrow::write_feather(col_data, paste0(file_path, "_colData.feather"))
    arrow::write_feather(counts_data, paste0(file_path, "_counts.feather"))
}

# Example usage:
# save_dds_arrow(dds, "/path/to/save/dds.feather")