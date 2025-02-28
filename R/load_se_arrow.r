#' Load SummarizedExperiment from Feather files
#'
#' This function loads a SummarizedExperiment object from Feather files.
#'
#' @param file_path A character string specifying the base file path (without extensions) 
#'                  to the Feather files. The function expects three files with suffixes 
#'                  "_rowData.feather", "_colData.feather", and "_counts.feather".
#'
#' @return A SummarizedExperiment object containing the loaded data.
#'
#' @importFrom arrow read_feather
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importFrom methods as
#' @export
#'
#' @examples
#' \dontrun{
#'   se <- load_se_arrow("/path/to/your/data")
#' }

load_se_arrow <- function(file_path) {
  
  # Load the saved Feather files
  arrow::row_data <- read_feather(paste0(file_path, "_rowData.feather"))
  arrow::col_data <- read_feather(paste0(file_path, "_colData.feather"))
  arrow::counts_data <- read_feather(paste0(file_path, "_counts.feather"))
  
  # Convert data frames to appropriate formats
  row_data <- as(row_data, "DataFrame")
  col_data <- as(col_data, "DataFrame")
  counts_data <- as.matrix(counts_data)
  
  # Create a SummarizedExperiment object
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts_data),
    rowData = row_data,
    colData = col_data
  )
  
  return(se)
}