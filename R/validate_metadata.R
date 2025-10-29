#' Validate Metadata Against SummarizedExperiment Object
#'
#' This function checks whether the row names of a metadata data frame match
#' the row names of the `rowData` in a `SummarizedExperiment` object.
#'
#' @param metadata_df A `data.frame` containing metadata. The row names of this
#' data frame should correspond to the row names of the `rowData` in the
#' `SummarizedExperiment` object.
#' @param se A `SummarizedExperiment` object. The function validates that the
#' row names of the `metadata_df` match the row names of the `rowData` in this object.
#'
#' @return Returns `TRUE` if the row names match. If the row names do not match,
#' the function throws an error.
#'
#' @export
validate_metadata <- function(metadata_df, se) {
  # Check if the rownames of the metadata data.frame match the rownames of the rowData in the SummarizedExperiment object
  if (!all(rownames(metadata_df) == rownames(rowData(se)))) {
    stop("The rownames of the metadata data.frame do not match the rownames of the rowData in the SummarizedExperiment object.")
  }
  return(TRUE)
}

