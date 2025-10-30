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
#' @return A `S4Vectors::DataFrame` containing the validated metadata.
#' 
#' @importFrom S4Vectors DataFrame
#'
#' @export
validate_metadata <- function(metadata_df, se) {
  
  # Check class
  if (!inherits(metadata_df, "data.frame") && !inherits(metadata_df, "S4Vectors::DataFrame")) {
    stop("`metadata_df` must be a data.frame or a S4Vectors::DataFrame.")
  }
  
  # Check rownames exist
  if (is.null(rownames(metadata_df))) {
    stop("`metadata_df` must have rownames.")
  }
  
  # Check rownames match colnames of SE
  if (!all(sort(rownames(metadata_df)) == sort(colnames(se)))) {
    stop("Rownames of `metadata_df` must match colnames of the SummarizedExperiment object.")
  }
  
  # Return as S4 DataFrame
  return(S4Vectors::DataFrame(metadata_df))
}