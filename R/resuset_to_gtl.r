#' Convert a result set to a GeneTonicList
#'
#' This function converts a result set to a GeneTonicList for different contrasts.
#'
#' @param myresuSet A list containing the result sets.
#' @param result_name A character string specifying the name of the result to be used.
#' @param dds A DESeqDataSet object.
#' @param anno_df A data frame containing annotation information.
#' @param enrich_algo A character string specifying the enrichment algorithm to be used. 
#'        Options are "not_given", "topGO_tbl", or "clupro_tbl". Default is "not_given".
#'
#' @return A GeneTonicList object containing the DESeqDataSet, DE results, enrichment results, and annotation data.
#' @export
#'
#' @examples
#' # Example usage:
#' myresuSet <- list(
#'   contrast1 = list(
#'     res_DESeq = res_de_example,
#'     topGO_tbl = topgo_tbl_example,
#'     clupro_tbl = clupro_tbl_example
#'   )
#' )
#' result_name <- "contrast1"
#' dds <- dds_example
#' anno_df <- anno_df_example
#' gtl <- resuset_to_gtl(myresuSet, result_name, dds, anno_df, enrich_algo = "topGO_tbl")
# function to easily generate GeneTonicLists for different contrasts
resuset_to_gtl <- function(myresuSet, result_name, dds, anno_df, enrich_algo = "not_given") {
  dds_gtl <- dds
  anno_df_gtl <- anno_df
  res_de_gtl <- myresuSet[[result_name]][["res_DESeq"]]
  
  if (enrich_algo == "topGO_tbl") {
    res_enrich_gtl <- GeneTonic::shake_topGOtableResult(myresuSet[[result_name]][["topGO_tbl"]])
  } else if (enrich_algo == "clupro_tbl") {
    res_enrich_gtl <- GeneTonic::shake_enrichResult(myresuSet[[result_name]][["clupro_tbl"]])
  } else res_enrich_gtl <- NULL
  
  gtl_assembled <- GeneTonicList(
    dds = dds_gtl,
    res_de = res_de_gtl,
    res_enrich = res_enrich_gtl,
    annotation_obj = anno_df_gtl
  )
  
  return(gtl_assembled)
}