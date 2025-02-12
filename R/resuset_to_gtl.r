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