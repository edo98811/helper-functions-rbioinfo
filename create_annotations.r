create_annotations <- function(dds) {

  if (!inherits(dds, "DESeqDataSet")) stop("dds object is not of class 'DDSdataset'. Please provide a valid dds object.")

  anns_path <- file.path("analyses_data", "anns.RDS")
  anno_df_path <- file.path("analyses_data", "anno_df.RDS")

  annotations <- annotation_datasets(dds)
  anns <- annotations$anns
  anno_df <- annotations$anno_df
#   saveRDS(anns, anns_path)
#   saveRDS(anno_df, anno_df_path)
  remove(annotations)

  assign("anns", anns, envir = parent.frame())
  assign("anno_df", anno_df, envir = parent.frame())
}