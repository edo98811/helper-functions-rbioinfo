save_dds_as_arrow <- function(dds, output_dir) {
  dir_create(output_dir)

  # Save counts matrix
  write_feather(as.data.frame(assay(dds)), file.path(output_dir, "counts.arrow"))

  # Save metadata
  write_feather(as.data.frame(rowData(dds)), file.path(output_dir, "row_data.arrow"))
  write_feather(as.data.frame(colData(dds)), file.path(output_dir, "col_data.arrow"))

  # Save design formula separately
  writeLines(as.character(design(dds)), file.path(output_dir, "design.txt"))

  message("Saved DESeqDataSet to: ", output_dir)
}