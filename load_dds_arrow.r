library(arrow)

load_dds_from_arrow <- function(input_dir) {
  # Read metadata
  row_data <- read_feather(file.path(input_dir, "row_data.arrow"))
  col_data <- read_feather(file.path(input_dir, "col_data.arrow"))

  # Read count matrix
  counts <- as.matrix(read_feather(file.path(input_dir, "counts.arrow")))

  # Read design formula
  design_formula <- as.formula(readLines(file.path(input_dir, "design.txt")))

  # Reconstruct DESeqDataSet
  dds <- DESeqDataSetFromMatrix(countData = counts, colData = col_data, design = design_formula)

  rowData(dds) <- row_data  # Restore row metadata

  return(dds)
}