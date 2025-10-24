
#' Plot Number of Reads per Sample
#'  
#' This function creates a bar plot showing the total number of reads for each sample in a DESeq2 dataset.
#' @param dds A DESeqDataSet object containing the count data.
#' @return A ggplot2 object representing the bar plot of read counts per sample.
#' @examples
#' \dontrun{
#' library(DESeq2)
#' dds <- DESeqDataSetFromMatrix(countData = count_matrix,
#'                               colData = col_data,
#'                               design = ~ condition)
#' plot <- plot_n_reads(dds)
#' print(plot)
#' }
#' @import ggplot2
#' @export
plot_n_reads <- function(dds) {
  
  myd <- data.frame(
    counts = colSums(counts(dds)),
    sample = colnames(dds)
  )
  ggplot(myd, aes(x = sample, weight = counts, fill = sample)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_bw() +
    coord_flip() +
    theme(legend.position = "none")
}
