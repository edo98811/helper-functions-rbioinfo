#' Plot Expression
#'
#' This function generates a boxplot of logCPM values for each group, with the option to use gene names or protein IDs.
#'
#' @param protein A character string specifying the protein or gene to plot.
#' @param se_object A SummarizedExperiment object containing the expression data.
#' @param use_gene_name A logical value indicating whether to use gene names (default is FALSE).
#' @param count_matrix An optional matrix of counts. If NULL, the count matrix from the SummarizedExperiment object is used.
#'
#' @return A ggplot object representing the expression plot.
#' @import ggplot2
#' @import ggforce
#' @import ggrepel
#' @importFrom SummarizedExperiment rowData assays colData
#'
#' @examples
#' # Assuming `se` is a SummarizedExperiment object and "GeneA" is a valid protein/gene ID
#' plot_expression("GeneA", se, use_gene_name = TRUE)
# Boxplot of logCPM values for each group
#' @export
plot_expression <- function(protein, se_object, use_gene_name = FALSE, count_matrix = NULL) {

  # Use correct plot name
  if (use_gene_name) {
    plot_name <- rowData(se_object)[protein, "gene_symbol"]
  } else {
    plot_name <- protein
  }
  if (is.null(count_matrix)) {
    count_matrix <- assays(se_object)$counts
  } else if (!is.matrix(count_matrix)) stop("count matrix must be either null or a matrix of counts")

  plotting_data <- t(count_matrix[protein,, drop = FALSE])
  rownames(plotting_data) <- colnames(count_matrix)
  colnames(plotting_data) <- "Protein_ID"

  plotting_data <- merge(plotting_data, colData(se_object), by="row.names")
  plotting_data$group <- as.factor(plotting_data$group)
  
  ggplot(plotting_data, aes(x = group, y = Protein_ID, fill = group)) +
    # geom_boxplot(outliers = FALSE) +
    # geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
    ggforce::geom_sina(aes(color = group), size = 1.5) + 
    ggrepel::geom_text_repel(aes(label = Row.names), size = 2.4) +
    geom_boxplot(alpha=0.3) +
    # scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 21)
    ) +
    ggtitle(paste("Expression plot for ", plot_name)) +
    xlab("") + 
    ylab(protein)
}