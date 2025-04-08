#' Plot PCA
#'
#' This function performs Principal Component Analysis (PCA) on a given DGEList object and plots the results using ggplot2.
#'
#' @param dge_list A DGEList object containing the count data and sample information.
#' @param color_encoding A character string specifying the column name in the sample information to be used for coloring the points. Default is "group".
#' @param legend_title A character string specifying the title of the legend.
#' @param matrix_type A character string specifying the type of matrix to be used for PCA. Options are "logCPM", "getOffset", "getCounts", and "log2". Default is "getCounts".
#' @param polygon A logical value indicating whether to draw polygons around groups. Default is FALSE.
#' @param plot_title A character string specifying the title of the plot. Default is "PCA plot".
#'
#' @import edgeR
#' @import ggplot2
#' @import ggfortify
#' @import ggrepel
#' 
#' @return A ggplot2 object representing the PCA plot.
#' @export
#'
#' @examples
#' \dontrun{
#' dge_list <- DGEList(counts = counts_matrix, samples = sample_info)
#' plot_pca(dge_list, color_encoding = "group", legend_title = "Group", matrix_type = "logCPM", polygon = TRUE, plot_title = "PCA of Samples")
#' }
plot_pca <- function(dge_list, color_encoding = "group", legend_title, matrix_type = "getCounts", polygon = FALSE, plot_title = "PCA plot") {
    
    # Get desired abundance matrix 
    if (matrix_type == "logCPM") matrix_to_pca <- cpm(dge_list, log = TRUE)
    else if (matrix_type == "getOffset") matrix_to_pca <- getOffset(dge_list)
    else if (matrix_type == "getCounts") matrix_to_pca <- getCounts(dge_list)
    else if (matrix_type == "log2") matrix_to_pca <- log2(getCounts(dge_list) + 1)  
    else stop("non valid matrix type")

    # Perform PCA
    pca <- prcomp(t(matrix_to_pca), scale. = TRUE)
    if (!(color_encoding %in% colnames(dge_list$samples) || color_encoding == "biological_sample")) stop("non valid color encoding")

    # Create a data frame with PCA results and metadata_group_column information
    pca_data <- data.frame(pca$x, group = dge_list$samples$group)
    if (color_encoding %in% colnames(dge_list$samples)) pca_data[[color_encoding]] <- dge_list$samples[[color_encoding]]

    sample_name <- rownames(dge_list$samples)
    experiment_metadata$biological_replicate_name <- apply(experiment_metadata, 1, function(row) paste0(row[1], "_", row[2]))
    pca_data$biological_sample <- experiment_metadata[match(rownames(pca_data), rownames(experiment_metadata)),]$biological_replicate_name

    # Plot PCA
    p <- ggplot(pca_data, aes_string(x = "PC1", y = "PC2", color = color_encoding)) +
        geom_point(size = 3) +
        geom_text_repel(aes(label = rownames(pca_data)), size = 3) +
        labs(title = plot_title, x = "PC1", y = "PC2", color = legend_title, fill = legend_title) +
        theme_minimal() +
        theme(plot.title = element_text(size = 25))
    
    if (polygon) {
        p <- p + geom_polygon(aes_string(group = color_encoding, fill = color_encoding), alpha = 0.3, linetype = "dashed")
    }
    
    print(p)
}