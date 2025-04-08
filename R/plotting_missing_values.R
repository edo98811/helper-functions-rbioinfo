#' Plot Missing Values in a Dataset
#'
#' This function visualizes the missing values in a dataset using the `visdat` package.
#'
#' @param se A `SummarizedExperiment` object or a matrix/data frame containing the dataset.
#' @return A ggplot object visualizing the missing values in the dataset.
#' @importFrom visdat vis_miss
#' @importFrom ggplot2 theme element_blank ggtitle xlab ylab
#' @export
#' @examples
#' # Example with a SummarizedExperiment object
#' library(SummarizedExperiment)
#' se <- SummarizedExperiment(assays = list(counts = matrix(rnorm(100), nrow = 10)))
#' plot_missing_values(se)
#'
#' # Example with a matrix
#' mat <- matrix(rnorm(100), nrow = 10)
#' plot_missing_values(mat)
plot_missing_values <- function(se) {
    if (inherits(se, "SummarizedExperiment")) {
        count_matrix <- assay(se)
    } else {
        count_matrix <- se
    }
    

    visdat::vis_miss(as.data.frame(count_matrix), warn_large_data = FALSE, show_perc_col = TRUE) + 
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        ggtitle("Missing values in the dataset") +
        xlab("Samples") +
        ylab("Proteins")

}
