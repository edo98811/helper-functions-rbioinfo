#' Generate a DataTable for Differentially Expressed Genes
#'
#' This function creates a DataTable for visualizing differentially expressed genes 
#' from a given contrast within a results set. If no differentially expressed genes 
#' are found, a message is displayed suggesting to increase the FDR threshold.
#'
#' @param contrast A character string specifying the contrast name to retrieve results for.
#' @param myresuSet A list containing results sets, where each element corresponds to a contrast.
#'        Each contrast should contain a data frame named `etbl_res_DE` with the results.
#' @param report Logical. If `TRUE`, the function outputs the DataTable in a format suitable 
#'        for inclusion in reports (e.g., using `knitr::knit_print`). If `FALSE`, the DataTable 
#'        is printed directly to the console.
#'
#' @return The input `myresuSet` list is returned unchanged.
#'
#' @examples
#' # Example usage:
#' # Assuming `myresuSet` is a list containing results for contrasts:
#' # myresuSet <- list(
#' #   contrast1 = list(etbl_res_DE = data.frame(gene = c("gene1", "gene2"), logFC = c(1.2, -0.8))),
#' #   contrast2 = list(etbl_res_DE = NULL)
#' # )
#' table_DE("contrast1", myresuSet, report = FALSE)
#'
#' @importFrom DT datatable
#' @importFrom htmltools tags
#' @importFrom knitr knit_print
#' @export
table_DE <- function(contrast, myresuSet, report = FALSE) {

  # Handle results
  if (is.null(myresuSet[[contrast]]$etbl_res_DE)) {
    cat(paste0("No differentially expressed genes found for contrast ", contrast, ". Consider increasing the FDR threshold."))
  } else {
    if (report) {
      cat(knitr::knit_print(DT::datatable(
        myresuSet[[contrast]]$etbl_res_DE,
        escape = FALSE,
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; color:black; font-size: 2em',
          contrast
        )
      )))
    } else {
      print(DT::datatable(
        myresuSet[[contrast]]$etbl_res_DE,
        escape = FALSE,
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; color:black; font-size: 2em',
          contrast
        )
      ))
    }
  }
  
  return(myresuSet)
}
