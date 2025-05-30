#' Perform Gene Ontology Enrichment Analysis with topGO
#'
#' This function performs Gene Ontology (GO) enrichment analysis using the `topGO` package.
#' It identifies enriched biological pathways for a given contrast based on differentially expressed genes.
#' @param params A list containing the following elements:
#'   \describe{
#'     \item{\code{species}}{A character string specifying the species. Valid values are "Mm" for mouse and "Hs" for human.}
#'     \item{\code{run_computations}}{A logical value indicating whether computations are run.}
#'   }
#' @param contrast A character string specifying the contrast name for the analysis.
#' @param se A `SummarizedExperiment` object containing gene expression data.
#' @param myresuSet A list containing results of differential expression analysis, including a table of DE genes (`tbl_res_DE`).
#' @param report Logical. If `TRUE`, the results are displayed in a report-friendly format using `knitr::knit_print`. Default is `FALSE`.
#'
#' @details
#' The function first identifies expressed genes based on the `SummarizedExperiment` object. It then performs GO enrichment analysis
#' using the `topGOtable` function from the `pcaExplorer` package. The enrichment is performed for the "Biological Process" (BP) ontology.
#' The function handles cases where no differentially expressed genes are found or when computations are skipped based on the `params$run_computations` flag.
#'
#' @return The function updates the `myresuSet` list with the enrichment results (`topGO_tbl`) for the specified contrast.
#' If no enriched pathways are found, a message is displayed. The results are printed or displayed in a report format based on the `report` parameter.
#'
#' @examples
#' \dontrun{
#' table_topGO(
#'   contrast = "contrast1",
#'   se = mySummarizedExperiment,
#'   myresuSet = myResultsList,
#'   report = TRUE
#' )
#' }
#'
#' @importFrom pcaExplorer topGOtable
#' @importFrom DT datatable
#' @importFrom htmltools tags
#' @importFrom knitr knit_print
#' @export
table_topGO <- function(params, contrast, se, myresuSet, report = FALSE) {

  # Validate the input parameters
  if (!is.list(params) || !all(c("species", "run_computations") %in% names(params))) {
    stop("Invalid parameters provided. Please provide a list with 'species' and 'run_computations'.")
  }
  # Analysis with topGO
  expressed_ensembl_id <- rowData(se)[(rowSums(assay(se)) > 0), "ENSEMBL"]
  expressed_gene_symbol <- anns$SYMBOL[match(expressed_ensembl_id, anns$ENSEMBL)]

  mapping <- ifelse(params$species == "Mm", "org.Mm.eg.db", "org.Hs.eg.db")

  # Enrichment with topGO
  if (params$run_computations == TRUE) {
    if (nrow(myresuSet[[i]][["tbl_res_DE"]]) > 0) {
      myresuSet[[i]][["topGO_tbl"]] <- 
        pcaExplorer::topGOtable(
          DEgenes = myresuSet[[i]][["tbl_res_DE"]]$gene_symbol, 
          BGgenes = expressed_gene_symbol, 
          ontology = "BP",
          geneID = "symbol",
          addGeneToTerms = TRUE, 
          topTablerows = 500,
          mapping = mapping
        )
    } else {
      warning(paste0("No differentially expressed genes found for contrast ", contrast, ". Cannot run enrichment."))
    }
  } else {
    message(paste0("Skipping topGO enrichment for contrast ", contrast, " as run_computations is FALSE."))
  }
  # Handle results
  if (is.null(myresuSet[[contrast]]$topGO_tbl)) {
    cat(paste0("No enriched pathways found for contrast ", contrast, "\n"))
  } else {
    if (report) {
      cat(knitr::knit_print(DT::datatable(
        myresuSet[[contrast]]$topGO_tbl,
        escape = FALSE,
        rownames = TRUE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; color: black; font-size: 1.5em; font-weight: bold;',
          paste0("Enrichment Results for Contrast: ", contrast)
        )
      )))
    } else {
      print(DT::datatable(
        myresuSet[[contrast]]$topGO_tbl,
        escape = FALSE,
        rownames = TRUE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; color: black; font-size: 1.5em; font-weight: bold;',
          paste0("Enrichment Results for Contrast: ", contrast)
        )
      ))
    }
  }
}