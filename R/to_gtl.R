gtl_from_DGEList <- function(DGEList_object, topTable_results, res_enrich, annotation_obj, verbose = FALSE) {

    # as.integer(getCounts(DGEList_object))
    dds <- DEFormats::as.DESeqDataSet(DGEList_object)

    SE_from_CI <- function(ci) {
        return((ci[2] - ci[1]) / (2 * qnorm(0.975))) # 0.975 perche voglio la CI al 95% (sinistra e destra sono entrabe 0.025) 
        # (ma questo funziona solo per norm distribuition, tra l'altro)
        # qnorm: given an area, find the boundary value that determines this area.
        # https://www.rdocumentation.org/packages/limma/versions/3.28.14/topics/toptable -> from topTable CI.L and CI.R are the lower and upper bounds of the confidence interval for the log2 fold change.
    }

    topTable_results$lfcSE <- apply(topTable_results[, c("CI.L", "CI.R")], 1, SE_from_CI)

    starting_cols <- c(
        "AveExpr", # "baseMean"
        "logFC", # "log2FoldChange"
        "lfcSE", # "lfcSE"
        "P.Value", # "pvalue"
        "adj.P.Val", # "padj"
        "gene_symbol", # "SYMBOL"
        "B",
        "uniprot_id",
        "ensembl_gene_id",  # "ensembl_gene_id"
        "t",
        "CI.R",
        "CI.L")

    topTable_results <- topTable_results[, starting_cols]

    cols_to_have <- c(
        "baseMean",
        "log2FoldChange",
        "lfcSE",
        "pvalue",
        "padj",
        "SYMBOL")

    rownames(topTable_results) <- cols_to_have

    res_de <- DESeqResults(topTable_results)

    gtl <- GeneTonicList(dds,
                        res_de,
                        res_enrich,
                        annotation_obj,
                        verbose = verbose)

    return(gtl)
}