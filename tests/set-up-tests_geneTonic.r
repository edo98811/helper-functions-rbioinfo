library("rbioinfoHelper")

message("--- Loading packages...")
suppressPackageStartupMessages({
  library("macrophage")
  library("DESeq2")
  library("org.Hs.eg.db")
  library("AnnotationDbi")
  library("clusterProfiler")
  library("limma")
  library("edgeR")
})
message("- Done!")

message("--- Generating objects for the testing setup...")

data(gse)

# dds --------------------------------------------------------------------------

dds_macrophage <- DESeqDataSet(gse, design = ~ line + condition)
rownames(dds_macrophage) <- substr(rownames(dds_macrophage), 1, 15)

# limma -------------------------------------------------------------------

d0_macrophage <- DGEList(assay(gse))
d0_macrophage <- calcNormFactors(d0_macrophage)

cutoff <- 1
drop <- which(apply(cpm(d0), 1, max) < cutoff)
d_macrophage <- d0_macrophage[-drop,]

matrix_to_fit_model_to <- voom(d_macrophage, design, plot = F)
# matrix_to_fit_model_to <- assay(se) # Use the assay data from the SummarizedExperiment object

fit <- lmFit(matrix_to_fit_model_to, design) # Fit the linear model
fit2 <- contrasts.fit(fit, contrast.matrix) # Apply the contrast matrix

# View(as.data.frame(colData(se)))
fit2 <- eBayes(fit2) # Empirical Bayes moderation

# annotation -------------------------------------------------------------------
anns <- data.frame(
  gene_id = rownames(dds_macrophage),
  gene_name = mapIds(org.Hs.eg.db,
    keys = rownames(dds_macrophage),
    column = "SYMBOL",
    keytype = "ENSEMBL"
  ),
  stringsAsFactors = FALSE,
  row.names = rownames(dds_macrophage)
)
# alternatively, one could use the wrapper in ...
# anns <- pcaExplorer::get_annotation_orgdb(dds_macrophage, "org.Hs.eg.db", "ENSEMBL")

# res_de -----------------------------------------------------------------------
## using counts and average transcript lengths from tximeta
# keep <- rowSums(counts(dds_macrophage) >= 10) >= 6
dds_macrophage <- dds_macrophage[keep, ]
# dds_unnormalized <- dds_macrophage

# dds_macrophage <- DESeq(dds_macrophage)
# vst_macrophage <- vst(dds_macrophage)
# res_macrophage_IFNg_vs_naive_dds <- results(dds_macrophage,
#   contrast = c("condition", "IFNg", "naive"),
#   lfcThreshold = 1, alpha = 0.05
# )
summary(res_macrophage_IFNg_vs_naive_dds)
res_macrophage_IFNg_vs_naive_dds$SYMBOL <- rowData(dds_macrophage)$SYMBOL

# res_de_limma -------------------------------------------------
res_macrophage_IFNg_vs_naive_limma <- topTable(fit2, coef = c("condition", "IFNg", "naive"), adjust = "fdr", number = Inf, confint = TRUE)


# res_enrich -------------------------------------------------------------------
de_symbols_IFNg_vs_naive <- res_macrophage_IFNg_vs_naive_dds[(!(is.na(res_macrophage_IFNg_vs_naive_dds$padj))) & (res_macrophage_IFNg_vs_naive_dds$padj <= 0.05), "SYMBOL"]
bg_ids <- rowData(dds_macrophage)$SYMBOL[rowSums(counts(dds_macrophage)) > 0]

# library("topGO")
# topgoDE_macrophage_IFNg_vs_naive <-
#   pcaExplorer::topGOtable(de_symbols_IFNg_vs_naive,
#                           bg_ids,
#                           ontology = "BP",
#                           mapping = "org.Hs.eg.db",
#                           geneID = "symbol",
#                           topTablerows = 500)
# write.table(topgoDE_macrophage_IFNg_vs_naive,
#             "inst/extdata/topgotable_res_IFNg_vs_naive.txt",
#             sep = "\t")
# topgoDE_macrophage_IFNg_vs_naive <-
#   read.table(system.file("extdata", "topgotable_res_IFNg_vs_naive.txt", package = "GeneTonic"),
#     stringsAsFactors = FALSE
#   )
# message("- Done!")

# message("--- Running enrichGO...")
# ego_IFNg_vs_naive <- enrichGO(
#   gene = de_symbols_IFNg_vs_naive,
#   universe = bg_ids,
#   keyType = "SYMBOL",
#   OrgDb = org.Hs.eg.db,
#   ont = "BP",
#   pAdjustMethod = "BH",
#   pvalueCutoff = 0.01,
#   qvalueCutoff = 0.05,
#   readable = FALSE
# )

# message("--- Running gseGO...")
# sorted_genes <- sort(
#   setNames(res_macrophage_IFNg_vs_naive_dds$log2FoldChange,
#            res_macrophage_IFNg_vs_naive_dds$SYMBOL),
#   decreasing = TRUE
# )

# sorted_genes_noNA <- sort(sorted_genes[!is.na(names(sorted_genes))], decreasing = TRUE)
# sorted_genes_noNA_nodupes <- sorted_genes_noNA[!duplicated(names(sorted_genes_noNA))]

# suppressWarnings({
#   gsego_IFNg_vs_naive <- gseGO(
#     geneList = sorted_genes_noNA_nodupes,
#     ont = "BP",
#     OrgDb = org.Hs.eg.db,
#     keyType = "SYMBOL",
#     minGSSize = 10,
#     maxGSSize = 500,
#     pvalueCutoff = 0.05,
#     verbose = TRUE
#   )
# })
# res_enrich_limma -------------------------------------------------


# save(dds_macrophage, res_macrophage_IFNg_vs_naive_dds, vst_macrophage, topgoDE_macrophage_IFNg_vs_naive, anns, ego_IFNg_vs_naive, file ="quick_startup.RData")

# unnormalized dds -------------------------------------------------
# dds_macrophage <- DESeqDataSet(gse, design = ~ line + condition)
# load("/Users/fede/Development/GeneTonic/quick_startup.RData")
dds_unnormalized <- dds_macrophage
assays(dds_unnormalized)[["normalizationFactors"]] <- NULL
res_enrich_IFNg_vs_naive <- shake_topGOtableResult(topgoDE_macrophage_IFNg_vs_naive)[1:200, ]
message("- Done!")

message("--- Test setup script completed!")
