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
  library("GeneTonic")
})
message("- Done!")

message("--- Generating objects for the testing setup...")

# Load the macrophage dataset ---------------------------------------------------

data(gse)
rownames(gse) <- substr(rownames(gse), 1, 15) # per togliere tutto quello dopo il punto

# dds --------------------------------------------------------------------------
dds <- DESeqDataSet(gse, design = ~ condition)

keep <- rowSums(counts(dds) >= 10) >= 6
dds <- dds[keep, ]

dds <- DESeq(dds)


# limma ------------------------------------------------------------------------
condition <- factor(colData(gse)[, "condition_name"])

design <- model.matrix(~0 + condition)
contrast.matrix <- makeContrasts(

  # Comparisons vs Control
  IFNg_vs_naive = conditionIFNg - conditionnaive,
  IFNg_SL1344_vs_naive = conditionIFNg_SL1344 - conditionnaive,
  SL1344_vs_naive = conditionSL1344 - conditionnaive,

  # Pairwise comparisons between conditions
  IFNg_vs_SL1344 = conditionIFNg - conditionSL1344,
  IFNg_SL1344_vs_IFNg = conditionIFNg_SL1344 - conditionIFNg,
  IFNg_SL1344_vs_SL1344 = conditionIFNg_SL1344 - conditionSL1344,

  levels = design
)

d0 <- DGEList(assay(gse))
d0 <- calcNormFactors(d0)

cutoff <- 1
drop <- which(apply(cpm(d0), 1, max) < cutoff)
d <- d0[-drop,]

matrix_to_fit_model_to <- voom(d, design, plot = F)
# matrix_to_fit_model_to <- assay(se) # Use the assay data from the SummarizedExperiment object

fit <- lmFit(matrix_to_fit_model_to, design) # Fit the linear model
fit2 <- contrasts.fit(fit, contrast.matrix) # Apply the contrast matrix

# View(as.data.frame(colData(se)))
fit2 <- eBayes(fit2) # Empirical Bayes moderation

# annotation -------------------------------------------------------------------
anns <- data.frame(
  gene_id = rownames(dds),
  gene_name = mapIds(org.Hs.eg.db,
    keys = rownames(dds),
    column = "SYMBOL",
    keytype = "ENSEMBL"
  ),
  stringsAsFactors = FALSE,
  row.names = rownames(dds)
)
# alternatively, one could use the wrapper in ...
# anns <- pcaExplorer::get_annotation_orgdb(dds, "org.Hs.eg.db", "ENSEMBL")

# res_de -----------------------------------------------------------------------
# vst_macrophage <- vst(dds)
res_macrophage_IFNg_vs_naive_dds <- results(dds,
  contrast = c("condition", "IFNg", "naive"),
  lfcThreshold = 1, alpha = 0.05
)
summary(res_macrophage_IFNg_vs_naive_dds)

res_macrophage_IFNg_vs_naive_dds$SYMBOL <- rowData(dds)$SYMBOL

## using counts and average transcript lengths from tximeta

# dds_unnormalized <- dds



# res_de_limma -------------------------------------------------
res_macrophage_IFNg_vs_naive_limma <- topTable(fit2, coef = "IFNg_vs_naive", adjust = "fdr", number = Inf, confint = TRUE)


# res_enrich -------------------------------------------------------------------
de_symbols_IFNg_vs_naive <- res_macrophage_IFNg_vs_naive_dds[(!(is.na(res_macrophage_IFNg_vs_naive_dds$padj))) & (res_macrophage_IFNg_vs_naive_dds$padj <= 0.05), "SYMBOL"]
bg_ids <- rowData(dds)$SYMBOL[rowSums(counts(dds)) > 0]

# res_enrich_limma -------------------------------------------------


save(dds, res_macrophage_IFNg_vs_naive_dds, limma_model, topgoDE_macrophage_IFNg_vs_naive, anns, file ="quick_startup.RData")

message("--- Test setup script completed!")

# View(as.data.frame(rowData(gse)))