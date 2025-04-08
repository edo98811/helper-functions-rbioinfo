# functions to make results nicer
createLinkGO <- function(val) {
  sprintf('<a href="http://amigo.geneontology.org/amigo/term/%s" target="_blank" class="btn btn-primary">%s</a>',val,val)
}

createLinkENS  <- function(val, species="Mus_musculus") {
  paste0('<a href="http://www.ensembl.org/',species,'/Gene/Summary?g=',val,'" target="_blank" class="btn btn-primary">',val,'</a>')
}

createLinkGeneSymbol <- function(val) {
  # possibilities:
  # ncbi
  # genecards
  paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',val,'[sym]" target="_blank" class="btn btn-primary">',val,'</a>')
}
# sections present in myResuSet
#  - res_DESeq -> DESeq2 results object
#  - maplot_res -> MA plot of DESeq2 results
#  - tbl_res_all -> Data frame of all DESeq2 results
#  - tbl_res_DE -> Data frame of significant DESeq2 results
#  - etbl_res_DE -> Interactive table of significant DESeq2 results
#  - clupro_tbl -> ClusterProfiler results table (not implemented in this script)
#  - topGO_tbl -> topGO results table (not implemented in this script)

alltheresults <- function(resuSet, dds_obj, contrast, FDR, anno_df, anns, species) {

  # id_contrast <- paste0(contrast[2],"_vs_",contrast[3])
  id_contrast <- contrast
  resuSet[[id_contrast]] <- list()
  
  # mycoef <- resultsNames(dds_obj)[2]
  mycoef <- contrast
  
  message("Extracting results...")
  resuSet[[id_contrast]][["res_DESeq"]] <- results(dds_obj,name = mycoef, alpha = FDR)
  message("Performing LFC shrinkage...")
  resuSet[[id_contrast]][["res_DESeq"]] <- lfcShrink(dds_obj,coef = mycoef,res = resuSet[[id_contrast]][["res_DESeq"]], type = "apeglm") 
  resuSet[[id_contrast]][["res_DESeq"]]$SYMBOL <- anno_df$gene_name[match(rownames(resuSet[[id_contrast]][["res_DESeq"]]), anno_df$gene_id)]
  
  message("Summary MAplot...")
  summary(resuSet[[id_contrast]][["res_DESeq"]])
  # resuSet[[id_contrast]][["maplot_res"]] <- 
  #   ideal::plot_ma(resuSet[[id_contrast]][["res_DESeq"]], ylim = c(-2,2), title = id_contrast, FDR = FDR) # commented because it makes my computer crash....
  resuSet[[id_contrast]][["maplot_res"]] <- NULL
    # plotMA(resuSet[[id_contrast]][["res_DESeq"]], ylim = c(-2, 2), main = id_contrast, alpha = FDR)
  
  message("Extracting tables...")
  resuSet[[id_contrast]][["tbl_res_all"]] <- deseqresult2df(resuSet[[id_contrast]][["res_DESeq"]])
  resuSet[[id_contrast]][["tbl_res_all"]]$geneSymbol <- anno_df$gene_name[match(resuSet[[id_contrast]][["tbl_res_all"]]$id, anno_df$gene_id)]
  resuSet[[id_contrast]][["tbl_res_all"]]$description <- anns$description[match(resuSet[[id_contrast]][["tbl_res_all"]]$id, anns$ensembl_gene_id)]
  
  message("Extracting DEtables...")
  resuSet[[id_contrast]][["tbl_res_DE"]] <- deseqresult2df(resuSet[[id_contrast]][["res_DESeq"]],FDR = FDR)
  resuSet[[id_contrast]][["tbl_res_DE"]]$geneSymbol <- anno_df$gene_name[match(resuSet[[id_contrast]][["tbl_res_DE"]]$id, anno_df$gene_id)]
  resuSet[[id_contrast]][["tbl_res_DE"]]$description <- anns$description[match(resuSet[[id_contrast]][["tbl_res_DE"]]$id, anns$ensembl_gene_id)]
  resuSet[[id_contrast]][["tbl_res_DE"]]$chromosome_name <- anns$chromosome_name[match(resuSet[[id_contrast]][["tbl_res_DE"]]$id, anns$ensembl_gene_id)]
  
  if(nrow(resuSet[[id_contrast]][["tbl_res_DE"]]) > 0) {
    message("Generating interactive DEtable...")
    resuSet[[id_contrast]][["etbl_res_DE"]] <- resuSet[[id_contrast]][["tbl_res_DE"]]
    resuSet[[id_contrast]][["etbl_res_DE"]]$id <- createLinkENS(resuSet[[id_contrast]][["etbl_res_DE"]]$id, species = species)
    resuSet[[id_contrast]][["etbl_res_DE"]]$geneSymbol <- createLinkGeneSymbol(resuSet[[id_contrast]][["etbl_res_DE"]]$geneSymbol)

    num_cols <- sapply(resuSet[[id_contrast]][["etbl_res_DE"]], is.numeric)
    resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols] <- lapply(resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols], round, 4)

  }
  
  mybuttons <- c('copy', 'csv', 'excel', 'pdf', 'print')
  datatable(resuSet[[id_contrast]][["etbl_res_DE"]],caption = paste0(id_contrast,", DE genes"),escape=F, extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = mybuttons))
  
  return(resuSet)
}