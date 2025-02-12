add_symbols <- function(dds, annotation_dataset) {
    rowData(dds)$SYMBOL <- annotation_dataset$gene_name[
        match(rownames(
            rowData(dds)), 
            annotation_dataset$gene_id)
        ]
    return(dds)
}