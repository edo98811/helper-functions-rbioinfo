subset_gse <- function(gse_to_subset) {

  # gse_to_subset <- gse_to_subset[, colData(gse_to_subset)$group != "WT"]
  # gse_to_subset <- gse_to_subset[, colData(gse_to_subset)$group %in% c("WT", "KO")]

  return(gse_to_subset)
}