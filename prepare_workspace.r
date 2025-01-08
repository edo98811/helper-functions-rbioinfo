


prepare_workspace <- function(params, experiment_metadata_file = "samplesDesign_paul_AT2.xlsx") {
  anns_path <- file.path("analyses_data", "anns.RDS")
  anno_df_path <- file.path("analyses_data", "anno_df.RDS")

  tryCatch({
    experiment_metadata <- readxl::read_xlsx(params$experiment_metadata_file, col_names = TRUE)
    assign("experiment_metadata", experiment_metadata, envir = parent.frame())
  }, error = function(e) {
    stop("Failed to load experiment metadata: ", e$message)
  })

  tryCatch({
    gse <- readRDS(file.path("analyses_data", "gse.RDS"))
    assign("gse", gse, envir = parent.frame())
  }, error = function(e) {
    stop("Failed to load gse: ", e$message)
  })


  # 

  if (file.exists(anns_path) && file.exists(anno_df_path)) {
    anns <- readRDS(anns_path)
    anno_df <- readRDS(anno_df_path)

    assign("anns", anns, envir = parent.frame())
    assign("anno_df", anno_df, envir = parent.frame())

  } else warning("anns and anno_df not loaded! create them after setting up the dds object")

}

