


prepare_workspace <- function(params) {
  anns_path <- file.path("analyses_data", "anns.RDS")
  anno_df_path <- file.path("analyses_data", "anno_df.RDS")

  tryCatch({
    experiment_metadata <- readxl::read_xlsx(params$experiment_metadata_file, col_names = TRUE)
    assign("experiment_metadata", experiment_metadata, envir = parent.frame())
    message("experiment metadata loaded and saved in the workspace as dataframe with name 'experiment_metadata'")
  }, error = function(e) {
    warning("Failed to load experiment metadata: ", e$message)
  })

  if (purrr::pluck(params, "load_gse", .default = FALSE)) {
    tryCatch({
      gse <- readRDS(file.path("analyses_data", "gse.RDS"))
      assign("gse", gse, envir = parent.frame())
      message("GSE object loaded and saved in the workspace as 'gse'")
    }, error = function(e) {
      warning("Failed to load gse: ", e$message)
    })
  } else warning("'load_gse' set to false")


  if (purrr::pluck(params, "load_complete_dds", .default = FALSE)) {
    tryCatch({
      complete_dds <- readRDS(file.path(params$complete_dds_source))
      assign("complete_dds", complete_dds, envir = parent.frame())
      message("main dds object loaded and saved in the workspace as DESeqDataSet with name 'complete_dds'")
    }, error = function(e) {
      warning("Failed to load complete_dds: ", e$message)
    })
  } else warning("'load_complete_dds' set to false")

  if (file.exists(anns_path) && file.exists(anno_df_path)) {
    anns <- readRDS(anns_path)
    anno_df <- readRDS(anno_df_path)

    assign("anns", anns, envir = parent.frame())
    assign("anno_df", anno_df, envir = parent.frame())

  } else warning("anns and anno_df not loaded! create them after setting up the dds object")

}

