#' Prepare Workspace
#'
#' This function prepares the workspace by loading various data files and assigning them to the global environment.
#'
#' @param params A list containing the following elements:
#'   \itemize{
#'     \item \code{sample_metadata_file}: Path to the experiment metadata file (xlsx format).
#'     \item \code{load_gse}: Logical, whether to load the GSE object (default is FALSE).
#'     \item \code{load_complete_dds}: Logical, whether to load the complete DDS object (default is FALSE).
#'     \item \code{complete_dds_source}: Path to the complete DDS source file.
#'   }
#'
#' @return This function does not return a value. It assigns the loaded data to the global environment.
#'
#' @details The function performs the following steps:
#'   \itemize{
#'     \item Checks if the experiment metadata file exists. If not, it stops with an error.
#'     \item Loads the experiment metadata from the specified file and assigns it to the global environment as \code{experiment_metadata}.
#'     \item If \code{load_gse} is TRUE, attempts to load the GSE object from "analyses_data/gse.RDS" and assigns it to the global environment as \code{gse}.
#'     \item If \code{load_complete_dds} is TRUE, attempts to load the complete DDS object from the specified source file and assigns it to the global environment as \code{complete_dds}.
#'     \item Checks if the annotation files ("anns.RDS" and "anns.RDS") exist in the "analyses_data" directory. If they exist, loads them and assigns them to the global environment as \code{anns} and \code{anns}, respectively.
#'   }
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   sample_metadata_file = "path/to/metadata.xlsx",
#'   load_gse = TRUE,
#'   load_complete_dds = TRUE,
#'   complete_dds_source = "path/to/complete_dds.RDS"
#' )
#' prepare_workspace(params)
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom purrr pluck
#' 
#' @export
prepare_workspace <- function(params) {
    if (purrr::pluck(params, "workflow", .default = "dds_gse") == "dds_gse") { 

      if (!dir.exists("analyses_data")) {
        dir.create("analyses_data")
        message("Created 'analyses_data' directory")
      }
      
      tryCatch({
        experiment_metadata <- data.frame(readxl::read_xlsx(params$sample_metadata_file, col_names = TRUE), row.names = 1)
        assign("experiment_metadata", experiment_metadata, envir = parent.frame())
        message("experiment metadata loaded and saved in the workspace as dataframe with name 'experiment_metadata'")
      }, error = function(e) {
        stop("Failed to load experiment metadata: ", e$message)
      })
    
    if (pluck(params, "load_gse", .default = FALSE)) {
        tryCatch({
          gse <- readRDS(file.path("analyses_data", "gse.RDS"))
          assign("gse", gse, envir = parent.frame())
          message("GSE object loaded and saved in the workspace as 'gse'")
        }, error = function(e) {
          warning("Failed to load gse: ", e$message)
        })
    } else message("'load_gse' set to false")

    if (pluck(params, "load_complete_dds", .default = FALSE)) {
        tryCatch({
          complete_dds <- readRDS(file.path(params$complete_dds_source))
          assign("complete_dds", complete_dds, envir = parent.frame())
          message("main dds object loaded and saved in the workspace as DESeqDataSet with name 'complete_dds'")
        }, error = function(e) {
          warning("Failed to load complete_dds: ", e$message)
        })
    } else message("'load_complete_dds' set to false")

  } else if (pluck(params, "workflow", .default = "dds_gse") == "dds") {

    tryCatch({
      experiment_metadata <- data.frame(readxl::read_xlsx(params$sample_metadata_file, col_names = TRUE), row.names = 1)
      assign("experiment_metadata", experiment_metadata, envir = parent.frame())
      message("experiment metadata loaded and saved in the workspace as dataframe with name 'experiment_metadata'")
    }, error = function(e) {
      stop("Failed to load experiment metadata: ", e$message)
    })

    tryCatch({
      se <- loadRDS(file.path("analyses_data", "se.RDS"))
      assign("se", se, envir = parent.frame())
      message("se object loaded and saved in the workspace as 'se'")
    }, error = function(e) {
      warning("Failed to load se: ", e$message)
    })

  } else {
    stop("Only dds_gse, dds, and se_vdx workflows are supported")
  }

  anns_path <- "analyses_data/anns.RDS"
  anno_df_path <- "analyses_data/anns.RDS"

  if (file.exists(anns_path)) {
    anns <- readRDS(anns_path)
    assign("anns", anns, envir = parent.frame())
  } else {
    warning("anns not loaded!")
  }

  if (file.exists(anno_df_path)) {
    anns <- readRDS(anno_df_path)
    assign("anns", anns, envir = parent.frame())
  } else {
    warning("anns not loaded!")
  }

}

