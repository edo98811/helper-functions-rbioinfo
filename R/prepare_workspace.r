#' Prepare Workspace for Bioinformatics Analysis
#'
#' This function initializes the workspace for bioinformatics analysis by loading required objects and metadata files.
#'
#' @param params A list containing the following elements:
#'   \itemize{
#'     \item \code{workflow}: A string specifying the workflow type. Supported values are \code{"se"}, \code{"se_vdx"}, \code{"se_dds"}, \code{"dds"}, or \code{"se_dds"}.
#'     \item \code{run_computations}: A logical value indicating whether computations should be run (not used in the current implementation).
#'     \item \code{analysis_folder}: A string specifying the folder where analysis results are stored.
#'     \item \code{analysis_name}: A string specifying the name of the analysis (not used in the current implementation).
#'     \item \code{metadata_file}: A string specifying the path to the metadata file (CSV or Excel format).
#'   }
#'
#' @return Logical \code{TRUE} if the workspace is successfully prepared.
#'
#' @details
#' The function performs the following tasks:
#' \itemize{
#'   \item Validates the input parameters.
#'   \item Loads RDS objects (\code{se}, \code{dds}, and \code{anns}) based on the specified workflow and directory path.
#'   \item Reads the metadata file, which can be in CSV or Excel format.
#' }
#'
#' @note
#' The function requires the \code{readxl} package to read Excel files. Ensure the package is installed if using Excel metadata files.
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   workflow = "se_dds",
#'   run_computations = TRUE,
#'   analysis_folder = "path/to/analysis_folder",
#'   analysis_name = "example_analysis",
#'   metadata_file = "path/to/metadata.csv"
#' )
#' prepare_workspace(params)
#' }
#'
#' @export
# Function: prepare_workspace
# Description: Skeleton for a function to prepare the workspace for bioinformatics analysis.

prepare_workspace <- function(params) {
  # Validate the input parameters
  if (!check_params(params)) stop("prepare_workspace: wrong parameters provided.")

  # Determine the directory path based on the provided parameters
  dir_path <- file.path("analyses_results", params$analysis_name)
  # Initialize workspace
  message("Initializing workspace...")

  # Load the source object from RDS
  object_path <- file.path(params$source_object)

  if (!file.exists(object_path)) {
    stop("The se file does not exist: ", object_path)
  } else {
    source_object <- readRDS(object_path)
    assign("source_object", source_object, envir = parent.frame())
    message(sprintf("Loaded 'source_object' from %s", object_path))
  }

  # # Load the se object if the workflow requires it
  # if (params$workflow %in% c("se", "se_vdx", "se_dds", "se_dde")) {

  #   # Load the se object from RDS
  #   object_path <- file.path(dir_path, "se.rds")

  #   if (!file.exists(object_path)) {
  #       stop("The se file does not exist: ", object_path)
  #   }
  #   se <- readRDS(object_path)
  #   assign("source_se", se, envir = .parent.frame())
  # }

  # # Load the dds object if the workflow requires it
  # if (params$workflow %in% c("dds")) {

  #   # Load the dds object from RDS
  #   object_path <- file.path(dir_path, "dds.rds")

  #   if (!file.exists(object_path)) {
  #       stop("The dds file does not exist: ", object_path)
  #   }
  #   dds <- readRDS(object_path)
  #   assign("source_dds", dds, envir = .parent.frame())
  # }

  # # Load the dds object if the workflow requires it
  # if (params$workflow %in% c("dde")) {

  #   # Load the dds object from RDS
  #   object_path <- file.path(dir_path, "dde.rds")

  #   if (!file.exists(object_path)) {
  #       stop("The dde file does not exist: ", object_path)
  #   }
  #   dds <- readRDS(object_path)
  #   assign("source_dde", dde, envir = .parent.frame())
  # }

  # Load the anns object from RDS
  object_path <- file.path(dir_path, "anns.rds")

  if (!file.exists(object_path)) {
    warning("The anns file does not exist: ", object_path)
  } else {
    anns <- readRDS(object_path)
    assign("anns", anns, envir = parent.frame())
    message(sprintf("Loaded 'anns' object from %s", object_path))
  }

  # Load the metadata file (CSV or Excel) based on params$metadata
  metadata_path <- params$metadata_file

  if (!file.exists(metadata_path)) {
    warning("The metadata file does not exist: ", metadata_path)
  }

  # Determine file type and load accordingly
  if (grepl("\\.csv$", metadata_path, ignore.case = TRUE)) {
    metadata <- read.csv(metadata_path, stringsAsFactors = FALSE)
  } else if (grepl("\\.(xls|xlsx)$", metadata_path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("The 'readxl' package is required to read Excel files. Please install it.")
    }
    metadata <- readxl::read_excel(metadata_path)
    message(sprintf("Loaded 'metadata' object from %s", metadata_path))
  } else {
    stop("Unsupported file format for metadata. Please provide a CSV or Excel file.")
  }

  assign("metadata", metadata, envir = parent.frame())

  message("Workspace preparation complete.")
}
