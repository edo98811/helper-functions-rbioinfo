# Function to check multiple conditions and return a message based on them
load_results <- function(params) {
  # Validate the input parameters
  if (!check_params(params)) stop("load_results: wrong parameters provided.")

  # If run_computations is TRUE, skip loading operations
  if (params$run_computations == TRUE) {
    message("run_computations is TRUE. Skipping loading of results")
    return()
  }

  # Determine the directory path based on the provided parameters
  dir_path <- file.path("analyses_results", params$analysis_name)

  # Load se object if the workflow requires it
  if (params$workflow %in% c("se", "se_vdx", "se_dds", "se_dde", "dde")) {
    object_path <- file.path(dir_path, "se.rds")
    if (!file.exists(object_path)) {
      warning("The se file does not exist: ", object_path)
    } else {
      se <- readRDS(object_path)
      assign("se", se, envir = parent.frame())
      message(sprintf("Loaded 'se' object from %s", object_path))
    }
  }

  # Load the dds object if the workflow requires it
  if (params$workflow %in% c("dds", "se_dds")) {
    object_path <- file.path(dir_path, "dds.rds")
    if (!file.exists(object_path)) {
      warning("The dds file does not exist: ", object_path)
    } else {
      dds <- readRDS(object_path)
      assign("dds", dds, envir = parent.frame())
      message(sprintf("Loaded 'dds' object from %s", object_path))
    }
  }

  # Load dde object if the workflow requires it
  if (params$workflow %in% c("dde", "se_dde")) {
    object_path <- file.path(dir_path, "dde.rds")
    if (!file.exists(object_path)) {
      warning("The dde file does not exist: ", object_path)
    } else {
      dde <- readRDS(object_path)
      assign("dde", dde, envir = parent.frame())
      message(sprintf("Loaded 'dde' object from %s", object_path))
    }
  }

  # Load vdx object if the workflow requires it
  if (params$workflow == "vdx") {
    object_path <- file.path(dir_path, "vdx.rds")
    if (!file.exists(object_path)) {
      warning("The vdx file does not exist: ", object_path)
    } else {
      vdx <- readRDS(object_path)
      assign("vdx", vdx, envir = parent.frame())
      message(sprintf("Loaded 'vdx' object from %s", object_path))
    }
  }

  # Load results if not running computations or if workflow is not dde or se_dde
  if (params$run_computations == FALSE && !params$workflow %in% c("dde", "se_dde")) {
    object_path <- file.path(dir_path, "results.rds")
    if (!file.exists(object_path)) {
      warning("The results file does not exist: ", object_path)
    } else {
      results <- readRDS(object_path)
      assign("results", results, envir = parent.frame())
      message(sprintf("Loaded 'results' object from %s", object_path))
    }
  }

