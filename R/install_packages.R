# Function to install and load R packages
install_and_load <- function() {
  # find DESCRIPTION up the filesystem tree (useful when run from package project)
  find_description <- function(path = getwd()) {
    path <- tryCatch(normalizePath(path, winslash = "/", mustWork = TRUE), error = function(e) NULL)
    if (is.null(path)) return(NULL)
    repeat {
      desc <- file.path(path, "DESCRIPTION")
      if (file.exists(desc)) return(desc)
      parent <- dirname(path)
      if (parent == path) return(NULL)
      path <- parent
    }
  }

  desc_file <- find_description()
  pkgs <- character(0)

  if (!is.null(desc_file)) {
    d <- read.dcf(desc_file, fields = c("Depends", "Imports", "Suggests"))
    vals <- unlist(d[1, , drop = FALSE], use.names = FALSE)
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals)) {
      pkgs <- unlist(strsplit(paste(vals, collapse = ","), ","))
      pkgs <- gsub("\\(.*?\\)", "", pkgs)   # drop version constraints
      pkgs <- trimws(pkgs)
      pkgs <- pkgs[pkgs != "" & pkgs != "R"]
      pkgs <- unique(pkgs)
    }
  }

  if (length(pkgs) == 0) {
    warning("No packages found in DESCRIPTION (Depends/Imports/Suggests). Set 'pkgs' manually if needed.")
    return(invisible(NULL))
  }

  installed_pkgs <- rownames(utils::installed.packages())
  to_install <- setdiff(pkgs, installed_pkgs)

  if (length(to_install) > 0) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    BiocManager::install(to_install, ask = FALSE)
  }

  # load packages (suppress startup messages)
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(require(p, character.only = TRUE, quietly = TRUE))
  }))
}
