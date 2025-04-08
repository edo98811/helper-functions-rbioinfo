#' Merge Multiple R Markdown Files into One
#'
#' This function takes a list of predefined `.Rmd` files, merges their contents into a single `.Rmd` file, 
#' and saves the output to the specified file name.
#'
#' @param output_filename A character string specifying the name of the merged output `.Rmd` file.
#' If no `.Rmd` extension is provided, it will be added automatically.
#' @param type A character string specifying the type of report to create
#'
#' @return A message indicating the merged file has been created.
#' @export
#'
create_report <- function(output_filename, type = "dds_gse") {
  # Define the list of Rmd files
#   rmd_files <- c(
#     "section_setup.Rmd",
#     "section_n_reads.Rmd",
#     "section_pca.Rmd",
#     "section_EDA.Rmd",
#     "section_design.Rmd",
#     "section_DE.Rmd",
#     "section_enrichment.Rmd",
#     "section_geneplots.Rmd",
#     "section_genetonic.Rmd",
#     "section_save.Rmd"
#   )

    if (type == "dds_gse") 
        rmd_files <- c(
            "section-01-setup_dds_gse.Rmd",
            "section-02-design.Rmd",
            "section-03-DDs_from_DE_annotation.Rmd",
            "section-04-EDA.Rmd",
            "section-05-n_reads.Rmd",
            "section-06-pca.Rmd",
            "section-07-DE.Rmd",
            "section-08-enrichment.Rmd",
            "section-09-save.Rmd"
        )
    else if (type == "dds") 
        rmd_files <- c(
            "section-01-setup_dds.Rmd",
            "section-02-design.Rmd",
            "section-03-DDs_from_DE.Rmd",
            "section-04-EDA.Rmd",
            "section-05-n_reads.Rmd",
            "section-06-pca.Rmd",
            "section-07-DE.Rmd",
            "section-08-enrichment.Rmd",
            "section-09-save.Rmd"
        )
    else stop("Invalid report type")

    rmd_dir <- system.file("extdata", package = "rbioinfoHelper")

    # List all .Rmd files in the directory
    rmd_files_paths <- list.files(rmd_dir, pattern = "\\.Rmd$", full.names = TRUE)

    # Ensure the output file has the .Rmd extension
    if (!grepl("\\.Rmd$", output_filename)) {
        output_filename <- paste0(output_filename, ".Rmd")
    }
    # Delete the output file if it exists
    if (file.exists(output_filename)) {
        file.remove(output_filename)
    }
    # Open the output file for writing
    sink(output_filename)

    lapply(rmd_files, function(file_name) {
        file <- rmd_files_paths[basename(rmd_files_paths) == file_name]

        cat("\n\n", "<!-- ---- Start of ", file_name, "---- -->\n\n", sep = "")
        cat(readLines(file), sep = "\n")
        cat("\n\n", "<!-- ---- End of ", file_name, "---- -->\n\n", sep = "")
    })

    # Close the output file
    sink()

    # Print success message
    message("Merged .Rmd file created: ", output_filename)
}
