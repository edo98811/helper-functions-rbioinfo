#' Merge Multiple R Markdown Files into One
#'
#' This function takes a list of predefined `.Rmd` files, merges their contents into a single `.Rmd` file, 
#' and saves the output to the specified file name.
#'
#' @param output_filename A character string specifying the name of the merged output `.Rmd` file.
#' If no `.Rmd` extension is provided, it will be added automatically.
#'
#' @return A message indicating the merged file has been created.
#' @export
#'
create_report <- function(output_filename) {
  # Define the list of Rmd files
  rmd_files <- c(
    "section_setup.Rmd",
    "section_n_reads.Rmd",
    "section_pca.Rmd",
    "section_EDA.Rmd",
    "section_design.Rmd",
    "section_DE.Rmd",
    "section_enrichment.Rmd",
    "section_geneplots.Rmd",
    "section_genetonic.Rmd",
    "section_save.Rmd"
  )
  
  # Ensure the output file has the .Rmd extension
  if (!grepl("\\.Rmd$", output_filename)) {
    output_filename <- paste0(output_filename, ".Rmd")
  }
  
  # Open the output file for writing
  sink(output_filename)
  
  # Loop through each file, read its content, and append it to the output file
  for (file in rmd_files) {
    if (file.exists(file)) {
      cat("\n\n", "# ---- Start of", file, "---- #\n\n", sep = "")
      cat(readLines(file), sep = "\n")
      cat("\n\n", "# ---- End of", file, "---- #\n\n", sep = "")
    } else {
      cat("\n\n", "# ---- WARNING: Missing file:", file, "---- #\n\n", sep = "")
    }
  }
  
  # Close the output file
  sink()
  
  # Print success message
  message("Merged .Rmd file created: ", output_filename)
}
