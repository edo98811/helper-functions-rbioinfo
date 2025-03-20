#' Replace Missing Values in SummarizedExperiment Assay Data
#'
#' This function replaces missing values (NA) in the assay data of a 
#' SummarizedExperiment object with zeros or vice versa, based on the specified 
#' `force_type`.
#'
#' @param se A `SummarizedExperiment` object. The input object whose assay data 
#'   will be modified.
#' @param force_type A character string specifying the type of replacement to 
#'   perform. Default is `"zero"`. If `"zero"`, NA values in the assay data 
#'   will be replaced with zeros. If `"NA"`, zero values in the assay data will 
#'   be replaced with NAs.
#'
#' @return A `SummarizedExperiment` object with modified assay data.
#'
#' @examples
#' # Assuming `se` is a SummarizedExperiment object with some NA values in the assay data
#' se <- missing_switch(se, force_type = "zero")
#'
#' # Assuming `se` is a SummarizedExperiment object with some zero values in the assay data
#' se <- missing_switch(se, force_type = "NA")
#'
#' @import SummarizedExperiment
#' @export
missing_switch <- function(se, force_type = "zero") {

    # if (!inherits(se, "SummarizedExperiment::SummarizedExperiment")) {
    #     stop("Input must be a SummarizedExperiment object")
    # }
    
    tryCatch({
        if (!inherits(se, "SummarizedExperiment")) {
            stop("Input must be a SummarizedExperiment object")
        }
        assay_data <- assay(se)
    }, error = function(e) {
        message("An error occurred: ", e$message)
        stop(e)
    })

    if (any(is.na(assay_data)) || force_type == "zero") assay_data[is.na(assay_data)] <- 0
    else if (!any(is.na(assay_data)) || force_type == "NA") assay_data[assay_data == 0] <- NA
    
    assay(se) <- assay_data
    
    return(se)
}