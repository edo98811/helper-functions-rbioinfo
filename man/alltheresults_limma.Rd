% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alltheresults_proteomics.R
\name{alltheresults_limma}
\alias{alltheresults_limma}
\title{Extract and Annotate Differential Expression Results from Limma Analysis}
\usage{
alltheresults_limma(
  resuSet,
  fitted_limma_model,
  contrast,
  FDR,
  anno_df,
  species = "mus_musculus"
)
}
\arguments{
\item{resuSet}{A list to store the results.}

\item{fitted_limma_model}{The fitted Limma model object.}

\item{contrast}{A character vector specifying the contrast of interest.}

\item{FDR}{A numeric value specifying the false discovery rate threshold.}

\item{anno_df}{A data frame containing annotation information with columns for gene symbols, Ensembl gene IDs, and UniProt IDs.}

\item{species}{A character string specifying the species (default is "mus_musculus").}
}
\value{
A list containing the extracted and annotated results, including interactive tables.
}
\description{
This function extracts and annotates differential expression results from a fitted Limma model,
and generates interactive tables with gene information and links.
}
\details{
The function performs the following steps:
\itemize{
\item Extracts the fitted model and results table.
\item Annotates the results table with gene information.
\item Filters the results table based on the specified FDR threshold.
\item Generates interactive tables with links to external databases for gene symbols, Ensembl gene IDs, and UniProt IDs.
\item Reorders columns and rounds numerical values for better readability.
}
}
\examples{
\dontrun{
resuSet <- list()
fitted_limma_model <- lmFit(...)
contrast <- c("condition1", "condition2")
FDR <- 0.05
anno_df <- data.frame(...)
results <- alltheresults_limma(resuSet, fitted_limma_model, contrast, FDR, anno_df)
}

}
