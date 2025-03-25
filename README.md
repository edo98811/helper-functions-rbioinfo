# Introduction

This is a package used to create reports, 

# Installation 

[https://github.com/edo98811/helper-functions-rbioinfo.git](https://github.com/edo98811/helper-functions-rbioinfo.git)

To install this package, you can use the `remotes` package in R:

```r
# Install the remotes package if you haven't already
install.packages("remotes")

# Use remotes to install the package from GitHub
remotes::install_github("edo98811/helper-functions-rbioinfo")
```

# Parameters

The `params` list contains the following elements:

* `filter_complete_dds`: Logical, whether to filter the complete DDS object including only a subset (default is FALSE).
* `load_complete_dds`: Logical, whether to load the complete DDS object (default is FALSE).
* `load_dds_objects`: Logical, whether to load DDS objects (default is TRUE).
* `save_dds_objects`: Logical, whether to save DDS objects (default is FALSE).
* `complete_dds_source`: Path to the complete DDS source file.
* `analysis_name`: Name of the experiment.
* `sample_metadata_file`: Path to the experiment metadata file (xlsx format).
* `create_annotation_df`: Logical, whether to create annotation data frames (default is FALSE).
* `workflow`: A character string specifying the workflow to use (default is "dds"). if working only with SummarizedExperiment write summarized_experiment

# Available functions 


`add_symbol_to_dds()`: Adds gene symbols to a DESeq2 dataset.

* `dds`: A DESeq2 dataset object.
* `annotation_dataset`: A data frame containing gene annotations with at least two columns: `gene_id` and `gene_name`. The `gene_id` column should match the row names of the DESeq2 dataset.

`alltheresults()`: Creates the myResuset list, useful to structure the results.

* `resuSet`: A list to store the results.
* `dds_obj`: A DESeqDataSet object.
* `contrast`: A character vector specifying the contrast.
* `FDR`: A numeric value specifying the false discovery rate threshold.
* `anno_df`: A data frame containing gene annotations with columns `gene_id` and `gene_name`.
* `anns`: A data frame containing additional annotations with columns `ensembl_gene_id`, `description`, and `chromosome_name`.
* `species`: A character string specifying the species for creating links.

`create_annotations()`: Creates two dataframes that contain the info on all the genes present in the dds object.

* `dds`: A DESeqDataSet object. The function will stop if the provided object is not of class 'DESeqDataSet'.

`load_analyses()`: Loads previously saved analysis results.

* `params`: A list containing the following elements:
    - `sample_metadata_file`: Path to the experiment metadata file (xlsx format).
    - `load_gse`: Logical, whether to load the GSE object (default is FALSE).
    - `load_complete_dds`: Logical, whether to load the complete DDS object (default is FALSE).
    - `complete_dds_source`: Path to the complete DDS source file.

`load_rds_arrow()`: Loads DDS objects using the Arrow package for efficient data handling.

`prepare_workspace()`: Sets up the R environment for analysis, including loading necessary libraries and data.

* `params`: A list containing the following elements:
    - `sample_metadata_file`: Path to the experiment metadata file (xlsx format).
    - `load_gse`: Logical, whether to load the GSE object (default is FALSE).
    - `load_complete_dds`: Logical, whether to load the complete DDS object (default is FALSE).
    - `complete_dds_source`: Path to the complete DDS source file.

* `params`:(to work not with dds) A list containing the following elements:
    - `sample_metadata_file`: Path to the experiment metadata file (xlsx format).
    - `workflow`: A character string specifying the workflow to use (needs to be "summarized_experiment").

`save_se_arrow()`: Loads DDS objects using the Arrow package for efficient data handling. (to implement)

`load_se_analyses()`: Loads previously saved SE analysis results. (to implement)

`create_annotations_proteomics()`: Creates annotation data frame for proteomics data (anno_df).

* `se`: a SummarizedExperiment (SE) object.

`save_se()`: Saves a SummarizedExperiment (SE) object to a RDS file.

* `se`: A SummarizedExperiment object to be saved.
* `params`: A character string specifying the path where the SE object should be saved.
