# Introduction

This is a package used to create reports, 

# Installation 

[git@github.com:edo98811/helper-functions-rbioinfo.git](https://github.com/edo98811/helper-functions-rbioinfo.git)

# Available functions 

## Available functions


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
    - `experiment_metadata_file`: Path to the experiment metadata file (xlsx format).
    - `load_gse`: Logical, whether to load the GSE object (default is FALSE).
    - `load_complete_dds`: Logical, whether to load the complete DDS object (default is FALSE).
    - `complete_dds_source`: Path to the complete DDS source file.

`load_rds_arrow()`: Loads DDS objects using the Arrow package for efficient data handling.

`prepare_workspace()`: Sets up the R environment for analysis, including loading necessary libraries and data.

* `params`: A list containing the following elements:
    - `experiment_metadata_file`: Path to the experiment metadata file (xlsx format).
    - `load_gse`: Logical, whether to load the GSE object (default is FALSE).
    - `load_complete_dds`: Logical, whether to load the complete DDS object (default is FALSE).
    - `complete_dds_source`: Path to the complete DDS source file.

`save_dds_arrow()`: Loads DDS objects using the Arrow package for efficient data handling.
