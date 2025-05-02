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

# Required data
* `sample_metadata_file`: Path to the experiment metadata file (xlsx format).
* `gse / se`: A gse or se object. The rowdata need to have as rownames ensemble id or ensemble id + version. The version will be removed from rownames if given and it will be stored in an additional column called "ensemble_version". The column "gene_name" will be created when creating the annotation df. If not present the code will run in any case. When using proteomics data the uniprotID can also be given as rownames. The functionsd that need to be used in this case have the "proteomics" defix. The functions for whcih two versions are available are: 
  - `create_annotation_(proteomics)
* `rownames`: The rownames contained in the gse object, 

There are also functions that function with a specific workflow, for example: 
* `prepare_worksapce_(limma)`
* `save_(limma)`
* `load_(limma)`
* `alltheresults_(limma)`


