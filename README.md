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

# Parameters for limma workflow

The `params` list contains the following elements:

* `subset_complete_dds`: Logical, whether to filter the complete DDS object including only a subset (default is FALSE).
* `load_complete_dds`: Logical, whether to load the complete DDS object (default is FALSE).
* `load_dds_objects`: Logical, whether to load DDS objects (default is TRUE).
* `save_dds_objects`: Logical, whether to save DDS objects (default is FALSE).
* `complete_dds_source`: Path to the complete DDS source file.
* `analysis_name`: Name of the experiment.
* `sample_metadata_file`: Path to the experiment metadata file (xlsx format).
* `create_annotation_df`: Logical, whether to create annotation data frames (default is FALSE).
* `workflow`: A character string specifying the workflow to use (default is "dds"). if working only with SummarizedExperiment write summarized_experiment

# Parameters for gse dds / dds workflow

The `params` list contains the following elements:

* `workflow`:
* `count_matrix`:
* `analysis_name`:
* `sample_metadata_file`:
* `create_annotation_df`:
* `save_se`:
* `load_results_objects`:

# Required data
* `sample_metadata_file`: Path to the experiment metadata file (xlsx format).
* `gse / se`: A gse or se object. The rowdata need to have as rownames ensemble id or ensemble id.version. The version will be removed from rownames if given and it will be stored in an additional column called "ensemble_version". The column "gene_name" will be created when creating the annotation df. If not present the code will run in any case. When using proteomics data the uniprotID can also be given as rownames. The functionsd that need to be used in this case have the "proteomics" defix. The functions for whcih two versions are available are: 
  - `create_annotation_(proteomics)
* `rownames`: The rownames contained in the gse object, they need to be an ensembl_id or an ensembl_id.version. 

There are also functions that function with a specific workflow, for example: 
* `prepare_worksapce_(limma)`
* `save_(limma)`
* `load_(limma)`
* `alltheresults_(limma)`

The rest of the functions are the same for all the workflows.

# Workflow process

The way it works is: you have to prepare a summarized experiment object, the metadata can be either given as external file or in the coldata of the summarized experiment. If they are given externally the rownames (so the first column of the excel or csv file) has to be the same that is found in the colnames of the summarized experiment object. 
The first thing to do is to set the params following the definition given above, normally 
The it is enough to call the function prepare workspace (or prepare_workspace_limma). At this point the objects are loaded, the gse, the metadata and the annotation if it was already present, if it is not present it is not a problem it will be loaded at a later step. 
Then the analysis can be loaded. This means that the results, which were saved either in the limma format or the dds format are loaded if needed, this is done to skip unnecessary calculations in the next step. 
Then the loaded gse can be subsetted, if the analysis that in this moment i want to run requires only a subset of the subjects.
At this point we can start with the real analysis, the first thing to do is to define the design of the experiemnt, to do this we start from the columns that are present in the metadata that we will use to define the design, we can wreite them in the factor coulns object, then the design is defined and printed, after this the contrasts that are of intertest. This step is only necessary with dds and not with limma.
