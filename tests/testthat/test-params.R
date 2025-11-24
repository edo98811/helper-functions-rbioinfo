library(testthat)
library(rbioinfoHelper)

# Test the check_params function
test_that("check_params works with valid parameters", {
  # Valid parameter list with all required fields
  valid_params <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "Hs",
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_true(check_params(valid_params))
})

test_that("check_params detects missing required parameters", {
  # Missing one required parameter
  incomplete_params <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "Hs",
    create_annotation_df = TRUE,
    workflow = "se"
    # missing save_results
  )
  
  expect_warning(
    result <- check_params(incomplete_params),
    "The following required parameters are missing: save_results"
  )
  expect_false(result)
})

test_that("check_params detects multiple missing parameters", {
  # Missing multiple required parameters
  incomplete_params <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    subset_object = TRUE,
    species = "Hs"
    # missing: source_object, metadata_file, create_annotation_df, workflow, save_results
  )
  
  expect_warning(
    result <- check_params(incomplete_params),
    "The following required parameters are missing:"
  )
  expect_false(result)
})

test_that("check_params validates workflow parameter", {
  # Invalid workflow
  invalid_workflow_params <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "Hs",
    create_annotation_df = TRUE,
    workflow = "invalid_workflow",
    save_results = FALSE
  )
  
  expect_error(
    check_params(invalid_workflow_params),
    "Invalid workflow type. Accepted values are: se, se_vdx, se_dds, dds, vdx, se_dde, dde"
  )
})

test_that("check_params validates all valid workflow options", {
  valid_workflows <- c("se", "se_vdx", "se_dds", "dds", "vdx", "se_dde", "dde")
  
  for (workflow in valid_workflows) {
    params <- list(
      run_computations = TRUE,
      analysis_name = "Test Analysis",
      source_object = "some_object",
      metadata_file = "metadata.txt",
      subset_object = TRUE,
      species = "Hs",
      create_annotation_df = TRUE,
      workflow = workflow,
      save_results = FALSE
    )
    
    expect_true(check_params(params), info = paste("Failed for workflow:", workflow))
  }
})

test_that("check_params validates subset_object as logical", {
  # Non-logical subset_object
  invalid_subset_params <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = "yes", # should be logical
    species = "Hs",
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_warning(
    result <- check_params(invalid_subset_params),
    "The 'subset_object' parameter must be TRUE or FALSE."
  )
  expect_false(result)
})

test_that("check_params accepts both TRUE and FALSE for subset_object", {
  # Test TRUE
  params_true <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "Hs",
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_true(check_params(params_true))
  
  # Test FALSE
  params_false <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = FALSE,
    species = "Hs",
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_true(check_params(params_false))
})

test_that("check_params validates species parameter", {
  # Invalid species
  invalid_species_params <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "Rn", # should be "Hs" or "Mm"
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_warning(
    result <- check_params(invalid_species_params),
    "The 'species' parameter must be either 'Hs' or 'Mm'."
  )
  expect_false(result)
})

test_that("check_params accepts valid species options", {
  # Test "Hs"
  params_hs <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "Hs",
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_true(check_params(params_hs))
  
  # Test "Mm"
  params_mm <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "Mm",
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_true(check_params(params_mm))
})

test_that("check_params handles empty list", {
  empty_params <- list()
  
  expect_warning(
    result <- check_params(empty_params),
    "The following required parameters are missing:"
  )
  expect_false(result)
})

test_that("check_params handles NULL input", {
  expect_warning(
    result <- check_params(NULL),
    "The following required parameters are missing:"
  )
  expect_false(result)
})

test_that("check_params handles multiple validation failures", {
  # Invalid subset_object, species, and missing parameters
  multi_invalid_params <- list(
    run_computations = TRUE,
    subset_object = "invalid", # should be logical
    species = "Invalid", # should be "Hs" or "Mm"
    workflow = "se"
    # missing several required parameters
  )
  
  # Should warn about missing parameters first
  expect_warning(
    result <- check_params(multi_invalid_params),
    "The following required parameters are missing:"
  )
  expect_false(result)
})

test_that("check_params handles case sensitivity for species", {
  # Test lowercase species (should fail)
  params_lowercase <- list(
    run_computations = TRUE,
    analysis_name = "Test Analysis",
    source_object = "some_object",
    metadata_file = "metadata.txt",
    subset_object = TRUE,
    species = "hs", # lowercase should fail
    create_annotation_df = TRUE,
    workflow = "se",
    save_results = FALSE
  )
  
  expect_warning(
    result <- check_params(params_lowercase),
    "The 'species' parameter must be either 'Hs' or 'Mm'."
  )
  expect_false(result)
})