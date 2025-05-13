library(testthat)
library(yaml)

# Source the enteric fermentation function directly
source("../../R/utils/enteric_fermentation_EF.R")

# Set the path to the YAML file
YAML_PATH <- normalizePath("../../inst/extdata/mms_CH4.yaml")

# Test the tier 1 enteric fermentation emission factor function
test_that("tier1 enteric fermentation emission factor calculation works correctly", {
  # Set up test paths
  yaml_file <- YAML_PATH
  skip_if(!file.exists(yaml_file), "CH4 emission factors file not found")
  
  # Temporarily modify the function to use our specific YAML file
  .get_yaml_file <- function() { return(yaml_file) }
  
  # Test pheasant (no parameters needed)
  ef_pheasant <- expect_warning(.tier1_EF("pheasant"))
  expect_equal(ef_pheasant, 0.0047)
  
  # Test laying_hens with hpr=FALSE and age=365
  ef_laying_hens_1 <- expect_warning(.tier1_EF("laying_hens", hpr = FALSE, age = 365))
  expect_equal(ef_laying_hens_1, 0.0010610)
  
  # Test broilers with age=42 and organic=FALSE
  ef_broilers_1 <- expect_warning(.tier1_EF("broilers", organic = FALSE, age = 42))
  expect_equal(ef_broilers_1, 1.5e-5)
  
  # Test laying_hens_ with hpr=TRUE and age=140
  ef_laying_hens_2 <- expect_warning(.tier1_EF("laying_hens_", hpr = TRUE, age = 140))
  expect_equal(ef_laying_hens_2, 3.561e-3)
  
  # Test broilers_ with age=91 and organic=TRUE
  ef_broilers_2 <- expect_warning(.tier1_EF("broilers_", organic = TRUE, age = 91))
  expect_equal(ef_broilers_2, 8.48e-5)
  
  # Test scaling with different age values
  # For laying_hens_ with hpr=TRUE, baseline is 140 days with EF 3.561e-3
  ef_laying_hens_scaled <- expect_warning(.tier1_EF("laying_hens_", hpr = TRUE, age = 200))
  expected_scaled_ef <- 3.561e-3 * (200 / 140)
  expect_equal(ef_laying_hens_scaled, expected_scaled_ef)
  
  # For broilers_ with organic=TRUE, baseline is 91 days with EF 8.48e-5
  ef_broilers_scaled <- expect_warning(.tier1_EF("broilers_", organic = TRUE, age = 60))
  expected_scaled_ef <- 8.48e-5 * (60 / 91)
  expect_equal(ef_broilers_scaled, expected_scaled_ef)
  
  # Test error cases
  expect_error(.tier1_EF("laying_hens", hpr = TRUE), "Parameter 'age' is required")
  expect_error(.tier1_EF("laying_hens", age = 140), "Parameter 'hpr' is required")
  expect_error(.tier1_EF("broilers", age = 42), "Parameter 'organic' is required")
  expect_error(.tier1_EF("broilers", organic = FALSE), "Parameter 'age' is required")
  expect_error(.tier1_EF("unknown_animal"), "not a tier 1 animal type")
})
