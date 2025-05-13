library(testthat)

# Source necessary files
source("../../R/Excreted_volatile_solids.R")

# Test the volatile solids calculations with the renamed parameters
test_that("Volatile solids calculations work with renamed parameters", {
  # Test manure_volatile_solids function with renamed parameters
  VS_manure <- manure_volatile_solids(
    m_manure_excreted = 20000,
    dm_manure = 0.15,
    fraction_dm_VS = 0.8,  # Renamed from f_dm_VS
    fraction_grass = 0.3
  )
  
  # Test that the calculation produces expected results
  expect_true(VS_manure > 0)
  expect_equal(VS_manure, 20000/365 * 0.15 * 0.8 * (1-0.3))
  
  # Test straw_volatile_solids function with renamed parameters
  VS_straw <- straw_volatile_solids(
    m_straw = 500,
    dm_straw = 0.85,
    fraction_ash = 0.045,  # Renamed from f_ash
    fraction_actual_grass = 0.25  # New parameter
  )
  
  # Test that the calculation produces expected results
  expect_true(VS_straw > 0)
  expect_equal(VS_straw, 500 * 0.85 * (1-0.045) * (1-0.25))
  
  # Test grass_volatile_solids function with renamed parameters
  VS_grass <- grass_volatile_solids(
    m_manure_excreted = 20000,
    dm_manure = 0.15,
    fraction_dm_VS = 0.8,  # Renamed from f_dm_VS
    fraction_grass = 0.3
  )
  
  # Test that the calculation produces expected results
  expect_true(VS_grass > 0)
  expect_equal(VS_grass, 20000/365 * 0.15 * 0.8 * 0.3)
  
  # Test housing_volatile_solids function
  VS_housing <- housing_volatile_solids(
    VS_manure = VS_manure,
    VS_straw = VS_straw
  )
  
  # Test that the calculation produces expected results
  expect_true(VS_housing > 0)
  expect_equal(VS_housing, VS_manure + VS_straw)
  
  # Print results for reference
  cat("\nVolatile solids calculation results:\n")
  cat("VS_manure:", VS_manure, "kg/(head.yr)\n")
  cat("VS_straw:", VS_straw, "kg/(head.yr)\n")
  cat("VS_grass:", VS_grass, "kg/(head.yr)\n")
  cat("VS_housing:", VS_housing, "kg/(head.yr)\n")
})

# Test the impact of fraction_actual_grass on volatile solids calculations
test_that("fraction_actual_grass affects straw volatile solids correctly", {
  # Calculate straw volatile solids with different fraction_actual_grass values
  VS_straw1 <- straw_volatile_solids(
    m_straw = 500,
    dm_straw = 0.85,
    fraction_ash = 0.045,
    fraction_actual_grass = 0.2  # Lower value
  )
  
  VS_straw2 <- straw_volatile_solids(
    m_straw = 500,
    dm_straw = 0.85,
    fraction_ash = 0.045,
    fraction_actual_grass = 0.4  # Higher value
  )
  
  # Test that higher fraction_actual_grass results in lower VS_straw
  # Since fraction_actual_grass is used in (1-fraction_actual_grass) in the calculation
  expect_true(VS_straw2 < VS_straw1)
  
  # Calculate the expected values
  expected_VS_straw1 <- 500 * 0.85 * (1-0.045) * (1-0.2)
  expected_VS_straw2 <- 500 * 0.85 * (1-0.045) * (1-0.4)
  
  # Test that the calculations match the expected values
  expect_equal(VS_straw1, expected_VS_straw1)
  expect_equal(VS_straw2, expected_VS_straw2)
  
  # Print results for reference
  cat("\nImpact of fraction_actual_grass on straw volatile solids:\n")
  cat("VS_straw with fraction_actual_grass = 0.2:", VS_straw1, "kg/(head.yr)\n")
  cat("VS_straw with fraction_actual_grass = 0.4:", VS_straw2, "kg/(head.yr)\n")
})
