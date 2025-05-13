library(testthat)

# Source necessary files instead of loading the rCH4 package
source("../../R/User_input.R")
source("../../R/Inventory.r")
source("../../R/Enteric_fermentation.r")
source("../../R/Excreted_volatile_solids.R")

# Test simulation of CH4 emissions for typical Danish livestock
# This test file simulates emissions for dairy cows, sows, and broilers
# using typical values from Danish agriculture

test_that("CH4 emissions for Danish dairy cows are calculated correctly", {
  # Typical values for Danish dairy cows
  dairy_input = User_input$new(
    animal_type = "dairy_cows",
    animal_number = 100,
    weight = 650,  # kg
    milk_yield = 10000,  # kg/year
    fat_content = 4.2,  # %
    protein_content = 3.4,  # %
    manure_excretion = 25000,  # kg/year
    manure_cn = 12,
    fraction_housing = 0.75,  # 75% time in housing
    fraction_grazing = 0.25,  # 25% time grazing
    feed_intake = 22,  # kg DM/day
    fraction_diet_grass = 0.4,  # 40% grass in diet
    fraction_diet_beet = 0.05,  # 5% beet in diet
    straw_amount = 550  # kg/year
  )
  
  # Create an inventory instance
  dairy_inventory = Inventory$new(
    user_input = dairy_input,
    debug_mode = TRUE
  )
  
  # Run the inventory calculation
  dairy_results = dairy_inventory$run_inventory()
  
  # Test that results are within expected ranges for Danish dairy cows
  # Enteric fermentation: typically 100-150 kg CH4/head/year
  expect_true(dairy_results$emissions$ch4_enteric > 100)
  expect_true(dairy_results$emissions$ch4_enteric < 150)
  
  # Manure management: typically 15-30 kg CH4/head/year
  expect_true(dairy_results$emissions$ch4_manure$total > 15)
  expect_true(dairy_results$emissions$ch4_manure$total < 30)
  
  # Housing emissions should be higher than grazing emissions due to time distribution
  expect_gt(dairy_results$emissions$ch4_manure$housing, dairy_results$emissions$ch4_manure$grass)
  
  # Print results for reference
  cat("\nDairy cow CH4 emissions (kg CH4/head/year):\n")
  cat("Enteric fermentation:", dairy_results$emissions$ch4_enteric, "\n")
  cat("Manure (housing):", dairy_results$emissions$ch4_manure$housing, "\n")
  cat("Manure (grazing):", dairy_results$emissions$ch4_manure$grass, "\n")
  cat("Manure (total):", dairy_results$emissions$ch4_manure$total, "\n")
  cat("Total emissions:", dairy_results$emissions$ch4_total, "\n")
})

test_that("CH4 emissions for Danish sows are calculated correctly", {
  # Typical values for Danish sows
  sow_input = User_input$new(
    animal_type = "sows",
    animal_number = 100,
    weight = 200,  # kg
    reproduction = 28,  # piglets/year
    manure_excretion = 5500,  # kg/year
    manure_cn = 10,
    fraction_housing = 1.0,  # 100% time in housing
    fraction_grazing = 0.0,  # 0% time grazing
    feed_intake = 5.5,  # kg DM/day
    fraction_diet_grass = 0.0,  # 0% grass in diet
    fraction_diet_beet = 0.0,  # 0% beet in diet
    straw_amount = 200  # kg/year
  )
  
  # Create an inventory instance
  sow_inventory = Inventory$new(
    user_input = sow_input,
    debug_mode = TRUE
  )
  
  # Run the inventory calculation
  sow_results = sow_inventory$run_inventory()
  
  # Test that results are within expected ranges for Danish sows
  # Enteric fermentation: typically 1-2 kg CH4/head/year
  expect_true(sow_results$emissions$ch4_enteric > 1)
  expect_true(sow_results$emissions$ch4_enteric < 5)
  
  # Manure management: typically 5-15 kg CH4/head/year
  expect_true(sow_results$emissions$ch4_manure$total > 5)
  expect_true(sow_results$emissions$ch4_manure$total < 15)
  
  # No grazing emissions for sows
  expect_equal(sow_results$emissions$ch4_manure$grass, 0)
  
  # Print results for reference
  cat("\nSow CH4 emissions (kg CH4/head/year):\n")
  cat("Enteric fermentation:", sow_results$emissions$ch4_enteric, "\n")
  cat("Manure (housing):", sow_results$emissions$ch4_manure$housing, "\n")
  cat("Manure (grazing):", sow_results$emissions$ch4_manure$grass, "\n")
  cat("Manure (total):", sow_results$emissions$ch4_manure$total, "\n")
  cat("Total emissions:", sow_results$emissions$ch4_total, "\n")
})

test_that("CH4 emissions for Danish broilers are calculated correctly", {
  # Typical values for Danish broilers
  broiler_input = User_input$new(
    animal_type = "poultry",
    animal_number = 10000,  # Broilers are typically raised in large numbers
    weight = 2.0,  # kg
    manure_excretion = 10,  # kg/year per bird
    manure_cn = 8,
    fraction_housing = 1.0,  # 100% time in housing
    fraction_grazing = 0.0,  # 0% time grazing
    feed_intake = 0.1,  # kg DM/day
    fraction_diet_grass = 0.0,  # 0% grass in diet
    fraction_diet_beet = 0.0,  # 0% beet in diet
    straw_amount = 0.2  # kg/year per bird
  )
  
  # Create an inventory instance
  broiler_inventory = Inventory$new(
    user_input = broiler_input,
    debug_mode = TRUE
  )
  
  # Run the inventory calculation
  broiler_results = broiler_inventory$run_inventory()
  
  # Test that results are within expected ranges for Danish broilers
  # Enteric fermentation: typically very low for poultry, < 0.1 kg CH4/head/year
  # But we're testing for total emissions from 10,000 birds
  expect_true(broiler_results$emissions$ch4_enteric > 0)
  expect_true(broiler_results$emissions$ch4_enteric < 1000)  # Total for 10,000 birds
  
  # Manure management: also low for poultry, < 0.1 kg CH4/head/year
  # But again testing for 10,000 birds
  expect_true(broiler_results$emissions$ch4_manure$total > 0)
  expect_true(broiler_results$emissions$ch4_manure$total < 1000)  # Total for 10,000 birds
  
  # No grazing emissions for broilers
  expect_equal(broiler_results$emissions$ch4_manure$grass, 0)
  
  # Calculate per-bird emissions for reporting
  per_bird_enteric = broiler_results$emissions$ch4_enteric / 10000
  per_bird_manure = broiler_results$emissions$ch4_manure$total / 10000
  per_bird_total = broiler_results$emissions$ch4_total / 10000
  
  # Print results for reference
  cat("\nBroiler CH4 emissions:\n")
  cat("Per bird (kg CH4/head/year):\n")
  cat("Enteric fermentation:", per_bird_enteric, "\n")
  cat("Manure (total):", per_bird_manure, "\n")
  cat("Total emissions:", per_bird_total, "\n")
  cat("\nTotal for 10,000 birds (kg CH4/year):\n")
  cat("Enteric fermentation:", broiler_results$emissions$ch4_enteric, "\n")
  cat("Manure (housing):", broiler_results$emissions$ch4_manure$housing, "\n")
  cat("Manure (total):", broiler_results$emissions$ch4_manure$total, "\n")
  cat("Total emissions:", broiler_results$emissions$ch4_total, "\n")
})
