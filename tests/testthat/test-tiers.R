library(testthat)

# Source necessary files
source("../../R/User_input.R")
source("../../R/Inventory.r")
source("../../R/Enteric_fermentation.r")
source("../../R/Excreted_volatile_solids.R")

# Test the tier1 and tier2 functionality
test_that("Tier 1 and Tier 2 enteric fermentation calculations work correctly", {
  # Test tier 1 (for poultry and fur animals)
  animal_no <- 1000
  EF_CH4 <- 0.02  # Example emission factor for poultry
  
  # Calculate tier 1 emissions
  tier1_emissions <- entFerment_tier1_CH4(
    animal_no = animal_no,
    EF_CH4 = EF_CH4
  )
  
  # Test that tier 1 emissions are calculated correctly
  expect_equal(tier1_emissions, animal_no * EF_CH4)
  
  # Test tier 2 (for other animals)
  EF_summer <- 50  # Example summer emission factor
  EF_winter <- 70  # Example winter emission factor
  
  # Calculate annual emission factor
  annual_ef <- entFerment_tier2_EF(
    EF_summer = EF_summer,
    EF_winter = EF_winter
  )
  
  # Test that annual emission factor is calculated correctly
  expect_equal(annual_ef, EF_summer + EF_winter)
  
  # Calculate tier 2 emissions
  tier2_emissions <- entFerment_tier2_CH4(
    animal_no = animal_no,
    EF_annual = annual_ef
  )
  
  # Test that tier 2 emissions are calculated correctly
  expect_equal(tier2_emissions, animal_no * annual_ef)
})

# Test the inventory with a test user input
test_that("Inventory works with test user input", {
  # Create a simple user input class for testing without validation
  TestUserInput <- R6::R6Class("TestUserInput",
    public = list(
      animal_type = "dairy_cattle",
      animal_number = 100,
      weight = 650,
      manure_excretion = 20000,
      manure_cn = 15,
      fraction_housing = 0.7,
      fraction_grazing = 0.3,
      fraction_actual_grass = 0.3,  # New parameter
      feed_intake = 20,
      fraction_diet_grass = 0.6,
      fraction_diet_beet = 0.1,
      fraction_ash = 0.045,         # Renamed from f_ash
      fraction_dm_VS = 0.8,         # Renamed from f_dm_VS
      straw_amount = 500,
      max_manure_CH4 = 0.24         # New parameter for maximum methane production potential
    )
  )
  
  # Create a test user input
  test_input <- TestUserInput$new()
  
  # Create a mock Inventory class for testing
  MockInventory <- R6::R6Class("MockInventory",
    public = list(
      user_input = NULL,
      config_paths = list(
        enteric_fermentation = "../../inst/extdata/enteric_fermentation.yaml",
        global_parameters = "../../inst/extdata/global_parameters.yaml",
        manure_management = "../../inst/extdata/manure_management.yaml"
      ),
      global_params = list(
        dm_manure = 0.15,
        dm_straw = 0.85,
        max_manure_CH4 = 0.24
      ),
      EF_by_animal = list(
        feed_intake = 20,
        GE_winter = 18.5,
        GE_summer = 18.0,
        methane_conversion_factor = 6.5,
        weight = 650
      ),
      
      initialize = function(user_input) {
        self$user_input = user_input
      },
      
      calculate_seasonal_ef = function() {
        # For tier 2 animals (cattle, pigs, etc)
        winter_ef <- .entFerment_tier2_other_EF_winter(
          feed_unit = self$user_input$feed_intake,
          GE_winter = self$EF_by_animal$GE_winter,
          ch4_conv_rate = self$EF_by_animal$methane_conversion_factor,
          f_grass = self$user_input$fraction_diet_grass
        )
        
        summer_ef <- .entFerment_tier2_other_EF_summer(
          feed_unit = self$user_input$feed_intake,
          GE_summer = self$EF_by_animal$GE_summer,
          ch4_conv_rate_grass = self$EF_by_animal$methane_conversion_factor,
          f_grass = self$user_input$fraction_diet_grass
        )
        
        return(list(
          winter_ef = winter_ef,
          summer_ef = summer_ef
        ))
      },
      
      calculate_volatile_solids = function() {
        # Calculate volatile solids using the new parameter names
        VS_manure <- manure_volatile_solids(
          m_manure_excreted = self$user_input$manure_excretion,
          dm_manure = self$global_params$dm_manure,
          fraction_dm_VS = self$user_input$fraction_dm_VS,
          fraction_grass = self$user_input$fraction_grazing
        )
        
        VS_straw <- straw_volatile_solids(
          m_straw = self$user_input$straw_amount,
          dm_straw = self$global_params$dm_straw,
          fraction_ash = self$user_input$fraction_ash,
          fraction_actual_grass = self$user_input$fraction_actual_grass
        )
        
        VS_grass <- grass_volatile_solids(
          m_manure_excreted = self$user_input$manure_excretion,
          dm_manure = self$global_params$dm_manure,
          fraction_dm_VS = self$user_input$fraction_dm_VS,
          fraction_grass = self$user_input$fraction_grazing
        )
        
        VS_housing <- housing_volatile_solids(
          VS_manure = VS_manure,
          VS_straw = VS_straw
        )
        
        return(list(
          VS_manure = VS_manure,
          VS_straw = VS_straw,
          VS_grass = VS_grass,
          VS_housing = VS_housing
        ))
      },
      
      run_inventory = function() {
        # Calculate emission factors
        ef_results <- self$calculate_seasonal_ef()
        
        # Calculate volatile solids
        vs_results <- self$calculate_volatile_solids()
        
        # For tier 2 animals, first combine seasonal emission factors
        annual_ef <- entFerment_tier2_EF(
          EF_summer = ef_results$summer_ef,
          EF_winter = ef_results$winter_ef
        )
        
        # Then calculate total CH4 emissions
        ch4_enteric <- entFerment_tier2_CH4(
          animal_no = self$user_input$animal_number,
          EF_annual = annual_ef
        )
        
        # Calculate CH4 emissions from manure management
        ch4_manure_results <- manure_management_CH4(
          VS_housing = vs_results$VS_housing,
          ch4_conv_factor_livestock = self$EF_by_animal$methane_conversion_factor,
          max_manure_CH4 = self$global_params$max_manure_CH4,
          VS_grass = vs_results$VS_grass
        )
        
        # Store results
        return(list(
          emission_factors = ef_results,
          volatile_solids = vs_results,
          emissions = list(
            ch4_enteric = ch4_enteric,
            ch4_manure = list(
              housing = ch4_manure_results$Housing,
              grass = ch4_manure_results$Grass,
              total = ch4_manure_results$Total
            ),
            ch4_total = ch4_enteric + ch4_manure_results$Total
          )
        ))
      }
    )
  )
  
  # Create an inventory instance
  inventory <- MockInventory$new(test_input)
  
  # Run the inventory calculation
  results <- inventory$run_inventory()
  
  # Test that results were calculated
  expect_true(!is.null(results$emission_factors$winter_ef))
  expect_true(!is.null(results$emission_factors$summer_ef))
  expect_true(!is.null(results$volatile_solids$VS_manure))
  expect_true(!is.null(results$volatile_solids$VS_straw))
  expect_true(!is.null(results$volatile_solids$VS_grass))
  expect_true(!is.null(results$volatile_solids$VS_housing))
  expect_true(!is.null(results$emissions$ch4_enteric))
  
  # Test the expanded manure management CH4 emissions structure
  expect_true(!is.null(results$emissions$ch4_manure$housing))
  expect_true(!is.null(results$emissions$ch4_manure$grass))
  expect_true(!is.null(results$emissions$ch4_manure$total))
  expect_true(!is.null(results$emissions$ch4_total))
  
  # Test that emissions are positive
  expect_gt(results$emissions$ch4_enteric, 0)
  expect_gt(results$emissions$ch4_manure$housing, 0)
  expect_gt(results$emissions$ch4_manure$grass, 0)
  expect_gt(results$emissions$ch4_manure$total, 0)
  expect_gt(results$emissions$ch4_total, 0)
  
  # Print results for reference
  cat("\nTest results for dairy cattle:\n")
  cat("Enteric fermentation CH4:", results$emissions$ch4_enteric, "kg CH4/yr\n")
  cat("Manure management CH4 (housing):", results$emissions$ch4_manure$housing, "kg CH4/yr\n")
  cat("Manure management CH4 (grass):", results$emissions$ch4_manure$grass, "kg CH4/yr\n")
  cat("Manure management CH4 (total):", results$emissions$ch4_manure$total, "kg CH4/yr\n")
  cat("Total CH4 emissions:", results$emissions$ch4_total, "kg CH4/yr\n")
})
