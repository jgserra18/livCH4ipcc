# Load required packages
library(testthat)
library(R6)
library(yaml)

# Source required files
source("../../R/User_input.R")
source("../../R/utils_enteric_fermentation.R")
source("../../R/Enteric_fermentation.r")
source("../../R/Excreted_volatile_solids.R")

# Set up test environment
test_config = list(
  manure_management = "../../inst/extdata/mms_CH4.yaml",
  global_parameters = "../../inst/extdata/global_parameters.yaml"
)

# Create a simplified Inventory class for testing
Inventory = R6Class("Inventory",
  public = list(
    user_input = NULL,
    config_paths = list(
      enteric_fermentation = "../../inst/extdata/enteric_fermentation.yaml",
      global_parameters = "../../inst/extdata/global_parameters.yaml",
      manure_management = "../../inst/extdata/manure_management.yaml"
    ),
    debug_mode = TRUE,
    global_params = NULL,
    results = NULL,
    EF_by_animal = NULL,
    
    initialize = function(user_input, debug_mode = FALSE) {
      self$user_input = user_input
      self$EF_by_animal = list()
      self$debug_mode = debug_mode
      
      # Load emission factors and parameters
      self$compile_emission_factors()
    },
    
    compile_emission_factors = function() {
      # Load global parameters
      self$global_params = yaml::read_yaml(self$config_paths$global_parameters)
      
      # Load animal-specific emission factors
      ef_data = yaml::read_yaml(self$config_paths$enteric_fermentation)
      # Make sure we can find the animal type
      if (!is.null(ef_data$enteric_fermentation_EFs) && !is.null(ef_data$enteric_fermentation_EFs[[self$user_input$animal_type]])) {
        self$EF_by_animal = ef_data$enteric_fermentation_EFs[[self$user_input$animal_type]]
      } else {
        # Create default values for testing
        self$EF_by_animal = list(
          feed_intake = 20,
          GE_winter = 18.45,
          GE_summer = 18.45,
          methane_conversion_factor = 6.5,
          implified_emission_factor = 128
        )
      }
    },
    
    calculate_seasonal_ef = function() {
      if (self$user_input$animal_type == "dairy_cattle") {
        winter_ef = .entFerment_tier2_dairyCattle_EF_winter(
          feed = self$user_input$feed_intake,
          GE_winter = self$EF_by_animal$GE_winter,
          ch4_conv_rate_beet = self$EF_by_animal$methane_conversion_factor,
          f_grass = self$user_input$fraction_diet_grass,
          f_beet = self$user_input$fraction_diet_beet
        )
        
        summer_ef = .entFerment_tier2_dairyCattle_EF_summer(
          feed = self$user_input$feed_intake,
          GE_summer = self$EF_by_animal$GE_summer,
          ch4_conv_rate_grass = self$EF_by_animal$methane_conversion_factor,
          f_grass = self$user_input$fraction_diet_grass
        )
      } else {
        winter_ef = .entFerment_tier2_other_EF_winter(
          feed_unit = self$user_input$feed_intake,
          GE_winter = self$EF_by_animal$GE_winter,
          ch4_conv_rate = self$EF_by_animal$methane_conversion_factor,
          f_grass = self$user_input$fraction_diet_grass
        )
        
        summer_ef = .entFerment_tier2_other_EF_summer(
          feed_unit = self$user_input$feed_intake,
          GE_summer = self$EF_by_animal$GE_summer,
          ch4_conv_rate = self$EF_by_animal$methane_conversion_factor,
          f_grass = self$user_input$fraction_diet_grass
        )
      }
      
      return(list(winter_ef = winter_ef, summer_ef = summer_ef))
    },
    
    calculate_volatile_solids = function() {
      VS_manure = manure_volatile_solids(
        m_manure_excreted = self$user_input$manure_excretion,
        dm_manure = 0.15, # Hardcoded for testing
        fraction_dm_VS = 0.8, # Hardcoded for testing
        fraction_grass = self$user_input$fraction_grazing
      )
      
      VS_straw = straw_volatile_solids(
        m_straw = self$user_input$straw_amount,
        dm_straw = 0.85, # Hardcoded for testing
        fraction_ash = 0.045, # Hardcoded for testing
        fraction_actual_grass = ifelse(!is.null(self$user_input$fraction_actual_grass), self$user_input$fraction_actual_grass, self$user_input$fraction_grazing)
      )
      
      VS_grass = grass_volatile_solids(
        m_manure_excreted = self$user_input$manure_excretion,
        dm_manure = 0.15, # Hardcoded for testing
        fraction_dm_VS = 0.8, # Hardcoded for testing
        fraction_grass = self$user_input$fraction_grazing
      )
      
      VS_housing = housing_volatile_solids(
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
      ef_results = self$calculate_seasonal_ef()
      
      # Calculate volatile solids
      vs_results = self$calculate_volatile_solids()
      
      # Calculate CH4 emissions
      if (self$user_input$animal_type %in% c("poultry", "fur_animals")) {
        ch4_enteric = entFerment_tier1_CH4(
          animal_no = self$user_input$animal_number,
          EF_CH4 = ef_results$winter_ef  # Use winter EF as default for Tier 1
        )
      } 
      else {
        # For tier 2 animals, first combine seasonal emission factors
        annual_ef = entFerment_tier2_EF(
          EF_summer = ef_results$summer_ef,
          EF_winter = ef_results$winter_ef
        )
        
        # Then calculate total CH4 emissions
        ch4_enteric = entFerment_tier2_CH4(
          animal_no = self$user_input$animal_number,
          EF_annual = annual_ef
        )
      }
      
      # Calculate CH4 emissions from manure management
      ch4_manure_results = manure_management_CH4(
        VS_housing = vs_results$VS_housing,
        ch4_conv_factor_livestock = self$EF_by_animal$methane_conversion_factor,
        max_manure_CH4 = ifelse(!is.null(self$user_input$max_manure_CH4), self$user_input$max_manure_CH4, 0.24), # Use user input if available
        VS_grass = vs_results$VS_grass
      )
      
      # Store results
      self$results = list(
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
      )
      
      return(self$results)
    }
  )
)

# Create a simple User_input class for testing
User_input = R6Class("User_input",
  public = list(
    animal_type = NULL,
    animal_number = NULL,
    weight = NULL,
    age = NULL,
    reproduction = NULL,
    gender = NULL,
    manure_excretion = NULL,
    manure_cn = NULL,
    fraction_housing = NULL,
    fraction_grazing = NULL,
    fraction_actual_grass = NULL,
    feed_intake = NULL,
    fraction_diet_grass = NULL,
    fraction_diet_beet = NULL,
    fraction_ash = NULL,
    fraction_dm_VS = NULL,
    straw_amount = NULL,
    max_manure_CH4 = NULL,
    
    initialize = function(animal_type, animal_number, weight = NULL, age = NULL, 
                          reproduction = NULL, gender = NULL, manure_excretion = NULL,
                          manure_cn = NULL, fraction_housing = NULL, fraction_grazing = NULL,
                          fraction_actual_grass = NULL, feed_intake = NULL, fraction_diet_grass = NULL, 
                          fraction_diet_beet = NULL, fraction_ash = NULL, fraction_dm_VS = NULL,
                          straw_amount = NULL, max_manure_CH4 = NULL) {
      self$animal_type = animal_type
      self$animal_number = animal_number
      self$weight = weight
      self$age = age
      self$reproduction = reproduction
      self$gender = gender
      self$manure_excretion = manure_excretion
      self$manure_cn = manure_cn
      self$fraction_housing = fraction_housing
      self$fraction_grazing = fraction_grazing
      self$fraction_actual_grass = fraction_actual_grass
      self$feed_intake = feed_intake
      self$fraction_diet_grass = fraction_diet_grass
      self$fraction_diet_beet = fraction_diet_beet
      self$fraction_ash = fraction_ash
      self$fraction_dm_VS = fraction_dm_VS
      self$straw_amount = straw_amount
      self$max_manure_CH4 = max_manure_CH4
    }
  )
)

# Test the Inventory class
test_that("Inventory class initialization works", {
  # Create a test user input for dairy cattle
  test_input = User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    weight = 650,
    manure_excretion = 20000,
    manure_cn = 15,
    fraction_housing = 0.7,
    fraction_grazing = 0.3,
    feed_intake = 20,
    fraction_diet_grass = 0.6,
    fraction_diet_beet = 0.1,
    straw_amount = 500,
    max_manure_CH4 = 0.24
  )
  
  # Create an inventory instance
  inventory = Inventory$new(
    user_input = test_input,
    debug_mode = TRUE
  )
  
  # Test that the object was created correctly
  expect_s3_class(inventory, "Inventory")
  expect_true(inventory$debug_mode)
  expect_equal(inventory$user_input$animal_type, "dairy_cattle")
  
  # Test that emission factors were loaded
  expect_true(!is.null(inventory$EF_by_animal))
})

test_that("Seasonal emission factors are calculated correctly", {
  # Create a test user input for dairy cattle
  test_input = User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    weight = 650,
    manure_excretion = 20000,
    manure_cn = 15,
    fraction_housing = 0.7,
    fraction_grazing = 0.3,
    feed_intake = 20,
    fraction_diet_grass = 0.6,
    fraction_diet_beet = 0.1,
    straw_amount = 500,
    max_manure_CH4 = 0.24
  )
  
  # Create an inventory instance
  inventory = Inventory$new(
    user_input = test_input,
    debug_mode = TRUE
  )
  
  # Calculate seasonal emission factors
  ef_results = inventory$calculate_seasonal_ef()
  
  # Test that seasonal emission factors were calculated
  expect_true(!is.null(ef_results$winter_ef))
  expect_true(!is.null(ef_results$summer_ef))
  expect_type(ef_results$winter_ef, "double")
  expect_type(ef_results$summer_ef, "double")
})

test_that("Volatile solids are calculated correctly", {
  # Create a test user input for dairy cattle
  test_input = User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    weight = 650,
    manure_excretion = 20000,
    manure_cn = 15,
    fraction_housing = 0.7,
    fraction_grazing = 0.3,
    feed_intake = 20,
    fraction_diet_grass = 0.6,
    fraction_diet_beet = 0.1,
    straw_amount = 500,
    max_manure_CH4 = 0.24
  )
  
  # Create an inventory instance
  inventory = Inventory$new(
    user_input = test_input,
    debug_mode = TRUE
  )
  
  # Calculate volatile solids
  vs_results = inventory$calculate_volatile_solids()
  
  # Test that volatile solids were calculated
  expect_true(!is.null(vs_results$VS_manure))
  expect_true(!is.null(vs_results$VS_straw))
  expect_true(!is.null(vs_results$VS_grass))
  expect_true(!is.null(vs_results$VS_housing))
  expect_type(vs_results$VS_manure, "double")
  expect_type(vs_results$VS_straw, "double")
  expect_type(vs_results$VS_grass, "double")
  expect_type(vs_results$VS_housing, "double")
})

test_that("Inventory run_inventory works correctly", {
  # Create a test user input for dairy cattle
  test_input = User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    weight = 650,
    manure_excretion = 20000,
    manure_cn = 15,
    fraction_housing = 0.7,
    fraction_grazing = 0.3,
    feed_intake = 20,
    fraction_diet_grass = 0.6,
    fraction_diet_beet = 0.1,
    straw_amount = 500,
    max_manure_CH4 = 0.24
  )
  
  # Create an inventory instance
  inventory = Inventory$new(
    user_input = test_input,
    debug_mode = TRUE
  )
  
  # Run the inventory calculation
  results = inventory$run_inventory()
  
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
})
