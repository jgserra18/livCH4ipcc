#' @title CH4 Inventory Class
#' @description Class for calculating CH4 emissions from livestock
#' based on user input parameters.
#'
#' The Inventory class handles the entire calculation workflow:
#' - Enteric fermentation (Tier 1 and Tier 2)
#' - Manure management
#' - Seasonal variations (winter/summer)
#'
#' @examples
#' # Create a User_input object
#' input = User_Input$new(
#'   animal_type = "dairy_cattle",
#'   animal_number = 100,
#'   weight = 650,
#'   manure_excretion = 20000,
#'   manure_cn = 15,
#'   fraction_housing = 0.7,
#'   fraction_grazing = 0.3,
#'   feed_intake = 20,
#'   fraction_diet_grass = 0.6,
#'   fraction_diet_beet = 0.1
#' )
#'
#' # Create Inventory object and run calculations
#' inv = Inventory$new(input)
#' results = inv$run_inventory()
#'
#' @importFrom R6 R6Class
#' @importFrom utils menu

# Explicitly import R6 package
library(R6)

#' Inventory Class for CH4 emissions
#'
#' @description An R6 class to perform CH4 emission calculations based on User_input
#' parameters.
#'
#' @export
#' @importFrom yaml read_yaml
#' @importFrom R6 R6Class
Inventory = R6Class("Inventory",
  public = list(
    #' @field user_input User_input object containing input parameters
    user_input = NULL,

    #' @field config_paths Paths to configuration files
    config_paths = list(
      enteric_fermentation = system.file("extdata/enteric_fermentation.yaml", package = "livCH4ipcc"),
      global_parameters = system.file("extdata/global_parameters.yaml", package = "livCH4ipcc"),
      manure_management = system.file("extdata/manure_management.yaml", package = "livCH4ipcc")
    ),

    #' @field debug_mode Enable debug mode for more verbose output
    debug_mode = TRUE,

    #' @field global_params Global parameters loaded from configuration files
    global_params = NULL,

    #' @field results Results of the simulation
    results = NULL,

    #' @field EF_by_animal Emission factors organized by animal type
    EF_by_animal = NULL,

    #' @description Create a new Inventory object
    #' @param user_input User_input object containing input parameters
    #' @param debug_mode Enable debug mode for more verbose output
    initialize = function(user_input, debug_mode = FALSE) {
      self$user_input = user_input
      self$EF_by_animal = list()
      self$debug_mode = debug_mode

      if (self$debug_mode) {
        message("Initializing Inventory object...")
        message(paste("Animal type:", user_input$animal_type))
      }

      # Load emission factors and parameters
      self$compile_emission_factors()

      if (self$debug_mode) {
        message("Inventory object initialized successfully.")
      }
    },

    #' @description Load and compile emission factors from configuration
    compile_emission_factors = function() {
      if (self$debug_mode) message("Loading emission factors...")

      # Load global parameters
      self$global_params = yaml::read_yaml(self$config_paths$global_parameters)

      # Load animal-specific emission factors
      # Load base emission factors
      ef_data = yaml::read_yaml(self$config_paths$enteric_fermentation)
      
      # Get the animal type from the user input
      animal_type = self$user_input$animal_type
      
      # Check if the animal type exists in the emission factors
      if (!(animal_type %in% names(ef_data$enteric_fermentation_EFs))) {
        # If not, try to handle "_" variants
        base_type = gsub("_$", "", animal_type)  # Remove trailing underscore if present
        underscore_type = paste0(base_type, "_")
        
        # Check if base or underscore type exists
        if (base_type %in% names(ef_data$enteric_fermentation_EFs)) {
          animal_type = base_type
          if (self$debug_mode) message(paste("Using base type:", animal_type))
        } 
        else if (underscore_type %in% names(ef_data$enteric_fermentation_EFs)) {
          animal_type = underscore_type
          if (self$debug_mode) message(paste("Using underscore type:", animal_type))
        } 
        else {
          warning(paste("Animal type", animal_type, "not found in emission factors"))
        }
      }
      
      # Access the emission factors from the enteric_fermentation_EFs section
      self$EF_by_animal = ef_data$enteric_fermentation_EFs[[animal_type]]
      
      # Adjust emission factors based on animal characteristics
      if (!is.null(self$user_input$weight)) {
        # Scale feed intake based on weight
        self$EF_by_animal$feed_intake = scale_feed_intake(
          original_feed_intake = self$EF_by_animal$feed_intake,
          reference_weight = self$EF_by_animal$weight,
          new_weight = self$user_input$weight
        )
      }
      
      if (!is.null(self$user_input$age)) {
        # Scale feed intake based on age
        self$EF_by_animal$feed_intake = scale_feed_intake_by_age(
          original_feed_intake = self$EF_by_animal$feed_intake,
          reference_age = self$EF_by_animal$age,
          new_age = self$user_input$age
        )
      }
      
      # Determine tier level for enteric fermentation
      tier_level = get_enteric_fermentation_tier(self$user_input$animal_type)
      
      # Calculate emission factors based on tier level
      if (tier_level == 1) {
        # For tier 1 animals (poultry, fur animals)
        self$EF_by_animal$EF_CH4 = .tier1_EF(
          animal_type = self$user_input$animal_type,
          age = self$user_input$age,
          hpr = self$user_input$reproduction,
          organic = FALSE  # Default to FALSE if not specified
        )
      } 
      else {
        # For tier 2 animals (cattle, pigs, etc)
        tier2_params = list(
          weight = self$user_input$weight,
          age = self$user_input$age,
          reproduction = self$user_input$reproduction,
          gender = self$user_input$gender,
          scale_by_weight = !is.null(self$user_input$weight),
          scale_by_age = !is.null(self$user_input$age)
        )
        
        tryCatch({
          self$EF_by_animal$EF_CH4 = do.call(
            .tier2_EF,
            c(list(animal_type = self$user_input$animal_type), tier2_params)
          )
        }, error = function(e) {
          if (self$debug_mode) {
            message(paste("Could not calculate Tier 2 EF:", e$message))
            message("Using default EF from configuration.")
          }
        })
      }
      
      if (self$debug_mode) message("Emission factors loaded and adjusted successfully.")
    },

    #' @description Calculate seasonal emission factors
    calculate_seasonal_ef = function() {
      if (self$debug_mode) message("Calculating seasonal emission factors...")

      if (self$user_input$animal_type == "dairy_cattle") {
        winter_ef = .entFerment_tier2_dairyCattle_EF_winter(
          feed = self$user_input$feed_intake,
          GE_winter = self$EF_by_animal$GE_winter,
          ch4_conv_rate_beet = self$EF_by_animal$methane_conversion_factor, # Using methane_conversion_factor from YAML
          f_grass = self$user_input$fraction_diet_grass,
          f_beet = self$user_input$fraction_diet_beet
        )
        
        summer_ef = .entFerment_tier2_dairyCattle_EF_summer(
          feed = self$user_input$feed_intake,
          GE_summer = self$EF_by_animal$GE_summer,
          ch4_conv_rate_grass = self$EF_by_animal$methane_conversion_factor, # Using methane_conversion_factor from YAML
          f_grass = self$user_input$fraction_diet_grass
        )
      } 
      else {
        winter_ef = .entFerment_tier2_other_EF_winter(
          feed_unit = self$user_input$feed_intake,
          GE_winter = self$EF_by_animal$GE_winter,
          ch4_conv_rate = self$EF_by_animal$methane_conversion_factor, # Using methane_conversion_factor from YAML
          f_grass = self$user_input$fraction_diet_grass
        )
        
        summer_ef = .entFerment_tier2_other_EF_summer(
          feed_unit = self$user_input$feed_intake,
          GE_summer = self$EF_by_animal$GE_summer,
          ch4_conv_rate = self$EF_by_animal$methane_conversion_factor, # Using methane_conversion_factor from YAML
          f_grass = self$user_input$fraction_diet_grass
        )
      }

      return(list(winter_ef = winter_ef, summer_ef = summer_ef))
    },

    #' @description Calculate volatile solids
    calculate_volatile_solids = function() {
      if (self$debug_mode) message("Calculating volatile solids...")

      VS_manure = manure_volatile_solids(
        m_manure_excreted = self$user_input$manure_excretion,
        dm_manure = self$global_params$dm_manure,
        fraction_dm_VS = self$user_input$fraction_dm_VS,
        fraction_grass = self$user_input$fraction_grazing
      )
      
      # Get straw amount and dry matter content, with defaults if not provided
      straw_amount <- if (!is.null(self$user_input$straw_amount)) self$user_input$straw_amount else 0
      dm_straw <- if (!is.null(self$global_params$dm_straw)) self$global_params$dm_straw else 0.85
      
      # Calculate straw volatile solids with proper parameter names
      VS_straw = straw_volatile_solids(
        m_straw = straw_amount,
        dm_straw = dm_straw,
        fraction_ash = if (!is.null(self$user_input$fraction_ash)) self$user_input$fraction_ash else 0.045,
        fraction_actual_grass = if (!is.null(self$user_input$fraction_actual_grass)) self$user_input$fraction_actual_grass else 0
      )
      
      VS_grass = grass_volatile_solids(
        m_manure_excreted = self$user_input$manure_excretion,
        dm_manure = self$global_params$dm_manure,
        fraction_dm_VS = self$user_input$fraction_dm_VS,
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

    #' @description Run the inventory calculation
    run_inventory = function() {
      if (self$debug_mode) message("Running inventory calculation...")

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
        max_manure_CH4 = self$global_params$max_manure_CH4,
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

      if (self$debug_mode) {
        message("Inventory calculation completed.")
        self$print_inventory_results()
      }

      return(self$results)
    },

    #' @description Print a summary of the inventory results
    print_inventory_results = function() {
      cat("\nCH4 Inventory Results\n")
      cat("===================\n")
      cat(sprintf("Animal Type: %s\n", self$user_input$animal_type))
      cat(sprintf("Number of Animals: %d\n", self$user_input$animal_no))
      cat("\nEmission Factors:\n")
      cat(sprintf("  Winter: %.2f kg CH4/(head.yr)\n", self$results$emission_factors$winter_ef))
      cat(sprintf("  Summer: %.2f kg CH4/(head.yr)\n", self$results$emission_factors$summer_ef))
      cat("\nEmissions:\n")
      cat(sprintf("  Enteric CH4: %.2f kg CH4/yr\n", self$results$emissions$ch4_enteric))
      cat(sprintf("  Manure CH4: %.2f kg CH4/yr\n", self$results$emissions$ch4_manure))
      cat(sprintf("  Total CH4: %.2f kg CH4/yr\n", self$results$emissions$ch4_total))
    }
  )
)
