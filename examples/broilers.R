#' @title Run Example for Danish Broilers
#' @description This script demonstrates how to use the rCH4 package to calculate
#' CH4 emissions from broilers under Danish conditions.
#'
#' @author João Serra
#' @date May 2025

# Source required files
source("R/User_input.R")
source("R/Inventory.r")
source("R/Enteric_fermentation.r")
source("R/Excreted_volatile_solids.R")
source("R/utils_enteric_fermentation.R")

# Load required packages
library(R6)
library(yaml)

# Override config paths for direct execution
User_Input$set("public", "config_paths", list(
  global_parameters = "inst/extdata/global_parameters.yaml",
  enteric_fermentation = "inst/extdata/enteric_fermentation.yaml",
  straw_litter = "inst/extdata/straw_litter.yaml",
  manure_management = "inst/extdata/manure_management.yaml",
  mms_CH4 = "inst/extdata/mms_CH4.yaml"
))

# Override Inventory config paths
Inventory$set("public", "config_paths", list(
  enteric_fermentation = "inst/extdata/enteric_fermentation.yaml",
  global_parameters = "inst/extdata/global_parameters.yaml",
  manure_management = "inst/extdata/manure_management.yaml"
))

# ---- Broilers Example ----
cat("=== Danish Broilers Example ===\n\n")

# Create a User_input object for broilers based on Danish conditions
broilers_input <- User_Input$new(
  animal_type = "broilers",
  animal_number = 50000,  # 50,000 head (typical commercial unit size)
  weight = 2.1,          # 2.1 kg average weight at slaughter
  manure_excretion = 10.5,  # kg/(head.yr) - Danish standard value
  manure_cn = 8,         # C:N ratio of manure
  fraction_housing = 1.0,  # 100% time in housing
  fraction_grazing = 0.0,  # 0% time grazing
  feed_intake = 40,      # Feed Units/(head.yr) - Danish standard for broilers
  fraction_diet_grass = 0.0,  # No grass in diet
  max_manure_CH4 = 0.00036  # m3 CH4/kg VS - From Danish emission inventory
)

# Print the input parameters
cat("Broilers Input Parameters:\n")
broilers_input$print()

# Check if the input is valid
if (broilers_input$valid) {
  # Create an Inventory object and run calculations
  broilers_inventory <- Inventory$new(broilers_input, debug_mode = TRUE)
  broilers_results <- broilers_inventory$run_inventory()
  
  # Print the results in a formatted way
  cat("\nBroilers Results Summary:\n")
  cat("------------------------\n")
  cat(sprintf("Enteric Fermentation CH4: %.4f kg CH4/(head.yr)\n", 
              broilers_results$emissions$ch4_enteric / broilers_input$animal_number))
  cat(sprintf("Manure Management CH4 (housing): %.4f kg CH4/(head.yr)\n", 
              broilers_results$emissions$ch4_manure$housing / broilers_input$animal_number))
  cat(sprintf("Manure Management CH4 (grazing): %.4f kg CH4/(head.yr)\n", 
              broilers_results$emissions$ch4_manure$grass / broilers_input$animal_number))
  cat(sprintf("Total CH4 emissions: %.4f kg CH4/(head.yr)\n", 
              broilers_results$emissions$ch4_total / broilers_input$animal_number))
  cat(sprintf("Total CH4 emissions for flock: %.2f kg CH4/yr\n", 
              broilers_results$emissions$ch4_total))
  
  # Calculate emissions per kg of manure
  emissions_per_manure <- broilers_results$emissions$ch4_total / 
                         (broilers_input$manure_excretion * broilers_input$animal_number)
  cat(sprintf("CH4 emissions per kg manure: %.6f kg CH4/kg manure\n", emissions_per_manure))
} else {
  cat("Invalid input parameters for broilers:\n")
  cat(broilers_input$get_validation_message_string())
}

# End of example
cat("\n\nBroilers example completed successfully.\n")
