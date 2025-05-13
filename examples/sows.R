#' @title Run Example for Danish Sows
#' @description This script demonstrates how to use the rCH4 package to calculate
#' CH4 emissions from sows under Danish conditions.
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

# ---- Sows Example ----
cat("=== Danish Sows Example ===\n\n")

# Create a User_input object for sows based on Danish conditions
sows_input <- User_Input$new(
  animal_type = "sows",
  animal_number = 500,  # 500 head
  weight = 200,         # 200 kg average weight
  manure_excretion = 5800,  # kg/(head.yr) - Danish standard value
  manure_cn = 9,        # C:N ratio of manure
  fraction_housing = 1.0,  # 100% time in housing (typical for Danish pig production)
  fraction_grazing = 0.0,  # 0% time grazing
  feed_intake = 1400,   # Feed Units/(head.yr) - Danish standard for sows
  fraction_diet_grass = 0.0,  # No grass in diet
  max_manure_CH4 = 0.45  # m3 CH4/kg VS - From Danish emission inventory
)

# Print the input parameters
cat("Sows Input Parameters:\n")
sows_input$print()

# Check if the input is valid
if (sows_input$valid) {
  # Create an Inventory object and run calculations
  sows_inventory <- Inventory$new(sows_input, debug_mode = TRUE)
  sows_results <- sows_inventory$run_inventory()
  
  # Print the results in a formatted way
  cat("\nSows Results Summary:\n")
  cat("--------------------\n")
  cat(sprintf("Enteric Fermentation CH4: %.2f kg CH4/(head.yr)\n", 
              sows_results$emissions$ch4_enteric / sows_input$animal_number))
  cat(sprintf("Manure Management CH4 (housing): %.2f kg CH4/(head.yr)\n", 
              sows_results$emissions$ch4_manure$housing / sows_input$animal_number))
  cat(sprintf("Manure Management CH4 (grazing): %.2f kg CH4/(head.yr)\n", 
              sows_results$emissions$ch4_manure$grass / sows_input$animal_number))
  cat(sprintf("Total CH4 emissions: %.2f kg CH4/(head.yr)\n", 
              sows_results$emissions$ch4_total / sows_input$animal_number))
  cat(sprintf("Total CH4 emissions for herd: %.2f kg CH4/yr\n", 
              sows_results$emissions$ch4_total))
  
  # Calculate emissions per kg of manure
  emissions_per_manure <- sows_results$emissions$ch4_total / (sows_input$manure_excretion * sows_input$animal_number)
  cat(sprintf("CH4 emissions per kg manure: %.4f kg CH4/kg manure\n", emissions_per_manure))
} else {
  cat("Invalid input parameters for sows:\n")
  cat(sows_input$get_validation_message_string())
}

# End of example
cat("\n\nSows example completed successfully.\n")
