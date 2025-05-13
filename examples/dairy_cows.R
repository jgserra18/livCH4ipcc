#' @title Run Example for Danish Dairy Cows
#' @description This script demonstrates how to use the livCH4ipcc package to calculate
#' CH4 emissions from dairy cows under Danish conditions.
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

# ---- Dairy Cattle Example ----
cat("=== Danish Dairy Cattle Example ===\n\n")

# No need to define custom config paths - the package handles this automatically

# Create a User_input object for dairy cattle based on Danish conditions
dairy_input <- User_Input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,  # 100 head
  weight = 650,         # 650 kg average weight
  manure_excretion = 21900,  # kg/(head.yr) - Danish standard value
  manure_cn = 15,       # C:N ratio of manure
  fraction_housing = 0.8,  # 80% time in housing
  fraction_grazing = 0.2,  # 20% time grazing
  fraction_actual_grass = 0.15,  # Actual days on grass considering Danish climate
  feed_intake = 7300,   # kg DM/(head.yr) - Danish standard for high-yielding dairy cows
  fraction_diet_grass = 0.3,  # 30% grass in diet
  fraction_diet_beet = 0.05,  # 5% beet in diet
  max_manure_CH4 = 0.24  # m3 CH4/kg VS - From Danish emission inventory
)

# Print the input parameters
cat("Dairy Cattle Input Parameters:\n")
dairy_input$print()

# Check if the input is valid
if (dairy_input$valid) {
  # Create an Inventory object and run calculations
  dairy_inventory <- Inventory$new(dairy_input, debug_mode = TRUE)
  dairy_results <- dairy_inventory$run_inventory()
  
  # Print the results in a formatted way
  cat("\nDairy Cattle Results Summary:\n")
  cat("-----------------------------\n")
  cat(sprintf("Enteric Fermentation CH4: %.2f kg CH4/(head.yr)\n", 
              dairy_results$emissions$ch4_enteric / dairy_input$animal_number))
  cat(sprintf("Manure Management CH4 (housing): %.2f kg CH4/(head.yr)\n", 
              dairy_results$emissions$ch4_manure$housing / dairy_input$animal_number))
  cat(sprintf("Manure Management CH4 (grazing): %.2f kg CH4/(head.yr)\n", 
              dairy_results$emissions$ch4_manure$grass / dairy_input$animal_number))
  cat(sprintf("Total CH4 emissions: %.2f kg CH4/(head.yr)\n", 
              dairy_results$emissions$ch4_total / dairy_input$animal_number))
  cat(sprintf("Total CH4 emissions for herd: %.2f kg CH4/yr\n", 
              dairy_results$emissions$ch4_total))
  
  # Calculate emissions per kg of manure
  emissions_per_manure <- dairy_results$emissions$ch4_total / (dairy_input$manure_excretion * dairy_input$animal_number)
  cat(sprintf("CH4 emissions per kg manure: %.4f kg CH4/kg manure\n", emissions_per_manure))
} else {
  cat("Invalid input parameters for dairy cattle:\n")
  cat(dairy_input$get_validation_message_string())
}

# End of example
cat("\n\nDairy cattle example completed successfully.\n")
