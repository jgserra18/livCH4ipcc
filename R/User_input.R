#' @title User Input Class for Livestock CH4 Emissions
#' @description Class for validating user input parameters for livestock CH4 emission calculations
#' with a focus on feeding and housing components.
#'
#' The User_Input class handles validation of all parameters needed for the CH4 emission
#' calculations from livestock, particularly focusing on feeding and housing components.
#' It ensures that input parameters are valid before they are used in calculations.
#'
#' @details This class validates the following parameters:
#' \itemize{
#'   \item Animal type and number
#'   \item Animal characteristics (weight, age, reproduction, gender)
#'   \item Manure excretion and C:N ratio
#'   \item Housing and grazing fractions
#'   \item Feed intake and diet composition
#'   \item Volatile solids parameters (f_ash, f_dm_VS)
#' }
#'
#' @examples
#' # Create a new User_Input object for dairy cattle
#' input = User_Input$new(
#'   animal_type = "dairy_cattle",
#'   animal_number = 100,
#'   manure_excretion = 20000,
#'   manure_cn = 15,
#'   fraction_housing = 0.7,
#'   fraction_grazing = 0.3,
#'   fraction_diet_grass = 0.6,
#'   fraction_diet_beet = 0.1
#' )
#'
#' # Check if the input is valid
#' if (input$valid) {
#'   # Get the parameters as a list for use in calculations
#'   params = input$get_parameters()
#'   # Use the parameters in your calculations
#' } else {
#'   # Print validation errors
#'   cat(input$get_validation_message_string())
#' }
#'
#' @importFrom yaml read_yaml
NULL

#' User_Input Class for Livestock CH4 Emissions
#'
#' @description An R6 class to validate user input for livestock CH4 emission calculations
#' with a focus on feeding and housing components. This class only handles input validation
#' and does not run any calculations.
#'
#' @export
User_Input = R6::R6Class("User_Input",
  public = list(
    #' @field animal_type Character string representing the animal type (e.g., "dairy_cattle", "fattening_pigs")
    animal_type = NULL,
    
    #' @field animal_number Number of animals (head/yr)
    animal_number = NULL,
    
    #' @field weight Animal weight (kg). Optional, depends on animal type.
    weight = NULL,
    
    #' @field age Animal age (months). Optional, depends on animal type.
    age = NULL,
    
    #' @field reproduction Boolean indicating if the animal is used for reproduction. Optional.
    reproduction = NULL,
    
    #' @field gender Character string representing the animal gender ("male", "female"). Optional.
    gender = NULL,
    
    #' @field manure_excretion Manure excretion (kg/(head.yr))
    manure_excretion = NULL,
    
    #' @field manure_cn C:N ratio of manure
    manure_cn = NULL,
    
    #' @field fraction_housing Fraction of time spent in housing [0-1]. Must sum to 1 with grazing.
    fraction_housing = NULL,
    
    #' @field fraction_grazing Fraction of time spent grazing [0-1]. Must sum to 1 with housing.
    fraction_grazing = NULL,
    
    #' @field fraction_actual_grass Fraction of actual days on grass [0-1]. May differ from fraction_grazing due to seasonal variations.
    fraction_actual_grass = NULL,
    
    #' @field feed_intake Feed intake (kg DM/(head.yr)). Optional, default values will be used if NULL.
    feed_intake = NULL,
    
    #' @field fraction_diet_grass Fraction of diet that is grass [0-1]
    fraction_diet_grass = NULL,
    
    #' @field fraction_diet_beet Fraction of diet that is beet [0-1]. Required for dairy cattle.
    fraction_diet_beet = NULL,
    
    #' @field fraction_ash Ash fraction in volatile solids [0-1]. Optional, default from global_parameters.yaml.
    fraction_ash = NULL,
    
    #' @field fraction_dm_VS Dry matter to volatile solids conversion factor [0-1]. Optional, default from global_parameters.yaml.
    fraction_dm_VS = NULL,
    
    #' @field straw_amount Amount of straw used (kg/(head.yr)). Optional.
    straw_amount = NULL,
    
    #' @field straw_dm Dry matter content of straw [0-1]. Optional.
    straw_dm = NULL,
    
    #' @field valid Boolean indicating if all input parameters are valid
    valid = FALSE,
    
    #' @field validation_messages List of validation error messages if any parameters are invalid
    validation_messages = NULL,
    
    #' @field config_paths Paths to configuration files
    config_paths = list(
      global_parameters = "extdata/global_parameters.yaml",
      enteric_fermentation = "extdata/enteric_fermentation.yaml",
      straw_litter = "extdata/straw_litter.yaml",
      manure_management = "extdata/manure_management.yaml"
    ),
    
    #' @field max_manure_CH4 Maximum methane production potential of manure (m3 CH4/kg VS). Optional, default from manure_management.yaml.
    max_manure_CH4 = NULL,
    
    #' @description Create a new User_Input object and validate the input parameters
    #' @param animal_type Character string representing the animal type (e.g., "dairy_cattle", "fattening_pigs")
    #' @param animal_number Number of animals (head/yr)
    #' @param weight Animal weight (kg). Optional, depends on animal type.
    #' @param age Animal age (months). Optional, depends on animal type.
    #' @param reproduction Boolean indicating if the animal is used for reproduction. Optional.
    #' @param gender Character string representing the animal gender ("male", "female"). Optional.
    #' @param manure_excretion Manure excretion (kg/(head.yr))
    #' @param manure_cn C:N ratio of manure
    #' @param fraction_housing Fraction of time spent in housing [0-1]. Must sum to 1 with grazing.
    #' @param fraction_grazing Fraction of time spent grazing [0-1]. Must sum to 1 with housing.
    #' @param fraction_actual_grass Fraction of actual days on grass [0-1]. May differ from fraction_grazing due to seasonal variations.
    #' @param feed_intake Feed intake (kg DM/(head.yr)). Optional, default values will be used if NULL.
    #' @param fraction_diet_grass Fraction of diet that is grass [0-1]
    #' @param fraction_diet_beet Fraction of diet that is beet [0-1]. Required for dairy cattle.
    #' @param fraction_ash Ash fraction in volatile solids [0-1]. Optional, default from global_parameters.yaml.
    #' @param fraction_dm_VS Dry matter to volatile solids conversion factor [0-1]. Optional, default from global_parameters.yaml.
    #' @param straw_amount Amount of straw used (kg/(head.yr)). Optional.
    #' @param straw_dm Dry matter content of straw [0-1]. Optional.
    #' @param max_manure_CH4 Maximum methane production potential of manure (m3 CH4/kg VS). Optional, default from manure_management.yaml.
    #' @return A new User_Input object with validation results
    initialize = function(animal_type,
                          animal_number,
                          weight = NULL,
                          age = NULL,
                          reproduction = NULL,
                          gender = NULL,
                          manure_excretion,
                          manure_cn,
                          fraction_housing,
                          fraction_grazing,
                          fraction_actual_grass = NULL,
                          feed_intake = NULL,
                          fraction_diet_grass = NULL,
                          fraction_diet_beet = NULL,
                          fraction_ash = NULL,
                          fraction_dm_VS = NULL,
                          straw_amount = NULL,
                          straw_dm = NULL,
                          max_manure_CH4 = NULL) {
      
      # Store the input parameters
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
      self$straw_dm = straw_dm
      self$max_manure_CH4 = max_manure_CH4
      
      # Initialize validation messages
      self$validation_messages = list()
      
      # Fill in missing parameters from configuration files
      self$fill_default_values()
      
      # Validate the input parameters
      self$validate()
    },
    
    #' @description Validate all input parameters for the CH4 emission calculations
    #' Checks that all fractions sum to 1 where appropriate and that all values are within valid ranges.
    #' @return TRUE if all parameters are valid, FALSE otherwise
    validate = function() {
      # Reset validation status
      self$valid = TRUE
      self$validation_messages = list()
      
      # Validate animal_type
      if (is.null(self$animal_type) || !is.character(self$animal_type)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "animal_type must be a character string")
      } 
      else {
        # Check if animal_type is in the list of supported types
        mms_ch4_file = system.file(self$config_paths$mms_CH4, package = "livCH4ipcc")
        if (mms_ch4_file == "") {
          mms_ch4_file = file.path(system.file("", package = "livCH4ipcc"), self$config_paths$mms_CH4)
        }
        
        if (file.exists(mms_ch4_file)) {
          mms_ch4_data = read_yaml(mms_ch4_file)
          if (!self$animal_type %in% names(mms_ch4_data$animal_types)) {
            self$valid = FALSE
            self$validation_messages = c(self$validation_messages, 
                                        paste0("animal_type '", self$animal_type, "' is not supported. ",
                                              "Supported types: ", paste(names(mms_ch4_data$animal_types), collapse = ", ")))
          }
        } else {
          self$valid = FALSE
          self$validation_messages = c(self$validation_messages, "Could not find mms_CH4.yaml configuration file")
        }
      }
      
      # Validate animal_number
      if (is.null(self$animal_number) || !is.numeric(self$animal_number) || self$animal_number <= 0) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "animal_number must be a positive number")
      }
      
      # Validate manure_excretion
      if (is.null(self$manure_excretion) || !is.numeric(self$manure_excretion) || self$manure_excretion <= 0) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "manure_excretion must be a positive number")
      }
      
      # Validate manure_cn
      if (is.null(self$manure_cn) || !is.numeric(self$manure_cn) || self$manure_cn <= 0) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "manure_cn must be a positive number")
      }
      
      # Validate fraction_housing and fraction_grazing
      if (is.null(self$fraction_housing) || !is.numeric(self$fraction_housing) || 
          self$fraction_housing < 0 || self$fraction_housing > 1) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "fraction_housing must be a number between 0 and 1")
      }
      
      if (is.null(self$fraction_grazing) || !is.numeric(self$fraction_grazing) || 
          self$fraction_grazing < 0 || self$fraction_grazing > 1) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "fraction_grazing must be a number between 0 and 1")
      }
      
      # Check that fraction_housing + fraction_grazing <= 1
      if (!is.null(self$fraction_housing) && !is.null(self$fraction_grazing) && 
          (self$fraction_housing + self$fraction_grazing > 1.0)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                    "fraction_housing + fraction_grazing must not exceed 1")
      }
      
      # Validate fraction_diet_grass
      if (is.null(self$fraction_diet_grass) || !is.numeric(self$fraction_diet_grass) || 
          self$fraction_diet_grass < 0 || self$fraction_diet_grass > 1) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "fraction_diet_grass must be a number between 0 and 1")
      }
      
      # Validate fraction_diet_beet for dairy cattle
      if (self$animal_type == "dairy_cattle" && 
          (is.null(self$fraction_diet_beet) || !is.numeric(self$fraction_diet_beet) || 
           self$fraction_diet_beet < 0 || self$fraction_diet_beet > 1)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                    "fraction_diet_beet must be a number between 0 and 1 for dairy cattle")
      }
      
      # Validate optional parameters if provided
      if (!is.null(self$weight) && (!is.numeric(self$weight) || self$weight <= 0)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "weight must be a positive number")
      }
      
      if (!is.null(self$age) && (!is.numeric(self$age) || self$age < 0)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "age must be a non-negative number")
      }
      
      if (!is.null(self$reproduction) && !is.logical(self$reproduction)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "reproduction must be TRUE or FALSE")
      }
      
      if (!is.null(self$gender) && !(self$gender %in% c("male", "female"))) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "gender must be 'male' or 'female'")
      }
      
      if (!is.null(self$feed_intake) && (!is.numeric(self$feed_intake) || self$feed_intake <= 0)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "feed_intake must be a positive number")
      }
      
      if (!is.null(self$fraction_ash) && (!is.numeric(self$fraction_ash) || self$fraction_ash < 0 || self$fraction_ash > 1)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "fraction_ash must be a number between 0 and 1")
      }
      
      if (!is.null(self$fraction_dm_VS) && (!is.numeric(self$fraction_dm_VS) || self$fraction_dm_VS < 0 || self$fraction_dm_VS > 1)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "fraction_dm_VS must be a number between 0 and 1")
      }
      
      # Validate fraction_actual_grass if provided
      if (!is.null(self$fraction_actual_grass) && (!is.numeric(self$fraction_actual_grass) || self$fraction_actual_grass < 0 || self$fraction_actual_grass > 1)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "fraction_actual_grass must be a number between 0 and 1")
      }
      
      # Validate straw_amount if provided
      if (!is.null(self$straw_amount) && (!is.numeric(self$straw_amount) || self$straw_amount < 0)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "straw_amount must be a non-negative number")
      }
      
      # Validate straw_dm if provided
      if (!is.null(self$straw_dm) && (!is.numeric(self$straw_dm) || self$straw_dm < 0 || self$straw_dm > 1)) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, "straw_dm must be a number between 0 and 1")
      }
      
      return(self$valid)
    },
    
    #' @description Get a string of all validation messages
    #' @return A string containing all validation messages separated by newlines
    get_validation_message_string = function() {
      if (length(self$validation_messages) == 0) {
        return("All input parameters are valid.")
      } 
      else {
        return(paste(self$validation_messages, collapse = "\n"))
      }
    },
    
    #' @description Get all parameters as a list
    #' @return A list containing all input parameters
    get_parameters = function() {
      return(list(
        animal_type = self$animal_type,
        animal_number = self$animal_number,
        weight = self$weight,
        age = self$age,
        reproduction = self$reproduction,
        gender = self$gender,
        manure_excretion = self$manure_excretion,
        manure_cn = self$manure_cn,
        fraction_housing = self$fraction_housing,
        fraction_grazing = self$fraction_grazing,
        fraction_actual_grass = self$fraction_actual_grass,
        feed_intake = self$feed_intake,
        fraction_diet_grass = self$fraction_diet_grass,
        fraction_diet_beet = self$fraction_diet_beet,
        fraction_ash = self$fraction_ash,
        fraction_dm_VS = self$fraction_dm_VS,
        straw_amount = self$straw_amount,
        straw_dm = self$straw_dm
      ))
    },
    
    #' @description Print the User_Input object
    #' @return NULL
    print = function() {
      cat("=== User Input Parameters ===\n")
      cat("Animal type:", self$animal_type, "\n")
      cat("Number of animals:", self$animal_number, "head/yr\n")
      
      if (!is.null(self$weight)) cat("Weight:", self$weight, "kg\n")
      if (!is.null(self$age)) cat("Age:", self$age, "months\n")
      if (!is.null(self$reproduction)) cat("Reproduction:", self$reproduction, "\n")
      if (!is.null(self$gender)) cat("Gender:", self$gender, "\n")
      
      cat("Manure excretion:", self$manure_excretion, "kg/(head.yr)\n")
      cat("Manure C:N ratio:", self$manure_cn, "\n")
      cat("Fraction housing:", self$fraction_housing, "\n")
      cat("Fraction grazing:", self$fraction_grazing, "\n")
      if (!is.null(self$fraction_actual_grass)) cat("Fraction actual grass:", self$fraction_actual_grass, "\n")
      
      if (!is.null(self$feed_intake)) cat("Feed intake:", self$feed_intake, "kg DM/(head.yr)\n")
      cat("Fraction diet grass:", self$fraction_diet_grass, "\n")
      if (!is.null(self$fraction_diet_beet)) cat("Fraction diet beet:", self$fraction_diet_beet, "\n")
      
      if (!is.null(self$fraction_ash)) cat("Fraction ash:", self$fraction_ash, "\n")
      if (!is.null(self$fraction_dm_VS)) cat("Fraction DM to VS:", self$fraction_dm_VS, "\n")
      if (!is.null(self$straw_amount)) cat("Straw amount:", self$straw_amount, "kg/(head.yr)\n")
      if (!is.null(self$straw_dm)) cat("Straw dry matter content:", self$straw_dm, "\n")
      
      cat("\nValidation status:", if(self$valid) "Valid" else "Invalid", "\n")
      if (!self$valid) {
        cat("Validation messages:\n")
        cat(self$get_validation_message_string(), "\n")
      }
      
      invisible(self)
    },
    
    #' @description Determine the appropriate animal type based on provided parameters
    #' @return Character string representing the resolved animal type
    resolve_animal_type = function() {
      # Extract the base type (without underscore)
      base_type = gsub("_$", "", self$animal_type)  # Remove trailing underscore if present
      underscore_type = paste0(base_type, "_")
      
      # Check if the animal type contains an underscore at the end
      has_underscore = grepl("_$", self$animal_type)
      
      # List of parameters that differentiate between base and underscore variants
      differentiating_params = list(
        weight = self$weight,
        age = self$age,
        reproduction = self$reproduction,
        gender = self$gender
      )
      
      # If none of the differentiating parameters are provided and the type doesn't already have an underscore,
      # default to the base type without underscore
      if (all(sapply(differentiating_params, is.null))) {
        # Check if both base and underscore types exist in configuration files
        enteric_fermentation_file = system.file(self$config_paths$enteric_fermentation, package = "livCH4ipcc")
        if (enteric_fermentation_file == "") {
          enteric_fermentation_file = file.path(system.file("", package = "livCH4ipcc"), self$config_paths$enteric_fermentation)
        }
        
        if (file.exists(enteric_fermentation_file)) {
          ef_data = tryCatch({
            read_yaml(enteric_fermentation_file)
          }, error = function(e) {
            NULL
          })
          
          if (!is.null(ef_data) && !is.null(ef_data$animal_types)) {
            # Check if both variants exist
            base_exists = base_type %in% names(ef_data$animal_types)
            underscore_exists = underscore_type %in% names(ef_data$animal_types)
            
            if (base_exists && underscore_exists) {
              # If both exist, default to the base type
              return(base_type)
            } 
            else if (base_exists) {
              return(base_type)
            } 
            else if (underscore_exists) {
              return(underscore_type)
            }
          }
        }
        
        # If we couldn't determine from config, use the original type without underscore
        return(base_type)
      }
      
      # If differentiating parameters are provided, keep the original type as specified
      return(self$animal_type)
    },
    
    #' @description Fill in default values from configuration files
    #' @return NULL
    fill_default_values = function() {
      # Resolve the animal type based on provided parameters
      resolved_animal_type = self$resolve_animal_type()
      
      # If the resolved type is different from the original, update it
      if (resolved_animal_type != self$animal_type) {
        self$animal_type = resolved_animal_type
      }
      
      # Load global parameters for fraction_ash and fraction_dm_VS if not provided
      if (is.null(self$fraction_ash) || is.null(self$fraction_dm_VS)) {
        self$load_global_parameters()
      }
      
      # Load manure management parameters if needed
      if (is.null(self$fraction_actual_grass) || is.null(self$max_manure_CH4)) {
        self$load_manure_management_parameters()
      }
      
      # Set fraction_actual_grass to fraction_grazing if still not provided
      if (is.null(self$fraction_actual_grass)) {
        self$fraction_actual_grass = self$fraction_grazing
      }
      
      # Load straw parameters if needed
      if (is.null(self$straw_dm)) {
        self$load_straw_parameters()
      }
      
      # Load animal-specific parameters if needed
      if (is.null(self$feed_intake)) {
        self$load_animal_parameters()
      }
    },
    
    #' @description Load manure management parameters from configuration file
    #' @return NULL
    load_manure_management_parameters = function() {
      # Try to find the file in the package
      manure_management_file = system.file(self$config_paths$manure_management, package = "livCH4ipcc")
      if (manure_management_file == "") {
        manure_management_file = file.path(system.file("", package = "livCH4ipcc"), self$config_paths$manure_management)
      }
      
      if (file.exists(manure_management_file) && !is.null(self$animal_type)) {
        manure_management_data = read_yaml(manure_management_file)
        
        if (self$animal_type %in% names(manure_management_data$enteric_fermentation_EFs)) {
          animal_data = manure_management_data$enteric_fermentation_EFs[[self$animal_type]]
          
          # Set fraction_grass if not provided in input but available in config
          if (is.null(self$fraction_diet_grass) && !is.null(animal_data$fraction_grass) && animal_data$fraction_grass != "none") {
            self$fraction_diet_grass = animal_data$fraction_grass
          }
          
          # Set fraction_actual_grass if not provided in input but available in config
          if (is.null(self$fraction_actual_grass) && !is.null(animal_data$fraction_actual_grass) && animal_data$fraction_actual_grass != "none") {
            self$fraction_actual_grass = animal_data$fraction_actual_grass
          }
          
          # Set max_manure_CH4 if not provided in input but available in config
          if (is.null(self$max_manure_CH4) && !is.null(animal_data$max_manure_CH4)) {
            self$max_manure_CH4 = animal_data$max_manure_CH4
          }
        }
      } else {
        warning("Could not find manure_management.yaml configuration file or animal_type is NULL")
      }
    },
    
    #' @description Load global parameters from configuration file
    #' @return NULL
    load_global_parameters = function() {
      # Try to find the file in the package
      global_params_file = system.file(self$config_paths$global_parameters, package = "livCH4ipcc")
      if (global_params_file == "") {
        global_params_file = file.path(system.file("", package = "livCH4ipcc"), self$config_paths$global_parameters)
      }
      
      if (file.exists(global_params_file)) {
        global_params = read_yaml(global_params_file)
        
        # Set fraction_ash if not provided
        if (is.null(self$fraction_ash) && !is.null(global_params$global_parameters$f_ash)) {
          self$fraction_ash = global_params$global_parameters$f_ash
        }
        
        # Set fraction_dm_VS if not provided
        if (is.null(self$fraction_dm_VS) && !is.null(global_params$global_parameters$f_dm_VS)) {
          self$fraction_dm_VS = global_params$global_parameters$f_dm_VS
        }
      } else {
        warning("Could not find global_parameters.yaml configuration file")
      }
    },
    
    #' @description Load animal-specific parameters from configuration file
    #' @return NULL
    load_animal_parameters = function() {
      # Try to find the file in the package
      enteric_fermentation_file = system.file(self$config_paths$enteric_fermentation, package = "livCH4ipcc")
      if (enteric_fermentation_file == "") {
        enteric_fermentation_file = file.path(system.file("", package = "livCH4ipcc"), self$config_paths$enteric_fermentation)
      }
      
      if (file.exists(enteric_fermentation_file) && !is.null(self$animal_type)) {
        enteric_fermentation_data = read_yaml(enteric_fermentation_file)
        
        if (self$animal_type %in% names(enteric_fermentation_data$animal_types)) {
          animal_data = enteric_fermentation_data$animal_types[[self$animal_type]]
          
          # Set feed_intake if not provided
          if (is.null(self$feed_intake) && !is.null(animal_data$feed_intake)) {
            self$feed_intake = animal_data$feed_intake
          }
        }
      } 
      else {
        warning("Could not find enteric_fermentation.yaml configuration file or animal_type is NULL")
      }
    },
    
    #' @description Load straw parameters from configuration file
    #' @return NULL
    load_straw_parameters = function() {
      # Try to find the file in the package
      straw_file = system.file(self$config_paths$straw_litter, package = "livCH4ipcc")
      if (straw_file == "") {
        straw_file = file.path(system.file("", package = "livCH4ipcc"), self$config_paths$straw_litter)
      }
      
      if (file.exists(straw_file) && !is.null(self$animal_type)) {
        straw_data = read_yaml(straw_file)
        
        if (self$animal_type %in% names(straw_data)) {
          animal_straw_data = straw_data[[self$animal_type]]
          
          # Set straw_dm if not provided
          if (is.null(self$straw_dm) && !is.null(animal_straw_data$dm_straw)) {
            self$straw_dm = animal_straw_data$dm_straw
          }
        }
      } 
      else {
        warning("Could not find straw_litter.yaml configuration file or animal_type is NULL")
      }
    }
  )
)
