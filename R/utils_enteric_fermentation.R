#' Get Enteric Fermentation Tier Level
#' 
#' This function determines whether an animal type should use Tier 1 or Tier 2 methodology
#' for calculating enteric fermentation emission factors.
#'
#' @param animal_type Character string specifying the animal type
#' @return Integer (1 or 2) indicating the tier level for enteric fermentation calculations
#' @export
#'
#' @examples
#' get_enteric_fermentation_tier("dairy_cattle")  # Returns 2
#' get_enteric_fermentation_tier("laying_hens")   # Returns 1
get_enteric_fermentation_tier = function(animal_type) {
  # Define tier 1 animals (poultry and fur animals)
  tier1_animals = c("laying_hens", "laying_hens_", 
                   "broilers", "broilers_", 
                   "fur_animals", "fur_animals_")
  
  # Define tier 2 animals
  tier2_animals = c("dairy_cattle", "calf", "calf_", 
                   "suckling_cattle", "bull", "bull_", 
                   "sheep", "fattening_pigs", "sows", 
                   "piglets", "deer", "goats", 
                   "horses", "turkeys", "turkeys_")
  
  # Determine tier level
  if (animal_type %in% tier1_animals) {
    return(1)
  } else if (animal_type %in% tier2_animals) {
    return(2)
  } else {
    stop(paste("Unknown animal type:", animal_type))
  }
}

#' Scale Feed Intake Based on Body Weight
#'
#' This helper function scales feed intake based on a new body weight compared to a reference weight.
#' It uses the formula feed_intake = original_feed_intake * (new_weight/reference_weight)^0.75
#' If a weight range is provided (e.g., "400-600"), it calculates the average of these values.
#'
#' @param original_feed_intake Numeric value representing the original feed intake (Feed Unit/yr)
#' @param reference_weight Numeric or character value representing the reference weight in the YAML file
#'                        Can be a single number, a range (e.g., "400-600"), or "none"
#' @param new_weight Numeric value representing the new weight to scale to
#' @param reference_weight_default Numeric value to use as reference weight if reference_weight is "none" (default: 600)
#'
#' @return Numeric value representing the scaled feed intake
#' @export
#'
#' @examples
#' # Scale feed intake from reference weight 600 to new weight 400
#' scale_feed_intake(1047, 600, 400)
#'
#' # Scale feed intake with a weight range as reference
#' scale_feed_intake(1047, "400-600", 500)
#'
#' # Scale feed intake with reference weight "none" (will use default 600)
#' scale_feed_intake(1047, "none", 500)
scale_feed_intake =  function(original_feed_intake, reference_weight, new_weight, reference_weight_default = 600) {
  # If reference_weight is "none", use the default reference weight
  if (is.character(reference_weight) && (reference_weight == "none" || reference_weight == "null")) {
    reference_weight = reference_weight_default
  }
  
  # If reference_weight is a range (e.g., "400-600"), calculate the average
  if (is.character(reference_weight) && grepl("-", reference_weight)) {
    weight_range = strsplit(reference_weight, "-")[[1]]
    lower_weight = as.numeric(trimws(weight_range[1]))
    upper_weight = as.numeric(trimws(weight_range[2]))
    reference_weight = (lower_weight + upper_weight) / 2
  }
  
  # Convert to numeric if not already
  reference_weight = as.numeric(reference_weight)
  new_weight = as.numeric(new_weight)
  
  # Scale feed intake based on the formula: feed_intake = original_feed_intake * (new_weight/reference_weight)^0.75
  scaled_feed_intake = original_feed_intake * (new_weight / reference_weight)^0.75
  
  return(scaled_feed_intake)
}

#' Scale Feed Intake Based on Age
#'
#' This helper function scales feed intake linearly based on a new age compared to a reference age.
#' It uses linear interpolation: feed_intake = original_feed_intake * (new_age/reference_age)
#' If an age range is provided (e.g., "0-6 months"), it calculates the upper value.
#'
#' @param original_feed_intake Numeric value representing the original feed intake (Feed Unit/yr)
#' @param reference_age Numeric or character value representing the reference age in the YAML file
#'                      Can be a single number, a range (e.g., "0-6 months"), or "none"
#' @param new_age Numeric value representing the new age to scale to (in the same units as reference_age)
#' @param reference_age_default Numeric value to use as reference age if reference_age is "none" (default: 12)
#'
#' @return Numeric value representing the scaled feed intake
#' @export
#'
#' @examples
#' # Scale feed intake from reference age 6 months to new age 4 months
#' scale_feed_intake_by_age(1234, 6, 4)
#'
#' # Scale feed intake with an age range as reference
#' scale_feed_intake_by_age(1234, "0-6 months", 4)
#'
#' # Scale feed intake with reference age "none" (will use default 12)
#' scale_feed_intake_by_age(1234, "none", 8)
scale_feed_intake_by_age =  function(original_feed_intake, reference_age, new_age, reference_age_default = 12) {
  # If reference_age is "none", use the default reference age
  if (is.character(reference_age) && (reference_age == "none" || reference_age == "null")) {
    reference_age = reference_age_default
  }
  
  # If reference_age is a range (e.g., "0-6 months"), extract the upper value
  if (is.character(reference_age) && grepl("-", reference_age)) {
    # Extract the numeric part before any text like "months"
    age_range = strsplit(reference_age, "-")[[1]]
    upper_age_with_unit = trimws(age_range[2])
    
    # Extract just the numeric part
    upper_age = as.numeric(gsub("[^0-9.]", "", upper_age_with_unit))
    reference_age = upper_age
  } 
  else if (is.character(reference_age)) {
    # If it's just a number with a unit (e.g., "6 months"), extract the number
    reference_age = as.numeric(gsub("[^0-9.]", "", reference_age))
  }
  
  # Convert to numeric if not already
  reference_age = as.numeric(reference_age)
  new_age = as.numeric(new_age)
  
  # Scale feed intake linearly based on age
  scaled_feed_intake = original_feed_intake * (new_age / reference_age)
  
  return(scaled_feed_intake)
}



#' Calculate Enteric Fermentation Emission Factor for Tier 1 Animals
#'
#' This function calculates the implied emission factor for enteric fermentation
#' based on animal type and specific parameters for tier 1 animals.
#' It reads the parameters directly from the mms_CH4.yaml configuration file.
#' For laying_hens and broilers, the emission factor is linearly scaled based on the
#' input age relative to the baseline age in the configuration.
#'
#' @param animal_type Character string specifying the animal type (e.g., "pheasant", "laying_hens", "broilers")
#' @param ... Additional parameters specific to the animal type:
#'   - laying_hens: hpr (logical), age (numeric in days) - required
#'   - broilers: age (numeric in days) - required, organic (logical) - required
#'
#' @return Numeric value representing the implied emission factor in kg CH4/head
#' @export
#'
#' @examples
#' # Get emission factor for pheasant
#' get_enteric_fermentation_EF("pheasant")
#'
#' # Get emission factor for laying hens with hpr=TRUE and age=140
#' get_enteric_fermentation_EF("laying_hens", hpr = TRUE, age = 140)
#'
#' # Get emission factor for laying hens with hpr=TRUE and age=200 (scaled from baseline age 140)
#' get_enteric_fermentation_EF("laying_hens", hpr = TRUE, age = 200)
#'
#' # Get emission factor for broilers with age=42 and organic=FALSE
#' get_enteric_fermentation_EF("broilers", age = 42, organic = FALSE)
#'
#' # Get emission factor for broilers with age=60 and organic=TRUE (scaled from baseline age 91)
#' get_enteric_fermentation_EF("broilers", age = 60, organic = TRUE)
.tier1_EF =  function(animal_type, ...) {
  # Get additional parameters
  params =  list(...)
  
  # Check required parameters for specific animal types
  if (animal_type == "laying_hens" || animal_type == "laying_hens_") {
    if (!"age" %in% names(params)) { stop("Parameter 'age' is required for animal type 'laying_hens'") }
    if (!"hpr" %in% names(params)) { stop("Parameter 'hpr' is required for animal type 'laying_hens'") }
  } 
  else if (animal_type == "broilers" || animal_type == "broilers_") {
    if (!"age" %in% names(params)) { stop("Parameter 'age' is required for animal type 'broilers'") }
    if (!"organic" %in% names(params)) { stop("Parameter 'organic' is required for animal type 'broilers'") }
  }
  
  # Define tier 1 animal types
  tier1_animals =  c("pheasant", "laying_hens", "laying_hens_", "broilers", "broilers_")
  
  # Check if animal type is a tier 1 animal
  if (!animal_type %in% tier1_animals) {
    stop(paste(animal_type, "is not a tier 1 animal type"))
  }
  
  # Get the YAML file path
  if (exists(".get_yaml_file", mode = "function")) {
    yaml_file =  .get_yaml_file()
  } else {
    yaml_file =  system.file("extdata", "mms_CH4.yaml", package = "rEMEP")
    if (yaml_file == "") {
      # If running outside the package context, try different relative paths
      possible_paths =  c(
        file.path("inst", "extdata", "mms_CH4.yaml"),
        file.path("../../inst", "extdata", "mms_CH4.yaml"),
        file.path("../inst", "extdata", "mms_CH4.yaml")
      )
      
      for (path in possible_paths) {
        if (file.exists(path)) {
          yaml_file =  path
          break
        }
      }
      
      if (!exists("yaml_file") || yaml_file == "") {
        stop("Could not find mms_CH4.yaml file")
      }
    }
  }
  
  # Read the YAML file
  yaml_content =  yaml::read_yaml(yaml_file)
  
  # Check if the YAML content was loaded correctly
  if (!is.list(yaml_content)) {
    stop("Failed to parse the YAML file correctly")
  }
  
  # Check if enteric_fermentation_EFs exists in the YAML file
  if (!"enteric_fermentation_EFs" %in% names(yaml_content)) {
    stop("enteric_fermentation_EFs section not found in the YAML file")
  }
  
  # Get the enteric fermentation data
  ef_data =  yaml_content$enteric_fermentation_EFs
  
  # Handle pheasant (simple case with just an emission factor)
  if (animal_type == "pheasant") {
    if (!"pheasant" %in% names(ef_data)) {  stop("Pheasant not found in emission factor data")   }
    
    pheasant_data =  ef_data$pheasant
    if ("implified_emission_factor" %in% names(pheasant_data)) {  
      return(as.numeric(pheasant_data$implified_emission_factor))  } 
    else  {  
      stop("No emission factor found for pheasant")  }
  }
  
  # Handle laying_hens and broilers with parameter matching
  if (animal_type %in% c("laying_hens", "laying_hens_", "broilers", "broilers_")) {
    if (!animal_type %in% names(ef_data)) {   stop(paste(animal_type, "not found in emission factor data"))  }
    
    # Get all entries for this animal type
    animal_data =  ef_data[[animal_type]]
    
    # Convert to list of lists if it's not already
    if (!is.list(animal_data[[1]])) {   animal_data =  list(animal_data) }
    
    # Extract input age
    input_age =  params$age
    
    # Create a copy of params without age for matching
    matching_params =  params
    matching_params$age =  NULL
    
    # Find matching entries based on other parameters (excluding age)
    matching_entries =  list()
    
    for (entry in animal_data) {
      # Skip entries without implified_emission_factor
      if (!"implified_emission_factor" %in% names(entry)) {  next  }
      
      # Check if all non-age parameters match
      match =  TRUE
      for (param_name in names(matching_params)) {
        if (!param_name %in% names(entry) || entry[[param_name]] != matching_params[[param_name]]) {
          match =  FALSE
          break
        }
      }
      
      if (match) {
        # Extract baseline age from entry
        if (is.character(entry$age) && grepl("days", entry$age)) {  
          baseline_age =  as.numeric(gsub(" days", "", entry$age))
        } else {
          baseline_age =  as.numeric(entry$age)
        }
        
        # Add to matching entries
        matching_entries[[length(matching_entries) + 1]] =  list(
          baseline_age = baseline_age,
          ef = as.numeric(entry$implified_emission_factor)
        )
      }
    }
    
    # If no matching entries found, return error
    if (length(matching_entries) == 0) {
      param_str =  paste(names(params), params, sep = "=", collapse = ", ")
      stop(paste("No emission factor found for", animal_type, "with parameters:", param_str))
    }
    
    # Find the exact match if available
    for (entry in matching_entries) {
      if (entry$baseline_age == input_age) {  
        return(entry$ef) 
      }
    }
    
    # If no exact match, scale linearly based on age
    baseline_entry =  matching_entries[[1]]
    baseline_age =  baseline_entry$baseline_age
    baseline_ef =  baseline_entry$ef
    
    # Scale the emission factor linearly based on age
    scaled_ef =  baseline_ef * (input_age / baseline_age)
    
    return(scaled_ef)
  }
  
  # If we get here, no match was found
  param_str =  paste(names(params), params, sep = "=", collapse = ", ")
  stop(paste("No emission factor found for", animal_type, "with parameters:", param_str))
}

#' Calculate Enteric Fermentation Emission Factor for Tier 2 Animals
#'
#' This function calculates the implied emission factor for enteric fermentation
#' based on animal type and specific parameters for tier 2 animals.
#' It reads the parameters directly from the mms_CH4.yaml configuration file.
#'
#' @param animal_type Character string specifying the animal type (e.g., "dairy_cattle", "calf", "bull")
#' @param ... Additional parameters specific to the animal type:
#'   - weight: Numeric value representing the animal weight (if applicable)
#'   - age: Character or numeric value representing the animal age (if applicable)
#'   - reproduction: Logical value indicating if the animal is used for reproduction (if applicable)
#'   - gender: Character value ("male" or "female") required for turkeys
#'   - scale_by_weight: Logical value indicating whether to scale the emission factor by weight (default: FALSE)
#'   - scale_by_age: Logical value indicating whether to scale the emission factor by age (default: FALSE)
#'
#' @return Numeric value representing the implied emission factor in kg CH4/head
#' @export
#'
#' @examples
#' # Get emission factor for dairy cattle
#' .tier2_EF("dairy_cattle")
#'
#' # Get emission factor for calf with age="0-6 months" and reproduction=FALSE
#' .tier2_EF("calf", age="0-6 months", reproduction=FALSE)
#'
#' # Get emission factor for bull with age="6 months" and reproduction=FALSE
#' .tier2_EF("bull_", age="6 months", reproduction=FALSE)
#'
#' # Get emission factor for turkeys with gender="male"
#' .tier2_EF("turkeys", gender="male")
#'
#' # Get emission factor for suckling_cattle with weight=500 (scaled from reference weight 600)
#' .tier2_EF("suckling_cattle", weight=500, scale_by_weight=TRUE)
#'
#' # Get emission factor for bull with age=4 (scaled from reference age 6 months)
#' .tier2_EF("bull_", age=4, scale_by_age=TRUE)
.tier2_EF =  function(animal_type, ...) {
  # Get additional parameters
  params =  list(...)
  
  # Check required parameters for specific animal types
  if (animal_type %in% c("turkeys", "turkeys_")) {
    if (!"gender" %in% names(params)) { 
      stop("Parameter 'gender' is required for animal type 'turkeys'") 
    }
  }
  
  # Define tier 2 animal types
  tier2_animals =  c("dairy_cattle", "calf", "calf_", "suckling_cattle", "bull", "bull_", 
                     "sheep", "fattening_pigs", "sows", "piglets", "deer", "goats", 
                     "horses", "turkeys", "turkeys_")
  
  # Check if animal type is a tier 2 animal
  if (!animal_type %in% tier2_animals) {
    stop(paste(animal_type, "is not a tier 2 animal type"))
  }
  
  # Get the YAML file path
  if (exists(".get_yaml_file", mode = "function")) {
    yaml_file =  .get_yaml_file()
  } else {
    yaml_file =  system.file("extdata", "mms_CH4.yaml", package = "rEMEP")
    if (yaml_file == "") {
      # If running outside the package context, try different relative paths
      possible_paths =  c(
        file.path("inst", "extdata", "mms_CH4.yaml"),
        file.path("../../inst", "extdata", "mms_CH4.yaml"),
        file.path("../inst", "extdata", "mms_CH4.yaml")
      )
      
      for (path in possible_paths) {
        if (file.exists(path)) {
          yaml_file =  path
          break
        }
      }
      
      if (!exists("yaml_file") || yaml_file == "") {
        stop("Could not find mms_CH4.yaml file")
      }
    }
  }
  
  # Read the YAML file
  yaml_content =  yaml::read_yaml(yaml_file)
  
  # Check if the YAML content was loaded correctly
  if (!is.list(yaml_content)) {
    stop("Failed to parse the YAML file correctly")
  }
  
  # Check if enteric_fermentation_EFs exists in the YAML file
  if (!"enteric_fermentation_EFs" %in% names(yaml_content)) {
    stop("enteric_fermentation_EFs section not found in the YAML file")
  }
  
  # Get the enteric fermentation data
  ef_data =  yaml_content$enteric_fermentation_EFs
  
  # Check if animal type exists in the data
  if (!animal_type %in% names(ef_data)) {
    stop(paste(animal_type, "not found in emission factor data"))
  }
  
  # Get the animal data
  animal_data =  ef_data[[animal_type]]
  
  # Check if the animal data has an emission factor
  if (!"implified_emission_factor" %in% names(animal_data)) {
    stop(paste("No emission factor found for", animal_type))
  }
  
  # For animals with multiple entries, we need to match the parameters
  if (animal_type %in% c("calf", "calf_", "bull", "bull_", "turkeys", "turkeys_")) {
    # Check if scaling is requested
    scaling_requested = ("scale_by_weight" %in% names(params) && params$scale_by_weight) ||
      ("scale_by_age" %in% names(params) && params$scale_by_age)
    
    # If not scaling, check if all required parameters match
    if (!scaling_requested) {
      for (param_name in names(params)) {
        # Skip scaling parameters
        if (param_name %in% c("scale_by_weight", "scale_by_age")) {
          next
        }
        
        if (param_name %in% names(animal_data)) {
          # For parameters that are stored as character in YAML
          if (is.character(animal_data[[param_name]])) {
            # Handle the case where YAML has 'none' but R has NULL
            if (animal_data[[param_name]] == "none" && is.null(params[[param_name]])) {
              next
            }
            # Handle normal string comparison
            if (animal_data[[param_name]] != params[[param_name]]) {
              param_str =  paste(names(params), params, sep = "=", collapse = ", ")
              stop(paste("Parameter mismatch for", animal_type, ":", param_name, 
                         "Expected", animal_data[[param_name]], "but got", params[[param_name]]))
            }
          }
          # For parameters that are stored as logical in YAML
          else if (is.logical(animal_data[[param_name]])) {
            if (animal_data[[param_name]] != params[[param_name]]) {
              param_str =  paste(names(params), params, sep = "=", collapse = ", ")
              stop(paste("Parameter mismatch for", animal_type, ":", param_name, 
                         "Expected", animal_data[[param_name]], "but got", params[[param_name]]))
            }
          }
        }
      }
    }
  }
  
  # Initialize variables for scaling
  original_feed_intake = as.numeric(animal_data$feed_intake)
  scaled_feed_intake = original_feed_intake
  
  # Check if we need to scale by weight
  if ("scale_by_weight" %in% names(params) && params$scale_by_weight && "weight" %in% names(params)) {
    # Check if the animal has a feed_intake and weight in the YAML data
    if ("feed_intake" %in% names(animal_data) && "weight" %in% names(animal_data)) {
      # Get the reference weight from the YAML file
      reference_weight = animal_data$weight
      
      # Only apply scaling if the reference weight is not "none"
      if (!is.character(reference_weight) || (reference_weight != "none" && reference_weight != "null")) {
        # Get the new weight from the parameters
        new_weight = params$weight
        
        # Scale the feed intake based on weight
        scaled_feed_intake = scale_feed_intake(original_feed_intake, reference_weight, new_weight)
      }
    }
  }
  
  # Check if we need to scale by age
  if ("scale_by_age" %in% names(params) && params$scale_by_age && "age" %in% names(params)) {
    # Check if the animal has a feed_intake and age in the YAML data
    if ("feed_intake" %in% names(animal_data) && "age" %in% names(animal_data)) {
      # Get the reference age from the YAML file
      reference_age = animal_data$age
      
      # Only apply scaling if the reference age is not "none"
      if (!is.character(reference_age) || (reference_age != "none" && reference_age != "null")) {
        # Get the new age from the parameters
        new_age = params$age
        
        # If we've already scaled by weight, use the scaled feed intake as the base
        # Otherwise use the original feed intake
        feed_intake_for_age_scaling = scaled_feed_intake
        
        # Scale the feed intake based on age
        scaled_feed_intake = scale_feed_intake_by_age(feed_intake_for_age_scaling, reference_age, new_age)
      }
    }
  }
  
  # If any scaling was applied, calculate the scaling factor and apply it to the emission factor
  if (scaled_feed_intake != original_feed_intake) {
    # Calculate the scaling factor for the emission factor
    scaling_factor = scaled_feed_intake / original_feed_intake
    
    # Scale the emission factor
    return(as.numeric(animal_data$implified_emission_factor) * scaling_factor)
  }
  
  # If no scaling is needed or possible, return the original emission factor
  return(as.numeric(animal_data$implified_emission_factor))
}
