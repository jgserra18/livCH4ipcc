\name{User_Input}
\alias{User_Input}
\title{User Input Class for Livestock CH4 Emissions}
\description{
  Class for validating user input parameters for livestock CH4 emission calculations
  with a focus on feeding and housing components.
  
  The User_Input class handles validation of all parameters needed for the CH4 emission
  calculations from livestock, particularly focusing on feeding and housing components.
  It ensures that input parameters are valid before they are used in calculations.
}
\details{
  This class validates the following parameters:
  \itemize{
    \item Animal type and number
    \item Animal characteristics (weight, age, reproduction, gender)
    \item Manure excretion and C:N ratio
    \item Housing and grazing fractions
    \item Feed intake and diet composition
    \item Volatile solids parameters (fraction_ash, fraction_dm_VS)
  }
}
\usage{
User_Input$new(animal_type, animal_number, weight = NULL, age = NULL, 
               reproduction = NULL, gender = NULL, manure_excretion = NULL, 
               manure_cn = NULL, fraction_housing = NULL, fraction_grazing = NULL, 
               fraction_actual_grass = NULL, feed_intake = NULL, 
               fraction_diet_grass = NULL, fraction_diet_beet = NULL, 
               max_manure_CH4 = NULL, straw_amount = NULL, fraction_ash = NULL)
}
\arguments{
  \item{animal_type}{Character string representing the animal type (e.g., "dairy_cattle", "fattening_pigs")}
  \item{animal_number}{Number of animals (head/yr)}
  \item{weight}{Average weight of animals (kg)}
  \item{age}{Age of animals (years)}
  \item{reproduction}{Boolean indicating if animals are used for reproduction}
  \item{gender}{Gender of animals ("male" or "female")}
  \item{manure_excretion}{Amount of manure excreted (kg/(head.yr))}
  \item{manure_cn}{C:N ratio of manure}
  \item{fraction_housing}{Fraction of time animals spend in housing (0-1)}
  \item{fraction_grazing}{Fraction of time animals spend grazing (0-1)}
  \item{fraction_actual_grass}{Fraction of actual days on grass considering climate (0-1)}
  \item{feed_intake}{Feed intake (kg DM/(head.yr))}
  \item{fraction_diet_grass}{Fraction of grass in diet (0-1)}
  \item{fraction_diet_beet}{Fraction of beet in diet (0-1)}
  \item{max_manure_CH4}{Maximum CH4 potential of manure (m3 CH4/kg VS)}
  \item{straw_amount}{Amount of straw used for bedding (kg/(head.yr))}
  \item{fraction_ash}{Ash content in manure or straw (0-1)}
}
\value{
  An R6 class object with methods for validating input parameters.
}
\examples{
\dontrun{
# Create a new User_Input object for dairy cattle
input = User_Input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  manure_excretion = 20000,
  manure_cn = 15,
  fraction_housing = 0.7,
  fraction_grazing = 0.3,
  fraction_diet_grass = 0.6,
  fraction_diet_beet = 0.1
)

# Check if the input is valid
if (input$valid) {
  # Get the parameters as a list for use in calculations
  params = input$get_parameters()
  # Use the parameters in your calculations
} else {
  # Print validation errors
  cat(input$get_validation_message_string())
}
}
}
\seealso{
  \code{\link{Inventory}}
}
