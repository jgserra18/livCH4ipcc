\name{Inventory}
\alias{Inventory}
\title{CH4 Inventory Class}
\description{
  Class for calculating CH4 emissions from livestock based on user input parameters.
  
  The Inventory class handles the entire calculation workflow:
  \itemize{
    \item Enteric fermentation (Tier 1 and Tier 2)
    \item Manure management
    \item Seasonal variations (winter/summer)
  }
}
\usage{
Inventory$new(user_input, debug_mode = FALSE)
}
\arguments{
  \item{user_input}{User_input object containing input parameters}
  \item{debug_mode}{Enable debug mode for more verbose output}
}
\value{
  An R6 class object with methods for calculating CH4 emissions.
}
\examples{
\dontrun{
# Create a User_input object
input = User_Input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  weight = 650,
  manure_excretion = 20000,
  manure_cn = 15,
  fraction_housing = 0.7,
  fraction_grazing = 0.3,
  feed_intake = 20,
  fraction_diet_grass = 0.6,
  fraction_diet_beet = 0.1
)

# Create Inventory object and run calculations
inv = Inventory$new(input)
results = inv$run_inventory()
}
}
\seealso{
  \code{\link{User_Input}}
}
