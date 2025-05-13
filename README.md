# livCH4ipcc: Methane Emissions Calculator for Livestock

## Overview

livCH4ipcc is an R package for calculating methane (CH4) emissions from livestock based on IPCC methodologies. It supports both Tier 1 and Tier 2 approaches for enteric fermentation and manure management, with seasonal variations and animal-specific parameters.

## Features

- Calculation of enteric fermentation emissions using Tier 1 and Tier 2 methodologies
- Manure management emissions calculations
- Seasonal variations (winter/summer) for emission factors
- Support for various livestock types (dairy cattle, beef cattle, pigs, etc.)
- Customizable parameters for animal characteristics

## Installation

```r
# Install from GitHub
devtools::install_github("joaoserra/livCH4ipcc")
```

## Usage

```r
library(livCH4ipcc)

# Create a User_input object
input <- User_Input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  weight = 650,
  manure_excretion = 21900,
  manure_cn = 15,
  fraction_housing = 0.8,
  fraction_grazing = 0.2,
  feed_intake = 7300,
  fraction_diet_grass = 0.3,
  fraction_diet_beet = 0.05
)

# Create Inventory object and run calculations
inv <- Inventory$new(input)
results <- inv$run_inventory()

# Access results
print(results$emissions$ch4_total)
```

## Documentation

For detailed documentation, see the package vignettes:

```r
vignette("introduction", package = "livCH4ipcc")
vignette("advanced-usage", package = "livCH4ipcc")
```

## License

This package is licensed under the MIT License.
