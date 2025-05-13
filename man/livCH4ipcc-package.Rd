\name{livCH4ipcc-package}
\alias{livCH4ipcc-package}
\alias{livCH4ipcc}
\docType{package}
\title{
  Methane Emissions Calculator for Livestock
}
\description{
  A comprehensive tool for calculating methane (CH4) emissions from livestock
  based on IPCC methodologies. Supports both Tier 1 and Tier 2 approaches for
  enteric fermentation and manure management, with seasonal variations and
  animal-specific parameters.
}
\details{
  The package provides classes and functions for calculating CH4 emissions from livestock.
  It supports both Tier 1 and Tier 2 methodologies according to IPCC guidelines.
  
  Main classes:
  \itemize{
    \item \code{User_Input}: Class for defining animal characteristics and management practices
    \item \code{Inventory}: Class for performing emission calculations
  }
  
  The package uses YAML configuration files stored in the inst/extdata directory.
}
\author{
  João Serra \email{joao.serra@example.com}
}
\keyword{ package }
