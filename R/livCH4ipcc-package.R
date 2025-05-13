#' @title livCH4ipcc: Methane Emissions Calculator for Livestock
#' 
#' @description 
#' A comprehensive tool for calculating methane (CH4) emissions from livestock
#' based on IPCC methodologies. Supports both Tier 1 and Tier 2 approaches for
#' enteric fermentation and manure management, with seasonal variations and
#' animal-specific parameters.
#'
#' @section Main Classes:
#' \itemize{
#'   \item \code{\link{User_Input}}: Class for defining animal characteristics and management practices
#'   \item \code{\link{Inventory}}: Class for performing emission calculations
#' }
#'
#' @section Configuration:
#' The package uses YAML configuration files stored in the inst/extdata directory:
#' \itemize{
#'   \item enteric_fermentation.yaml: Contains emission factors for enteric fermentation
#'   \item global_parameters.yaml: Contains global parameters used across calculations
#'   \item manure_management.yaml: Contains emission factors for manure management
#' }
#'
#' @docType package
#' @name livCH4ipcc-package
#' @aliases livCH4ipcc
#' @author João Serra \email{joao.serra@example.com}
#' @keywords package
NULL
