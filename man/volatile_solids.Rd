\name{volatile_solids}
\alias{housing_volatile_solids}
\alias{manure_volatile_solids}
\alias{straw_volatile_solids}
\alias{grass_volatile_solids}
\title{Volatile Solids Calculation Functions}
\description{
  Functions for calculating volatile solids from different sources in livestock systems.
}
\usage{
housing_volatile_solids(VS_manure, VS_straw)

manure_volatile_solids(m_manure_excreted, dm_manure, f_dm_VS = 0.8, f_ash = 0.2)

straw_volatile_solids(m_straw, dm_straw, fraction_ash = 0.045, fraction_actual_grass)

grass_volatile_solids(m_manure_excreted, dm_manure, fraction_dm_VS = 0.8, fraction_grass)
}
\arguments{
  \item{VS_manure}{Volatile solids excreted from manure (kg/(head.yr))}
  \item{VS_straw}{Volatile solids excreted from straw (kg/(head.yr))}
  \item{m_manure_excreted}{Amount of manure excreted (kg/(head.yr))}
  \item{dm_manure}{Dry matter fraction of manure (0-1)}
  \item{f_dm_VS}{Share of volatile solids of dry matter (default: 0.8 or 80\%)}
  \item{f_ash}{Ash content in manure (0-1, default: 0.2 or 20\%)}
  \item{m_straw}{Amount of straw (kg/(head.yr))}
  \item{dm_straw}{Dry matter fraction of straw (0-1)}
  \item{fraction_ash}{Ash content in straw (0-1, default: 0.045 or 4.5\%)}
  \item{fraction_actual_grass}{Fraction of actual days on grass (0-1)}
  \item{fraction_dm_VS}{Share of volatile solids of dry matter (default: 0.8 or 80\%)}
  \item{fraction_grass}{Share of feeding days on grass (0-1)}
}
\value{
  Numeric value representing volatile solids (kg/(head.yr))
}
\examples{
# Calculate volatile solids from manure
manure_VS <- manure_volatile_solids(m_manure_excreted = 20000, dm_manure = 0.15)

# Calculate volatile solids from straw
straw_VS <- straw_volatile_solids(m_straw = 500, dm_straw = 0.85)

# Calculate volatile solids excreted on grass
grass_VS <- grass_volatile_solids(m_manure_excreted = 20000, 
                                 dm_manure = 0.15, 
                                 fraction_grass = 0.3)

# Calculate total volatile solids in housing
housing_VS <- housing_volatile_solids(VS_manure = 1000, VS_straw = 200)
}
