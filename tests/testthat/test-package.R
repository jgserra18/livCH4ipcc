test_that("Package loads correctly", {
  # Test that the package can be loaded
  expect_true("livCH4ipcc" %in% rownames(installed.packages()) || 
              file.exists(system.file(package = "livCH4ipcc")))
})

test_that("Configuration files exist", {
  # Test that the configuration files exist
  expect_true(file.exists(system.file("extdata/enteric_fermentation.yaml", package = "livCH4ipcc")))
  expect_true(file.exists(system.file("extdata/global_parameters.yaml", package = "livCH4ipcc")))
  expect_true(file.exists(system.file("extdata/manure_management.yaml", package = "livCH4ipcc")))
  expect_true(file.exists(system.file("extdata/straw_litter.yaml", package = "livCH4ipcc")))
})

test_that("Volatile solids functions work correctly", {
  # Test housing_volatile_solids
  expect_equal(housing_volatile_solids(VS_manure = 1000, VS_straw = 200), 1200)
  
  # Test manure_volatile_solids with default parameters
  expect_equal(
    manure_volatile_solids(m_manure_excreted = 10000, dm_manure = 0.2), 
    10000 * 0.2 * 0.8 * (1-0.2)
  )
  
  # Test straw_volatile_solids with default parameters
  expect_equal(
    straw_volatile_solids(m_straw = 500, dm_straw = 0.85, fraction_actual_grass = 0.2), 
    500 * 0.85 * (1-0.045) * (1-0.2)
  )
  
  # Test grass_volatile_solids with default parameters
  expect_equal(
    grass_volatile_solids(m_manure_excreted = 10000, dm_manure = 0.2, fraction_grass = 0.3), 
    10000 * 0.2 * 0.8 * 0.3
  )
})
