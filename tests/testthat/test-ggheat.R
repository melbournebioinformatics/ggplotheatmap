context("class_definitions")

test_that("ggheat creates a GGHeat class",{
  data(pms)
  gh <- ggheat(pms)  
  expect_is(gh,"GGHeat")
})
