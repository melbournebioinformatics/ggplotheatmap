context("class_definitions")

test_that("ggheat creates a GGHeat class",{
  data(pms)
  gh <- ggheat(as.data.frame(pms))  
  expect_is(gh,"GGHeat")
})
