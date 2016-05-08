context("ggheat")

test_that("ggheat creates a GGHeat class with pms data",{
  data(pms)
  gh <- ggheat(as.data.frame(pms))  
  expect_is(gh,"GGHeat")
})

test_that("ggheat creates a GGHeat class with squid data",{
  data(squid)
  gh <- ggheat(squid,id.vars = colnames(squid[,1:3]))  
  expect_is(gh,"GGHeat")
})