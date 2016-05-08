context("stat_heat")

test_that("stat_heat adds tall pms layer data",{
  data(pms)
  gh <- ggheat(as.data.frame(pms))  

  ghs <- gh + stat_heat(aes(fill=value))
  ld <- layer_data(ghs@main_plot)

  expect_equal(dim(ld),c(240,13))
      
})


test_that("stat_heat adds tall squid layer data",{
  data(squid)
  gh <- ggheat(squid, id.vars = colnames(squid[,1:3]))
  
  ghs <- gh + stat_heat(aes(fill=value))
  ld <- layer_data(ghs@main_plot)
  
  expect_equal(dim(ld),c(500,13))
  
})
