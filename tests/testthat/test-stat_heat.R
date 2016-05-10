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



test_that("stat_heat allows grouping",{
  data(squid)
  sqg <- cbind(grp = rep(c(1,2),each=50),squid)
  
  gh <- ggheat(sqg, id.vars = colnames(sqg[,1:4]))
  
  ghs <- gh + stat_heat(aes(fill=value,group=grp))
  ld <- layer_data(ghs@main_plot)
  
  expect_equal(dim(ld),c(500,13))
  
})
