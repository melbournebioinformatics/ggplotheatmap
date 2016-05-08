context("wide_tall")

test_that("converting pms from wide to tall works",{
  data(pms)
  pms_w <- wide_to_tall(as.data.frame(pms))

  expect_equal(dim(pms_w)[2],3)
})

test_that("converting pms from wide to tall and back works",{
  data(pms)
  pms_w <- wide_to_tall(as.data.frame(pms))
  pms_tw <- tall_to_wide(pms_w)
  
  expect_equal(pms_tw,as.data.frame(pms))
})


test_that("converting squid from wide to tall works",{
  data(squid)
  squid_idcols <- colnames(squid[,1:3])
  squid_w <- wide_to_tall(squid,id.vars = squid_idcols)
  expect_equal(dim(squid_w)[2],6)
})


test_that("converting squid from wide to tall and back works",{
  data(squid)
  squid_idcols <- colnames(squid[,1:3])
  squid_w <- wide_to_tall(squid,id.vars = squid_idcols)
  squid_tw <- tall_to_wide(squid_w)
    
  expect_equal(squid_tw,squid)
})
