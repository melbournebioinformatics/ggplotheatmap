context("heatmap_components")

test_that("returns a list with expected components",{
  data(pms)
  cmps <- heatmap_components(pms)
  expect_is(cmps,"list")
  expect_equal(length(cmps),4)
  expect_equal(names(cmps),c("tile","rowd","cold","data"))
  expect_equal(names(cmps$data),c("tile","row.ord","col.ord"))
})

test_that("stripdata is retained in returned tile data",{
  data(squid)
  cmps <- heatmap_components(squid[,4:8],stripdata = squid[,1:2])
  expect_equal(length(setdiff(colnames(cmps$data$tile),c("rlabels","iBAQ","prot_id","clabels","Intensity"))),0)
})

test_that("stripdata accepts a single column vector",{
  data(squid)
  cmps <- heatmap_components(squid[,4:8],stripdata = squid[,1])
  expect_equal(length(setdiff(colnames(cmps$data$tile),c("rlabels","stripdata","clabels","Intensity"))),0)
})

test_that("stripdata accepts a single column df and keeps its name",{
  data(squid)
  sd = data.frame(iBAQ=squid[,1])
  cmps <- heatmap_components(squid[,4:8],stripdata = sd)
  expect_equal(length(setdiff(colnames(cmps$data$tile),c("rlabels","iBAQ","clabels","Intensity"))),0)
})