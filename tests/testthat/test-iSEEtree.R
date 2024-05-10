test_that("iSEEtree", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  
  panels <- c(RowTreePlot(), AbundancePlot(), AbundanceDensityPlot(),
              ReducedDimensionPlot(), ComplexHeatmapPlot())
  
  expect_true(any(.is_class_present(panels, "RowTreePlot")))
  expect_false(any(.is_class_present(panels, "undefined_class")))
  
  expect_length(.check_panel(tse, panels, "ReducedDimensionPlot", reducedDims), 4)
  
  expect_no_error(iSEE(tse))
  
})
