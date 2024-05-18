test_that("iSEEtree", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  
  panels <- c(RowDataTable(), ColumnDataTable(), RowTreePlot(),
              AbundancePlot(), AbundanceDensityPlot(),
              ReducedDimensionPlot(), ComplexHeatmapPlot())

  #expect_length(.check_panel(tse, panels, "ReducedDimensionPlot", reducedDims), 6)
  
  expect_no_error(iSEE(tse))
  
})
