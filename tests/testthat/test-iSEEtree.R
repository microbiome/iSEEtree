test_that("iSEEtree", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  
  panels <- c(RowDataTable(), ColumnDataTable(), RowTreePlot(),
              AbundancePlot(), AbundanceDensityPlot(),
              ReducedDimensionPlot(), ComplexHeatmapPlot())

  expect_warning(
    panels <- .check_panel(tse, panels, "ReducedDimensionPlot", reducedDims),
    "no valid reducedDims fields for ReducedDimensionPlot"
  )
  
  expect_no_error(iSEE(tse, initial = panels))
  
})
