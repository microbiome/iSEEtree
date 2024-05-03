test_that("RowTreePlot", {
  
  panel <- RowTreePlot()
  
  expect_identical(.fullName(panel), "Row tree plot")
  expect_identical(.panelColor(panel), "#4EEE94")
  
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel),
                  c("layout", "add_legend", "edge_colour", "edge_colour_by",
                    "tip_colour", "tip_colour_by"))
  
})