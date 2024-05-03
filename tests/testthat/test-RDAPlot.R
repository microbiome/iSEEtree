test_that("RDAPlot", {
  
  panel <- RDAPlot()

  expect_identical(.fullName(panel), "RDA plot")
  expect_identical(.panelColor(panel), "#CD5B45")
  
  expect_false(.multiSelectionResponsive(panel, "row"))
  expect_true(.multiSelectionResponsive(panel, "column"))
  
  expect_contains(slotNames(panel),
                  c("dimred", "add.ellipse", "colour_by",
                    "vec.text", "add.vectors"))
  
})