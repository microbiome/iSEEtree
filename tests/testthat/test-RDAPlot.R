test_that("RDAPlot", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- RDAPlot()

  expect_identical(.getEncodedName(panel), "RDAPlotNA")
  expect_identical(.fullName(panel), "RDA plot")
  expect_identical(.panelColor(panel), "#CD5B45")
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")  
  
  expect_true(.hideInterface(panel, "RowSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "row"))
  expect_true(.multiSelectionResponsive(panel, "column"))
  
  expect_contains(slotNames(panel),
                  c("dimred", "add.ellipse", "colour_by",
                    "vec.text", "add.vectors"))
  
  expect_contains(.definePanelTour(panel)[[1]],
                  c("#RDAPlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_rda(panel, tse), "shiny.tag.list")

})
