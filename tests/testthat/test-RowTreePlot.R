test_that("RowTreePlot", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- RowTreePlot()
  
  expect_identical(.getEncodedName(panel), "RowTreePlotNA")
  expect_identical(.fullName(panel), "Row tree plot")
  expect_identical(.panelColor(panel), "#4EEE94")
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")  
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel),
                  c("layout", "add_legend", "edge_colour", "edge_colour_by",
                    "tip_colour", "tip_colour_by"))
  
  expect_contains(.definePanelTour(panel)[[1]],
                  c("#RowTreePlotNA_DataBoxOpen", "#RowTreePlotNA_VisualBoxOpen",
                    "#RowTreePlotNA", "#RowTreePlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_rowtree(panel, tse), "shiny.tag.list")
  
})