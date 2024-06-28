test_that("RowTreePlot", {
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- RowTreePlot()
  
  panel[["layout"]] <- "rectangular"
  
  expect_identical(.getEncodedName(panel), "RowTreePlotNA")
  expect_identical(.fullName(panel), "Row tree plot")
  expect_identical(.panelColor(panel), "#4EEE94")
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
      'p <- miaViz::plotRowTree(se, layout="rectangular", add_legend=TRUE,
    order_tree=FALSE)',
      fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel),
                  c("layout", "add_legend", "edge_colour_by",
                    "tip_colour_by", "order_tree", "tip_size_by",
                    "tip_shape_by", "edge_size_by", "node_size_by",
                    "node_shape_by", "node_colour_by"))
  
  expect_contains(.definePanelTour(panel)[[1]],
                  c("#RowTreePlotNA_DataBoxOpen", "#RowTreePlotNA_VisualBoxOpen",
                    "#RowTreePlotNA", "#RowTreePlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_rowtree(panel, tse), "shiny.tag.list")
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$RowTreePlotNA, "shiny.render.function")
  expect_s3_class(output$RowTreePlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$RowTreePlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
})