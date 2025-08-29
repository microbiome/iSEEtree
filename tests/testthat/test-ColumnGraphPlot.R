test_that("ColumnGraphPlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("GlobalPatterns", package = "mia")
  tse <- GlobalPatterns
  
  data("col_graph", package = "miaViz")
  metadata(tse)$graph <- col_graph
  
  panel <- ColumnGraphPlot()
  
  panel[["layout"]] <- "kk"
  
  expect_identical(.getEncodedName(panel), "ColumnGraphPlotNA")
  expect_identical(.fullName(panel), "Column graph plot")
  expect_identical(.panelColor(panel), "purple")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 3)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
      'p <- miaViz::plotColGraph(se, name=\"graph\", assay.type=\"counts\",\n    show.label=FALSE, layout=\"kk\", edge.type=\"fan\", add.legend=TRUE)',
      fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "row"))
  expect_true(.multiSelectionResponsive(panel, "column"))
  
  expect_contains(slotNames(panel), c("name", "assay.type", "layout",
      "show.label", "add.legend", "edge.colour.by", "edge.size.by",
      "node.colour.by", "node.shape.by", "node.size.by"))
  
  expect_contains(.definePanelTour(panel)[[1]],
      c("#ColumnGraphPlotNA_DataBoxOpen", "#ColumnGraphPlotNA_VisualBoxOpen",
      "#ColumnGraphPlotNA", "#ColumnGraphPlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_graph(panel, tse), "shiny.tag.list")
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$ColumnGraphPlotNA, "shiny.render.function")
  expect_s3_class(output$ColumnGraphPlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$ColumnGraphPlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
  expect_identical(.exportOutput(panel, tse), "ColumnGraphPlotNA.pdf")
  
})
