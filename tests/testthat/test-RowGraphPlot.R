test_that("RowGraphPlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("GlobalPatterns", package = "mia")
  tse <- GlobalPatterns
  
  data("row_graph_order", package = "miaViz")
  
  tse  <- mia::agglomerateByRank(tse, rank = "Order", na.rm = TRUE)
  metadata(tse)$graph <- row_graph_order
  
  panel <- RowGraphPlot()
  
  panel[["layout"]] <- "kk"
  
  expect_identical(.getEncodedName(panel), "RowGraphPlotNA")
  expect_identical(.fullName(panel), "Row graph plot")
  expect_identical(.panelColor(panel), "orange")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 3)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
      'p <- miaViz::plotRowGraph(se, name=\"graph\", assay.type=\"counts\",\n    show.label=FALSE, layout=\"kk\", edge.type=\"fan\", add.legend=TRUE)',
      fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel), c("name", "assay.type", "layout",
      "show.label", "add.legend", "edge.colour.by", "edge.size.by",
      "node.colour.by", "node.shape.by", "node.size.by"))
  
  expect_contains(.definePanelTour(panel)[[1]],
      c("#RowGraphPlotNA_DataBoxOpen", "#RowGraphPlotNA_VisualBoxOpen",
      "#RowGraphPlotNA", "#RowGraphPlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_graph(panel, tse), "shiny.tag.list")
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$RowGraphPlotNA, "shiny.render.function")
  expect_s3_class(output$RowGraphPlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$RowGraphPlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
  expect_identical(.exportOutput(panel, tse), "RowGraphPlotNA.pdf")
  
})
