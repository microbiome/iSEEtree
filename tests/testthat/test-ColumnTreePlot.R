test_that("ColumnTreePlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  
  tse <- TreeSummarizedExperiment(
      assays = list(counts = t(assay(tse, "counts"))),
      colData = rowData(tse), rowData = colData(tse),
      colTree = rowTree(tse))
  
  panel <- ColumnTreePlot()
  
  panel[["layout"]] <- "rectangular"
  
  expect_identical(.getEncodedName(panel), "ColumnTreePlotNA")
  expect_identical(.fullName(panel), "Column tree plot")
  expect_identical(.panelColor(panel), "steelblue")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 2)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  # expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
  #              'p <- miaViz::plotColTree(se, layout="rectangular", add.legend=TRUE,
  #   order.tree=FALSE)',
  #              fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "row"))
  expect_true(.multiSelectionResponsive(panel, "column"))
  
  expect_contains(slotNames(panel), c("layout", "add.legend", "edge.colour.by",
      "tip.colour.by", "order.tree", "tip.size.by", "tip.shape.by",
      "edge.size.by", "node.size.by", "node.shape.by", "node.colour.by",
      "add.node.lab", "add.tip.lab", "branch.length", "open.angle",
      "rotate.angle"))
  
  expect_contains(.definePanelTour(panel)[[1]],
      c("#ColumnTreePlotNA_DataBoxOpen", "#ColumnTreePlotNA_VisualBoxOpen",
      "#ColumnTreePlotNA", "#ColumnTreePlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_tree(panel, tse), "shiny.tag.list")
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$ColumnTreePlotNA, "shiny.render.function")
  expect_s3_class(output$ColumnTreePlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$ColumnTreePlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
  # expect_identical(.exportOutput(panel, tse), "ColumnTreePlotNA.pdf")
  
})
