test_that("LoadingPlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- LoadingPlot()
  
  panel[["layout"]] <- "heatmap"

  tse <- scater::runPCA(tse, assay.type = "counts", ncomponents = 5)
  
  expect_identical(.getEncodedName(panel), "LoadingPlotNA")
  expect_identical(.fullName(panel), "Loading plot")
  expect_identical(.panelColor(panel), "yellow")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 2)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
  'p <- miaViz::plotLoadings(se, dimred="PCA", layout="heatmap", ',
  'add.tree=FALSE,\n    ncomponents=5)',
  fixed = TRUE)
  
  expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "column"))
  expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel), c("dimred", "layout", "ncomponents", "add.tree"))
  
  expect_contains(.definePanelTour(panel)[[1]],
      c("#LoadingPlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_loading_plot(panel, tse), "shiny.tag.list")
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$LoadingPlotNA, "shiny.render.function")
  expect_s3_class(output$LoadingPlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$LoadingPlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
  expect_identical(.exportOutput(panel, tse), "LoadingPlotNA.pdf")
  
})