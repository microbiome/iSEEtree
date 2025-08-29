test_that("ScreePlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  panel <- ScreePlot()
  
  panel[["show.barplot"]] <- FALSE
  panel[["show.line"]] <- FALSE

  tse <- scater::runPCA(tse, assay.type = "counts", ncomponents = 5)
  
  expect_identical(.getEncodedName(panel), "ScreePlotNA")
  expect_identical(.fullName(panel), "Scree plot")
  expect_identical(.panelColor(panel), "#0066CC")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 5)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
  "p <- miaViz::plotScree(se, dimred=\"PCA\", show.barplot=FALSE, show.points=TRUE,\n    show.line=FALSE, add.proportion=TRUE, add.cumulative=FALSE,\n    show.names=FALSE, show.labels=FALSE, n=5)",
  fixed = TRUE)
  
  # expect_true(.hideInterface(panel, "ColumnSelectionSource"))
  # expect_false(.multiSelectionResponsive(panel, "column"))
  # expect_true(.multiSelectionResponsive(panel, "row"))
  
  expect_contains(slotNames(panel), c("dimred", "show.barplot", "show.points",
      "show.line", "show.labels", "add.proportion", "add.cumulative", "n",
      "show.names", "eig.name"))
  
  expect_contains(.definePanelTour(panel)[[1]],
      c("#ScreePlotNA_DataBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_scree_plot(panel, tse), "shiny.tag.list")
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$ScreePlotNA, "shiny.render.function")
  # expect_s3_class(output$ScreePlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  # expect_s3_class(output$ScreePlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
  expect_identical(.exportOutput(panel, tse), "ScreePlotNA.pdf")
  
})