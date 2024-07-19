test_that("RDAPlot", {
  
  output <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()
  select_info <- list(single = list(feature = "---", sample = "---"),
                      multi = list(row = "---", column = "---"))
  
  data("enterotype", package = "mia")
  tse <- enterotype
  panel <- RDAPlot()
  
  tse <- mia::runRDA(tse,
      formula = assay ~ ClinicalStatus + Gender + Age,
      na.action = na.exclude)
  
  panel[["colour_by"]] <- "ClinicalStatus"

  expect_identical(.getEncodedName(panel), "RDAPlotNA")
  expect_identical(.fullName(panel), "RDA plot")
  expect_identical(.panelColor(panel), "#CD5B45")
  
  expect_s3_class(.defineInterface(panel, tse, select_info)[[1]][[1]], "shiny.tag.list")
  expect_length(.defineDataInterface(panel, tse, select_info), 4)
  
  expect_s3_class(.defineOutput(panel), "shiny.tag.list")
  expect_match(.generateOutput(panel, tse)[["commands"]][["fun"]],
  'p <- miaViz::plotRDA(se, dimred="RDA", colour_by="ClinicalStatus",\n    ',
  'confidence.level=0.95, add.expl.var=TRUE, add.significance=TRUE)',
  fixed = TRUE)
  
  expect_true(.hideInterface(panel, "RowSelectionSource"))
  expect_false(.multiSelectionResponsive(panel, "row"))
  expect_true(.multiSelectionResponsive(panel, "column"))
  
  expect_contains(slotNames(panel), c("dimred", "add.ellipse", "colour_by",
      "vec.text", "add.vectors", "ellipse.alpha", "confidence.level",
      "add.significance", "add.expl.var", "vec.size", "vec.colour",
      "vec.linetype", "arrow.size", "ellipse.linewidth",
      "ellipse.linetype", "label.colour", "label.size"))
  
  expect_contains(.definePanelTour(panel)[[1]],
      c("#RDAPlotNA_SelectionBoxOpen"))
  
  expect_s3_class(.create_visual_box_for_rda(panel, tse), "shiny.tag.list")
  
  expect_null(.renderOutput(panel, tse, output = output, pObjects = pObjects, rObjects = rObjects))
  expect_s3_class(output$RDAPlotNA, "shiny.render.function")
  expect_s3_class(output$RDAPlotNA_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
  expect_s3_class(output$RDAPlotNA_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
  
})
