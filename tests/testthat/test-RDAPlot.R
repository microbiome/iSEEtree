test_that("RDAPlot", {
  
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

})
