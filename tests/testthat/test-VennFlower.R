test_that("venn.flower() function return venn flower plot", {
  expect_equal(venn.flower(gene_absence), recordPlot())
})
