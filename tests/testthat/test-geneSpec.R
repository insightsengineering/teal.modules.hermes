# geneSpecInput ----

test_that("geneSpecInput creates expected HTML", {
  expect_snapshot(geneSpecInput(
    "my_genes",
    funs = list(mean = colMeans),
    label_funs = "Please select function"
  ))
})

# geneSpecServer ----

test_that("geneSpec module works as expected in the test app", {
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new("scatterplot/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  # Note: left hand side name is composed as:
  # prefix: teal-main_ui-modules_ui-root_
  # label: scatterplot-
  # inputId: assay_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-experiment_name")
  expect_identical(initial_experiment_name, "hd1")

  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_scatterplot-plot", "message")
  expect_identical(
    plot_message,
    "no assays are available for this experiment, please choose another experiment"
  )

  # Choose another experiment.
  app$setInputs(
    "teal-main_ui-modules_ui-root_scatterplot-experiment_name" = "hd2"
  )

  initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-assay_name")
  expect_identical(initial_assay_name, "cpm")

  initial_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-x_var", ignore = "")
  expect_identical(initial_x_var, NULL)

  initial_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-y_var", ignore = "")
  expect_identical(initial_y_var, NULL)

  # Initially there is no plot.
  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_scatterplot-plot", "message")
  expect_identical(
    plot_message,
    "please select x gene"
  )

  # Check what happens if genes are the same.
  app$setInputs(
    "teal-main_ui-modules_ui-root_scatterplot-x_var" = "GeneID:5205",
    "teal-main_ui-modules_ui-root_scatterplot-y_var" = "GeneID:5205"
  )
  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_scatterplot-plot", "message")
  expect_identical(plot_message, "please select different genes for x and y variables")

  # Change the sample filter and confirm that genes are not updated.
  app$setInputs(
    "teal-main_ui-filter_panel-add_MAE_filter-hd2-col_to_add" = "ARM"
  )
  now_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-x_var")
  expect_identical(now_x_var, "GeneID:5205")

  now_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-y_var")
  expect_identical(now_y_var, "GeneID:5205")

  # Change the genes filter and confirm that genes are staying the same.
  app$setInputs(
    "teal-main_ui-filter_panel-add_MAE_filter-hd2-row_to_add" = "Chromosome",
    wait_ = FALSE, values_ = FALSE
  )
  now_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-x_var", ignore = "")
  expect_identical(now_x_var, "GeneID:5205")

  now_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-y_var", ignore = "")
  expect_identical(now_y_var, "GeneID:5205")

  # Now change the experiment_name, genes, method.
  app$setInputs(
    "teal-main_ui-modules_ui-root_scatterplot-experiment_name" = "hd2",
    "teal-main_ui-modules_ui-root_scatterplot-x_var" = "GeneID:5205",
    "teal-main_ui-modules_ui-root_scatterplot-y_var" = "GeneID:102723793",
    "teal-main_ui-modules_ui-root_scatterplot-smooth_method" = "loess"
  )

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_scatterplot-plot",
    name = "final_plot.png"
  )

  app$stop()
})

# validate_gene_spec ----

test_that("validate_gene_spec works as expected", {
  expect_error(
    validate_gene_spec(hermes::gene_spec(), letters),
    "please select at least one gene"
  )
  gene_spec <- hermes::gene_spec(c("a", "b", "z"))
  expect_silent(validate_gene_spec(gene_spec, letters))
  expect_error(
    validate_gene_spec(gene_spec, letters[1:3]),
    "1 gene (z) not included",
    fixed = TRUE
  )
})
