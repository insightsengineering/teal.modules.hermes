# ui_g_boxplot ----

test_that("ui_g_boxplot creates expected HTML", {
  mae_name <- "MyMAE"
  data <- teal.data::teal_data(MyMAE = function() hermes::multi_assay_experiment)
  result <- ui_g_boxplot(
    id = "testid",
    mae_name = mae_name,
    summary_funs = list(Mean = colMeans),
    pre_output = NULL,
    post_output = NULL
  )
  expect_tag(result)
})

# tm_g_boxplot ----

# nolint start

test_that("boxplot module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("boxplot"),
    name = "boxplot",
    variant = platform_variant(),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)


  # check initialization
  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "counts")

  res <- app$get_active_module_input("strat-sample_var")
  expect_null(res)

  res <- app$get_active_module_input("genes-genes")
  expect_null(res)

  # check initial message
  res <- app$get_active_module_output("table")
  expect_equal(res$message, "please select at least one gene")

  # Do a couple of updates to obtain a plot.
  app$set_module_input("jitter", TRUE)
  app$set_module_input("violin", TRUE)
  app$set_module_input("genes-genes", "GeneID:5205")
  app$set_module_input("strat-sample_var", "COUNTRY")
  app$set_module_input("color-sample_var", "AGE18")


  app$wait_for_idle()

  res <- app$get_active_module_output("table")
  expect_snapshot(cat(res))

  app$stop()
})

# nolint end
