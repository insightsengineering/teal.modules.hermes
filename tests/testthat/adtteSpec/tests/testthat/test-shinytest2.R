library(shinytest2)

test_that("{shinytest2} recording: adtteSpecServer module works as expected in the test app", {
  app <- AppDriver$new(
    name = "adtteSpecServer module works as expected in the test app",
    height = 520, width = 979
  )
  app$set_inputs(`teal-main_ui-filter_panel-add_ADTTE_filter-filter-var_to_add` = character(0))
  app$set_inputs(`teal-main_ui-filter_panel-add_MAE_filter-subjects-var_to_add` = character(0))
  app$set_inputs(`teal-main_ui-filter_panel-add_MAE_filter-hd1-row_to_add` = character(0))
  app$set_inputs(`teal-main_ui-filter_panel-add_MAE_filter-hd1-col_to_add` = character(0))
  app$set_inputs(`teal-main_ui-filter_panel-add_MAE_filter-hd2-row_to_add` = character(0))
  app$set_inputs(`teal-main_ui-filter_panel-add_MAE_filter-hd2-col_to_add` = character(0))
  app$set_inputs(`teal-main_ui-filter_panel-add_MAE_filter-hd3-row_to_add` = character(0))
  app$set_inputs(`teal-main_ui-filter_panel-add_MAE_filter-hd3-col_to_add` = character(0))
  app$set_inputs(`teal-main_ui-root-active_tab` = "adttespec_example")
  app$click("teal-main_ui-root-adttespec_example-genes-select_none_button")
  app$click("teal-main_ui-root-adttespec_example-genes-select_all_button")
  app$click("teal-main_ui-root-adttespec_example-genes-text_button")
  app$click("teal-main_ui-filter_panel-remove_all_filters")
  app$click("teal-main_ui-filter_panel-ADTTE_filter-remove_filters")
  app$click("teal-main_ui-filter_panel-MAE_filter-remove_filters")
  app$set_inputs(`teal-main_ui-root-adttespec_example-experiment-name` = "hd1")
  app$set_inputs(`teal-main_ui-root-adttespec_example-assay-name` = "")
  app$set_inputs(`teal-main_ui-root-adttespec_example-genes-genes` = character(0))
  app$set_inputs(`teal-main_ui-root-adttespec_example-genes-fun_name` = "Mean")
  app$set_inputs(`teal-main_ui-root-adttespec_example-adtte-paramcd` = "")
  app$set_inputs(`teal-main_ui-root-adttespec_example-genes-lock_button` = FALSE)
  app$expect_values()
  app$set_inputs(`teal-main_ui-root-adttespec_example-genes-genes` = "GeneID:28")

  # Upon initialization the endpoint is not selected automatically, the user
  # has to click this actively.
  app$expect_values()
  app$set_inputs(`teal-main_ui-root-adttespec_example-adtte-paramcd` = "CRSD")
  app$expect_values()

  # Test what happens if selected endpoint (here PFS) is no longer in filtered data.
  app$set_inputs(`teal-main_ui-filter_panel-add_ADTTE_filter-filter-var_to_add` = "PARAMCD")
  app$click("teal-main_ui-filter_panel-ADTTE_filter-filter-_var_PARAMCD-remove")
  app$set_inputs(`teal-main_ui-filter_panel-ADTTE_filter-filter-_var_PARAMCD-content-selection` = "OS")
  app$expect_values()

  # Now we update the filter by adding PFS back. However the user would have to
  # actively select it.
  app$set_inputs(`teal-main_ui-filter_panel-ADTTE_filter-filter-_var_PARAMCD-content-selection` = c(
    "OS",
    "PFS"
  ))
  app$expect_values()
})
