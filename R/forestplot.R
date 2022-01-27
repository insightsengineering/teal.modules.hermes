#' Teal Module for Survival Forest Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module provides an interactive survival forest plot.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' mae <- hermes::multi_assay_experiment
#' adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'   dplyr::mutate(is_event = (.data$CNSR == 0))
#'
#' data <- teal_data(
#'   dataset(
#'     "ADTTE",
#'     adtte,
#'     code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'       dplyr::mutate(is_event = (.data$CNSR == 0))'
#'   ),
#'   dataset("MAE", mae)
#' )
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     tm_g_forest_tte(
#'       label = "forestplot",
#'       adtte_name = "ADTTE",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_forest_tte <- function(label,
                            adtte_name,
                            mae_name,
                            adtte_vars = list(
                              aval = "AVAL",
                              is_event = "is_event",
                              paramcd = "PARAMCD",
                              usubjid = "USUBJID",
                              avalu = "AVALU"
                            ),
                            exclude_assays = "counts",
                            summary_funs = list(
                              Mean = colMeans,
                              Median = matrixStats::colMedians,
                              Max = matrixStats::colMaxs
                            ),
                            pre_output = NULL,
                            post_output = NULL,
                            plot_height = c(600L, 200L, 2000L),
                            plot_width = c(1360L, 500L, 2000L)) {
  logger::log_info("Initializing tm_g_forest_tte")
  assert_string(label)
  assert_string(adtte_name)
  assert_string(mae_name)
  assert_adtte_vars(adtte_vars)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_summary_funs(summary_funs)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_forest_tte,
    server_args = list(
      adtte_name = adtte_name,
      mae_name = mae_name,
      adtte_vars = adtte_vars,
      exclude_assays = exclude_assays,
      summary_funs = summary_funs,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_forest_tte,
    ui_args = list(
      adtte_name = adtte_name,
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = c(adtte_name, mae_name)
  )
}

#' @describeIn tm_g_forest_tte sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_forest_tte <- function(id,
                            datasets,
                            adtte_name,
                            mae_name,
                            summary_funs,
                            pre_output,
                            post_output) {
  ns <- NS(id)
  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      helpText("Analysis of ADTTE:", tags$code(adtte_name)),
      adtteSpecInput(ns("adtte")),
      teal.devel::panel_group(
        teal.devel::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sliderInput(ns("probs"), label = ("Probability Cutoff"), min = 0.01, max = 0.99, value = 0.5),
          sampleVarSpecInput(ns("subgroups"), "Select Subgroup Variable")
        )
      )
    ),
    output = teal.devel::plot_with_settings_ui(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_forest_tte sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_forest_tte <- function(input,
                             output,
                             session,
                             datasets,
                             adtte_name,
                             mae_name,
                             adtte_vars,
                             exclude_assays,
                             summary_funs,
                             plot_height,
                             plot_width) {
  experiment <- experimentSpecServer(
    "experiment",
    datasets = datasets,
    mae_name = mae_name
  )
  assay <- assaySpecServer(
    "assay",
    assays = experiment$assays,
    exclude_assays = exclude_assays
  )
  genes <- geneSpecServer(
    "genes",
    funs = summary_funs,
    gene_choices = experiment$genes
  )
  subgroups <- sampleVarSpecServer(
    "subgroups",
    experiment_name = experiment$name,
    original_data = experiment$data
  )
  adtte <- adtteSpecServer(
    "adtte",
    datasets = datasets,
    adtte_name = adtte_name,
    mae_name = mae_name,
    adtte_vars = adtte_vars,
    experiment_data = subgroups$experiment_data,
    experiment_name = experiment$name,
    assay = assay,
    genes = genes,
    probs = reactive({
      input$probs
    })
  )

  surv_subgroups <- reactive({
    binned_adtte <- adtte$binned_adtte_subset()
    subgroups_var <- subgroups$sample_var()

    validate(need(
      is.null(subgroups_var) || is.factor(binned_adtte[[subgroups_var]]),
      "please select a categorical variable"
    ))

    tern::extract_survival_subgroups(
      variables = list(
        tte = adtte_vars$aval,
        is_event = adtte_vars$is_event,
        arm = adtte$gene_factor,
        subgroups = subgroups_var
      ),
      label_all = "All Patients",
      data = binned_adtte
    )
  })

  result <- reactive({
    surv_subgroups <- surv_subgroups()
    lyt <- rtables::basic_table()
    time_unit <- adtte$time_unit()

    tern::tabulate_survival_subgroups(
      lyt = lyt,
      df = surv_subgroups,
      vars = c("n_tot_events", "n", "n_events", "median", "hr", "ci"),
      time_unit = time_unit
    )
  })

  forest_plot <- reactive({
    result <- result()
    tern::g_forest(result)
  })

  callModule(
    teal.devel::plot_with_settings_srv,
    id = "plot",
    plot_r = forest_plot,
    height = plot_height,
    width = plot_width
  )
}

#' @describeIn tm_g_forest_tte sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_forest_tte()
#' }
sample_tm_g_forest_tte <- function() { # nolint

  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(is_event = .data$CNSR == 0)

  data <- teal_data(
    dataset(
      "ADTTE",
      adtte,
      code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
        dplyr::mutate(is_event = .data$CNSR == 0)'
    ),
    dataset("MAE", mae)
  )

  app <- init(
    data = data,
    modules = root_modules(
      tm_g_forest_tte(
        label = "forest",
        adtte_name = "ADTTE",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
