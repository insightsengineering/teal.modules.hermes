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
#' mae_data <- dataset("MAE", mae)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     static = {
#'       tm_g_forest_tte(
#'         label = "forestplot",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_forest_tte <- function(label,
                         mae_name,
                         pre_output = NULL,
                         post_output = NULL,
                         plot_height = c(600L, 200L, 2000L),
                         plot_width = c(1360L, 500L, 2000L)) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_forest_tte,
    server_args = list(
      mae_name = mae_name,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_forest_tte,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_forest_tte sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_forest_tte <- function(id, datasets, mae_name,pre_output, post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)
  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      selectInput(ns("experiment_name"), "Select Experiment", experiment_name_choices),
      selectInput(ns("assay_name"), "Select Assay", choices = ""),
      selectizeInput(ns("geneid"), "Gene ID", choices = ""),
      sliderInput(ns("probs"), label = ("Probability Cutoff"), min = 0.01, max = 0.99, value = 0.5),
      optionalSelectInput(ns("subgroups"), label = "Subgroups", choices = "", selected = "", multiple = TRUE)
    ),
    output = teal.devel::plot_with_settings_ui(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_forest_tee sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_forest_tte <- function(input,
                             output,
                             session,
                             datasets,
                             mae_name,
                             plot_height,
                             plot_width) {

  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    experiment_name <- input$experiment_name
    req(input$experiment_name) # Important to avoid running into NULL here.

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the chosen experiment changes, recompute the available assay.
  assay_names <- eventReactive(input$experiment_name, ignoreNULL = FALSE, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
  })

  # When the chosen experiment changes, recompute the available genes.
  genes <- eventReactive(input$experiment_name, ignoreNULL = FALSE, {
    object <- experiment_data()
    rownames(object)
  })

  # When the chosen experiment changes, recompute the available colData.
  subgroups <- eventReactive(input$experiment_name, {
    adtte <- datasets$get_data("ADTTE", filtered = TRUE)
    colnames(adtte)
  })

  # When the assay names change, update the choices for assay.
  observeEvent(assay_names(), {
    assay_name_choices <- assay_names()

    updateSelectInput(
      session,
      "assay_name",
      choices = assay_name_choices,
      selected = assay_name_choices[1]
    )
  })

  # When the genes are recomputed, update the choice for genes in the UI.
  observeEvent(genes(), {
    gene_choices <- genes()

    updateSelectizeInput(
      session,
      "geneid",
      choices = gene_choices,
      selected = gene_choices[1],
      server = TRUE
    )
  })

  observeEvent(subgroups(), {
    subgroup_choices <- subgroups()

    updateOptionalSelectInput(
      session,
      "subgroups",
      choices = subgroup_choices,
      selected = subgroup_choices[1]
    )
  })

  adtte_counts <- reactive({
    mae <- datasets$get_data(mae_name, filtered = TRUE)
    adtte <- datasets$get_data("ADTTE", filtered = TRUE)
    geneid <- input$geneid
    experiment_name <- input$experiment_name
    assay_name <- input$assay_name

    req(geneid, experiment_name, assay_name)

    h_km_mae_to_adtte(
      adtte,
      mae,
      gene_var = geneid,
      experiment_name = experiment_name,
      assay_name = assay_name
    )
  })

  adtte_final <- reactive({
    adtte_counts <- adtte_counts()
    probs <- input$probs
    colname <- attr(adtte_counts, "gene_cols")

    adtte_counts %>%
      tern::df_explicit_na() %>%
      dplyr::mutate(
        AVAL = tern::day2month(AVAL),
        AVALU = "Months",
        is_event = CNSR == 0,
        gene_bin = tern::cut_quantile_bins(
          adtte_counts[[colname]],
          probs = probs,
          labels = c("Low", "High")
        )
      )
  })

  tbl <- reactive({
    adtte_final <- adtte_final()
    subgroups <- input$subgroups

    tern::extract_survival_subgroups(
      variables = list(
        tte = "AVAL",
        is_event = "is_event",
        arm = "gene_bin",
        subgroups = subgroups
      ),
      label_all = "All Patients",
      data = adtte_final
    )
  })

  result <- reactive({
    tbl <- tbl()

    rtables::basic_table() %>%
      tern::tabulate_survival_subgroups(
        df = tbl,
        vars = c("n_tot_events", "n", "n_events", "median", "hr", "ci"),
        time_unit = adtte_final()$AVALU[1]
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
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_forest_tte()
#' }
sample_tm_g_forest_tte <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  adsl <- cdisc_dataset("ADSL", rtables::ex_adsl)
  adtte <- cdisc_dataset("ADTTE", rtables::ex_adtte)
  data <- teal_data(mae_data, adsl, adtte) %>%
    mutate_join_keys("MAE", "MAE", c("STUDYID", "USUBJID"))
  app <- init(
    data = data,
    modules = root_modules(
      static = {
        tm_g_forest_tte(
          label = "forestplot",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
