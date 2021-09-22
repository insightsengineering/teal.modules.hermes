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
#'     tm_g_forest_tte(
#'       label = "forestplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_forest_tte <- function(label,
                            mae_name,
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
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_list(
    summary_funs,
    types = "function",
    min.len = 1L,
    unique = TRUE,
    any.missing = FALSE,
    names = "unique"
  )
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_forest_tte,
    server_args = list(
      mae_name = mae_name,
      exclude_assays = exclude_assays,
      summary_funs = summary_funs,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_forest_tte,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_forest_tte sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_forest_tte <- function(id,
                            datasets,
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
      geneSpecInput(ns("genes"), summary_funs, label_genes = "Select Gene(s)"),
      sliderInput(ns("probs"), label = ("Probability Cutoff"), min = 0.01, max = 0.99, value = 0.5),
      sampleVarSpecInput(ns("subgroups"), "Optional subgroup variable")
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
                             mae_name,
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

  adtte_counts <- reactive({
    mae <- datasets$get_data(mae_name, filtered = TRUE)
    adtte <- datasets$get_data("ADTTE", filtered = TRUE)
    genes <- genes()
    experiment_name <- experiment$name
    assay <- assay()

    req(
      genes,
      experiment_name,
      assay
    )
    # TODO : need to change the h_km_mae_to_adtte function to take gene spec
    # as well as the HermesData object directly coming out of sample var
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

    adtte_counts <- tern::df_explicit_na(adtte_counts)
    adtte_counts <- dplyr::mutate(
      adtte_counts,
      AVAL = tern::day2month(.data$AVAL),
      AVALU = "Months",
      is_event = .data$CNSR == 0,
      gene_bin = tern::cut_quantile_bins(
        adtte_counts[[colname]],
        probs = probs,
        labels = c("Low", "High")
      )
    )
  })

  tbl <- reactive({
    adtte_final <- adtte_final()
    subgroups_var <- subgroups$sample_var()

    tern::extract_survival_subgroups(
      variables = list(
        tte = "AVAL",
        is_event = "is_event",
        arm = "gene_bin",
        subgroups = subgroups_var
      ),
      label_all = "All Patients",
      data = adtte_final
    )
  })

  result <- reactive({
    tbl <- tbl()
    lyt <- rtables::basic_table()
    tern::tabulate_survival_subgroups(
      lyt = lyt,
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
sample_tm_g_forest_tte <- function() { # nolint # nousage
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  adsl <- cdisc_dataset("ADSL", rtables::ex_adsl)
  adtte <- cdisc_dataset("ADTTE", rtables::ex_adtte)
  data <- teal_data(mae_data, adsl, adtte)
  data <- mutate_join_keys(data, "MAE", "MAE", c("STUDYID", "USUBJID"))
  app <- init(
    data = data,
    modules = root_modules(
      tm_g_forest_tte(
        label = "forestplot",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
