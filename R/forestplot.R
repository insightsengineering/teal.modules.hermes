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
#'   dplyr::mutate(CNSR = as.logical(.data$CNSR))
#'
#' data <- teal_data(
#'   dataset(
#'     "ADTTE",
#'     adtte,
#'     code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'       dplyr::mutate(CNSR = as.logical(.data$CNSR))'
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_forest_tte <- function(label,
                            adtte_name,
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
  assert_string(adtte_name)
  assert_string(mae_name)
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
      selectizeInput(ns("paramcd"), "Select Endpoint", choices = ""),
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
    output = plotOutput(ns("plot")),
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
    adtte <- datasets$get_data(adtte_name, filtered = TRUE)
    genes <- genes()
    experiment_name <- experiment$name()
    experiment_data <- subgroups$experiment_data()
    assay <- assay()

    validate(need(genes$get_genes(), "please select at least one gene"))
    req(
      genes$returns_vector(),
      experiment_name,
      assay
    )

    mae[[experiment_name]] <- experiment_data
    h_km_mae_to_adtte(
      adtte,
      mae,
      genes = genes,
      experiment_name = experiment_name,
      assay_name = assay
    )
  })

  adtte_data <- reactive({
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

  # After post processing ADTTE, we recompute endpoints.
  endpoints <- reactive({
    adtte_data <- adtte_data()
    unique(adtte_data$PARAMCD)
  })

  # When the endpoints are recomputed, update the choices for endpoints in the UI.
  observeEvent(endpoints(), {
    endpoints <- endpoints()

    updateSelectizeInput(
      session,
      "paramcd",
      choices = endpoints,
      selected = endpoints[1],
      server = TRUE
    )
  })

  surv_subgroups <- reactive({
    endpoint <- input$paramcd
    adtte_data <- adtte_data()
    subgroups_var <- subgroups$sample_var()

    req(endpoint)
    validate(need(
      nrow(adtte_data) > 0,
      "ADTTE is empty - please relax the filter criteria"
    ))
    validate(need(
      is.null(subgroups_var) || is.factor(adtte_data[[subgroups_var]]),
      "please select a categorical variable"
    ))
    adtte_final <- dplyr::filter(adtte_data, .data$PARAMCD == endpoint)

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
    surv_subgroups <- surv_subgroups()
    lyt <- rtables::basic_table()
    tern::tabulate_survival_subgroups(
      lyt = lyt,
      df = surv_subgroups,
      vars = c("n_tot_events", "n", "n_events", "median", "hr", "ci"),
      time_unit = adtte_data()$AVALU[1]
    )
  })

  output$plot <- renderPlot({
    result <- result()
    tern::g_forest(result)
  })
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
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))

  data <- teal_data(
    dataset(
      "ADTTE",
      adtte,
      code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
        dplyr::mutate(CNSR = as.logical(.data$CNSR))'
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
