#' Teal Module for Kaplan-Meier Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This teal module produces a grid style Kaplan-Meier plot for data with
#' ADaM structure.
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
#'
#' modules <- root_modules(
#'   tm_g_km(
#'     label = "kaplan-meier",
#'     adtte_name = "ADTTE",
#'     mae_name = "MAE"
#'   )
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules
#' )
#'
#' if (interactive()) {
#'   shinyApp(ui = app$ui, server = app$server)
#' }
tm_g_km <- function(label,
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
                    post_output = NULL) {
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
    server = srv_g_km,
    server_args = list(
      adtte_name = adtte_name,
      mae_name = mae_name,
      adtte_vars = adtte_vars,
      exclude_assays = exclude_assays,
      summary_funs = summary_funs
    ),
    ui = ui_g_km,
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

#' @describeIn tm_g_km sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_km <- function(id,
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
          sampleVarSpecInput(ns("strata"), "Select Strata"),
          sliderInput(
            ns("percentiles"),
            "Select quantiles to be displayed",
            min = 0,
            max = 1,
            value = c(0, 0.5)
          )
        )
      )
    ),
    output = plotOutput(ns("km_plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_km sets up the user interface.
#' @inheritParams module_arguments
#' @export
srv_g_km <- function(input,
                     output,
                     session,
                     datasets,
                     adtte_name,
                     mae_name,
                     adtte_vars,
                     summary_funs,
                     exclude_assays) {
  experiment <- experimentSpecServer(
    "experiment",
    datasets = datasets,
    mae_name = mae_name,
    sample_vars_as_factors = FALSE # To avoid converting logical `event` to factor.
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
  strata <- sampleVarSpecServer(
    "strata",
    experiment_name = experiment$name,
    original_data = experiment$data
  )
  percentiles_without_borders <- reactive({
    percentiles <- input$percentiles

    result <- setdiff(percentiles, c(0, 1))
    validate(need(
      length(result) > 0,
      "Please select at least one quantile other than 0 and 1"
    ))
    result
  })
  adtte <- adtteSpecServer(
    "adtte",
    datasets = datasets,
    adtte_name = adtte_name,
    mae_name = mae_name,
    adtte_vars = adtte_vars,
    experiment_data = strata$experiment_data,
    experiment_name = experiment$name,
    assay = assay,
    genes = genes,
    probs = percentiles_without_borders
  )

  output$km_plot <- renderPlot({
    strata_var <- strata$sample_var()
    binned_adtte <- adtte$binned_adtte_subset()

    variables <- list(
      tte = adtte_vars$aval,
      is_event = adtte_vars$is_event,
      arm = adtte$gene_factor,
      strat = strata_var
    )
    tern::g_km(binned_adtte, variables = variables, annot_coxph = TRUE)
  })
}

#' @describeIn tm_g_km sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_km()
#' }
sample_tm_g_km <- function() { # nolint # nousage

  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(is_event = (.data$CNSR == 0))

  data <- teal_data(
    dataset(
      "ADTTE",
      adtte,
      code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
        dplyr::mutate(is_event = (.data$CNSR == 0))'
    ),
    dataset("MAE", mae)
  )

  modules <- root_modules(
    tm_g_km(
      label = "kaplan-meier",
      adtte_name = "ADTTE",
      mae_name = "MAE"
    )
  )

  app <- init(
    data = data,
    modules = modules
  )

  shinyApp(ui = app$ui, server = app$server)
}
