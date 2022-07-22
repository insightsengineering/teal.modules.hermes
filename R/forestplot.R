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
#'   modules = modules(
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

  teal::module(
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
  teal.widgets::standard_layout(
    encoding = div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      shiny::tags$br(),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      helpText("Analysis of ADTTE:", tags$code(adtte_name)),
      adtteSpecInput(ns("adtte")),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sliderInput(ns("probs"), label = ("Probability Cutoff"), min = 0.01, max = 0.99, value = 0.5),
          sampleVarSpecInput(ns("subgroups"), "Select Subgroup Variable")
        )
      )
    ),
    output = teal.widgets::plot_with_settings_ui(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_forest_tte sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_forest_tte <- function(id,
                             datasets,
                             reporter,
                             adtte_name,
                             mae_name,
                             adtte_vars,
                             exclude_assays,
                             summary_funs,
                             plot_height,
                             plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
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
      original_data = experiment$data,
      categorical_only = TRUE
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

    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = forest_plot,
      height = plot_height,
      width = plot_width
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Forest Plot")
        card$append_text("Forest Plot", "header2")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Selected Options", "header3")
        encodings_list <- list(
          "Experiment:",
          input$`experiment-name`,
          "\nAssay:",
          input$`assay-name`,
          "\nGenes Selected:",
          paste0(input$`genes-genes`, collapse = ", "),
          "\nGene Summary:",
          input$`genes-fun_name`,
          "\nEndpoint:",
          input$`adtte-paramcd`,
          "\nProbability Cutoff:",
          input$probs,
          "\nSubgroup Variable:",
          input$`subgroups-sample_var`
        )
        null_encodings_indices <- which(sapply(encodings_list, function(x) is.null(x) || x == ""))
        final_encodings <- if (length(null_encodings_indices) > 0) {
          null_encodings_indices_1 <- c(null_encodings_indices, null_encodings_indices - 1)
          paste(encodings_list[-null_encodings_indices_1], collapse = " ")
        } else {
          paste(encodings_list, collapse = " ")
        }

        card$append_text(final_encodings, style = "verbatim")
        card$append_text("Plot", "header3")
        card$append_plot(forest_plot(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
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

  data <- teal.data::teal_data(
    teal.data::dataset(
      "ADTTE",
      adtte,
      code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
        dplyr::mutate(is_event = .data$CNSR == 0)'
    ),
    teal.data::dataset("MAE", mae)
  )

  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_forest_tte(
        label = "forest",
        adtte_name = "ADTTE",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
