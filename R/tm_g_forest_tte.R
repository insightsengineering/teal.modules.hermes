#' Teal Module for Survival Forest Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module provides an interactive survival forest plot.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   ADTTE <- teal.data::rADTTE %>%
#'     dplyr::mutate(is_event = .data$CNSR == 0)
#'   MAE <- hermes::multi_assay_experiment
#' })
#' join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")
#'
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
                            plot_width = c(1360L, 500L, 2000L),
                            .test = FALSE,
                            transformators = list()) {
  message("Initializing tm_g_forest_tte")
  assert_string(label)
  assert_string(adtte_name)
  assert_string(mae_name)
  assert_adtte_vars(adtte_vars)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_summary_funs(summary_funs)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)
  assert_flag(.test)

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
      plot_width = plot_width,
      .test = .test
    ),
    ui = ui_g_forest_tte,
    ui_args = list(
      adtte_name = adtte_name,
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output,
      .test = .test
    ),
    transformators = transformators,
    datanames = c(adtte_name, mae_name)
  )
}

#' @describeIn tm_g_forest_tte sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_forest_tte <- function(id,
                            adtte_name,
                            mae_name,
                            summary_funs,
                            pre_output,
                            post_output,
                            .test = FALSE) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = tags$div(
      ### Reporter
      teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
      tags$br(), tags$br(),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      uiOutput(ns("experiment_ui")),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      helpText("Analysis of ADTTE:", tags$code(adtte_name)),
      adtteSpecInput(ns("adtte")),
      bslib::accordion(
        bslib::accordion_panel(
          input_id = "settings_item",
          open = TRUE,
          title = "Additional Settings",
          sliderInput(ns("probs"), label = ("Probability Cutoff"), min = 0.01, max = 0.99, value = 0.5),
          sampleVarSpecInput(ns("subgroups"), "Select Categorical Subgroup Variable")
        )
      )
    ),
    output = div(
      if (.test) verbatimTextOutput(ns("table")) else NULL,
      teal.widgets::plot_with_settings_ui(ns("plot"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_forest_tte sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_forest_tte <- function(id,
                             data,
                             filter_panel_api,
                             reporter,
                             adtte_name,
                             mae_name,
                             adtte_vars,
                             exclude_assays,
                             summary_funs,
                             plot_height,
                             plot_width,
                             .test = FALSE) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  assert_class(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.hermes")
    output$experiment_ui <- renderUI({
      experimentSpecInput(session$ns("experiment"), data, mae_name)
    })
    experiment <- experimentSpecServer(
      "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
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
      categorical_only = TRUE,
      explicit_na = TRUE
    )
    adtte <- adtteSpecServer(
      "adtte",
      data = data,
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
      res <- result()
      tern::g_forest(res)
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = forest_plot,
      height = plot_height,
      width = plot_width
    )

    if (.test) {
      table_r <- reactive({
        rtables::as_result_df(result())
      })
      output$table <- renderPrint(table_r())
    }

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- report_card_template(
          title = "Forest Plot",
          label = label,
          with_filter = TRUE,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Selected Options", "header3")
        encodings_list <- list(
          "Experiment:",
          input$`experiment-name`,
          "\nAssay:",
          input$`assay-name`,
          "\nGenes Selected:",
          paste0(genes()$get_gene_labels(), collapse = ", "),
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
      teal.reporter::add_card_button_srv("add_reporter", reporter = reporter, card_fun = card_fun)
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
sample_tm_g_forest_tte <- function(.test = FALSE) { # nolint
  data <- within(teal.data::teal_data(), {
    ADTTE <- teal.data::rADTTE |> # nolint
      dplyr::mutate(is_event = .data$CNSR == 0)
    MAE <- hermes::multi_assay_experiment # nolint
  })
  join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")

  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_forest_tte(
        label = "forest",
        adtte_name = "ADTTE",
        mae_name = "MAE",
        .test = .test
      )
    )
  )
  shinyApp(app$ui, app$server)
}
