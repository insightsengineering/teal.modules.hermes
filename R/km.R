#' Teal Module for `Kaplan-Meier` Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This teal module produces a grid style `Kaplan-Meier` plot for data with
#' `ADaM` structure.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   ADTTE <- teal.modules.hermes::rADTTE %>%
#'     dplyr::mutate(is_event = .data$CNSR == 0)
#'   MAE <- hermes::multi_assay_experiment
#' })
#' datanames <- c("ADTTE", "MAE")
#' datanames(data) <- datanames
#' join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#'
#' modules <- modules(
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
  message("Initializing tm_g_km")
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
    datanames = c(adtte_name, mae_name)
  )
}

#' @describeIn tm_g_km sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_km <- function(id,
                    adtte_name,
                    mae_name,
                    summary_funs,
                    pre_output,
                    post_output) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      uiOutput(ns("experiment_ui")),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      helpText("Analysis of ADTTE:", tags$code(adtte_name)),
      adtteSpecInput(ns("adtte")),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
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
    output = teal.widgets::plot_with_settings_ui(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_km sets up the user interface.
#' @inheritParams module_arguments
#' @export
srv_g_km <- function(id,
                     data,
                     filter_panel_api,
                     reporter,
                     adtte_name,
                     mae_name,
                     adtte_vars,
                     summary_funs,
                     exclude_assays) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  assert_class(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$experiment_ui <- renderUI({
      experimentSpecInput(ns("experiment"), data, mae_name)
    })
    experiment <- experimentSpecServer(
      "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
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
      data = data,
      adtte_name = adtte_name,
      mae_name = mae_name,
      adtte_vars = adtte_vars,
      experiment_data = strata$experiment_data,
      experiment_name = experiment$name,
      assay = assay,
      genes = genes,
      probs = percentiles_without_borders
    )

    km_plot <- reactive({
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

    output$km_plot <- renderPlot(km_plot())

    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = km_plot
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- report_card_template(
          title = "Kaplan-Meier Plot",
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
          "\nStrata Selected:",
          input$`strata-sample_var`,
          "\nQuantiles Displayed:",
          paste0(input$percentiles, collapse = "-")
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
        card$append_plot(km_plot(), dim = pws$dim())
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

#' @describeIn tm_g_km sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_km()
#' }
sample_tm_g_km <- function() { # nolint
  data <- teal_data()
  data <- within(data, {
    ADTTE <- teal.modules.hermes::rADTTE %>% # nolint
      dplyr::mutate(is_event = .data$CNSR == 0)
    MAE <- hermes::multi_assay_experiment # nolint
  })
  datanames <- c("ADTTE", "MAE")
  datanames(data) <- datanames
  join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")

  modules <- teal::modules(
    tm_g_km(
      label = "kaplan-meier",
      adtte_name = "ADTTE",
      mae_name = "MAE"
    )
  )

  app <- teal::init(
    data = data,
    modules = modules
  )

  shinyApp(ui = app$ui, server = app$server)
}
