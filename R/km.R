#' Data Preprocessing for KM Module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function to help with merging of MAE to `ADTTE` for use with `g_km`.
#'
#' @note We require that each patient only has one sample.
#'
#' @inheritParams function_arguments
#'
#' @return A data frame containing all columns/rows from `adtte` that match
#'   by `USUBJID` with the row names of the MAE and have the gene samples available
#'   in the given experiment. The attributes `sample_id`
#'   and `gene_cols` contain the column names for the sample ID and gene columns.
#'
#' @note The final gene column names can start with a different string than
#'   the original gene IDs, in particular white space, dots and colons are removed,
#'   see [tern::make_names()] for details.
#'
#' @export
#' @examples
#' mae <- hermes::multi_assay_experiment
#' adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte |>
#'   dplyr::mutate(CNSR = as.logical(CNSR))
#'
#' new_adtte <- h_km_mae_to_adtte(
#'   adtte,
#'   mae,
#'   gene_var = "GeneID:1820",
#'   experiment_name = "hd2"
#' )
#' new_adtte2 <- h_km_mae_to_adtte(
#'   adtte,
#'   mae,
#'   gene_var = c("GeneID:1820", "GeneID:94115"),
#'   experiment_name = "hd2"
#' )
h_km_mae_to_adtte <- function(adtte,
                              mae,
                              gene_var,
                              experiment_name = "hd1",
                              assay_name = "counts") {
  assert_choice(
    assay_name,
    c("counts", "cpm", "rpkm", "tpm", "voom")
  )
  assert_character(gene_var, min.chars = 1L)
  assert_string(experiment_name)

  mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
  samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
  patients_in_experiment <- samplemap_experiment$primary

  assert_character(patients_in_experiment, unique = TRUE)

  merge_samplemap <- samplemap_experiment[, c("primary", "colname")]
  merge_samplemap <- as.data.frame(merge_samplemap)
  colnames(merge_samplemap) <- c("USUBJID", "SampleID")

  hd <- mae[[experiment_name]]
  assert_class(hd, "AnyHermesData")

  num_genes <- length(gene_var)
  gene_assay <- SummarizedExperiment::assay(hd, assay_name)[gene_var,]
  gene_assay <- as.data.frame(gene_assay)
  gene_names <- tern::make_names(gene_var)
  merged_names <- paste(gene_names, assay_name, sep = "_")

  if (num_genes == 1){
    colnames(gene_assay) <- merged_names
    gene_assay$SampleID <- rownames(gene_assay)
  } else {
    rownames(gene_assay) <- merged_names
    gene_assay <- data.frame(t(gene_assay), SampleID = colnames(gene_assay))
  }

  merge_se_data <- merge(merge_samplemap, gene_assay, by = "SampleID")

  adtte_patients <- unique(adtte$USUBJID)
  se_patients <- merge_se_data$USUBJID

  patients_not_in_adtte <- setdiff(se_patients, adtte_patients)
  if (length(patients_not_in_adtte) > 0) {
    showNotification(
      paste(
        "Patients", paste(patients_not_in_adtte, collapse = ", "),
        "removed from MAE because not contained in ADTTE."
      ),
      type = "warning"
    )
  }

  # Now do the inner join.
  merged_adtte <- merge(adtte, merge_se_data, by = "USUBJID")
  merged_adtte <- tern::df_explicit_na(merged_adtte)

  structure(
    merged_adtte,
    sample_id = "SampleID",
    gene_cols = merged_names
  )
}

#' Teal Module for Kaplan-Meier Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This teal module produces a grid style Kaplan-Meier plot for data with
#' ADaM structure.
#'
#' @inheritParams module_arguments
#'
#' @param adtte_name (`string`)\cr name of the ADTTE dataset.
#' @param strata_var (`character`)\cr names of the stratification variables.
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' mae <- hermes::multi_assay_experiment
#' adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte |>
#'   dplyr::mutate(CNSR = as.logical(CNSR))
#'
#' data <- teal_data(
#'   dataset(
#'     "ADTTE",
#'     adtte,
#'     code = 'adtte <- radtte(cached = TRUE) |> mutate(CNSR = as.logical(CNSR))'
#'   ),
#'   dataset("mae", mae)
#' )
#'
#' modules <- root_modules(
#'   tm_g_km(
#'     label = "KM PLOT",
#'     adtte_name = "ADTTE",
#'     mae_name = "mae",
#'     strata_var = c("SEX", "STRATA1")
#'   )
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules
#' )
#'
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_km <- function(label,
                    adtte_name,
                    mae_name,
                    strata_var = NULL,
                    pre_output = NULL,
                    post_output = NULL) {

  assert_string(label)
  assert_string(adtte_name)
  assert_string(mae_name)
  assert_character(strata_var, null.ok = TRUE)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_km,
    server_args = list(
      adtte_name = adtte_name,
      mae_name = mae_name,
      strata_var = strata_var
    ),
    ui = ui_g_km,
    ui_args = list(
      adtte_name = adtte_name,
      mae_name = mae_name,
      strata_var = strata_var,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}


#' @describeIn tm_g_km sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_km <- function(id,
                    datasets,
                    adtte_name,
                    mae_name,
                    strata_var,
                    pre_output,
                    post_output) {

  ns <- NS(id)

  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)
  adtte <- datasets$get_data(adtte_name, filtered = FALSE)
  paramcd_choices <- unique(adtte$PARAMCD)

  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),
      selectInput(ns("assay_name"), "Select assay", choices = ""),
      selectizeInput(ns("x_var"), "Select gene", choices = ""),
      selectizeInput(ns("paramcd"), "Select endpoint", choices = ""),
      selectizeInput(ns("strata_var"), "Select strata", choices = strata_var, multiple = TRUE),
      sliderInput(
        ns("percentiles"),
        "Select quantiles to be displayed",
        min = 0,
        max = 1,
        value = c(0, 0.5)
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
                     strata_var) {

  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the filtered data set or the chosen experiment changes, update
  # the call that creates the chosen experiment data object.
  experiment_call <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    dat <- datasets$get_filtered_datasets(mae_name)
    dat$get_filter_states(input$experiment_name)$get_call()
  })

  # When the chosen experiment changes, recompute the assay names.
  assay_names <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
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

  # When the chosen experiment call changes, we recompute gene names.
  genes <- eventReactive(experiment_call(), ignoreNULL = FALSE, {
    object <- experiment_data()
    rownames(object)
  })

  # When the genes are recomputed, update the choices for genes in the UI.
  observeEvent(genes(), {
    gene_choices <- genes()

    updateSelectizeInput(
      session,
      "x_var",
      choices = gene_choices,
      selected = gene_choices[1],
      server = TRUE
    )
  })

  # When the gene changes, post process ADTTE.
  adtte_data <- reactive({
    # Resolve all reactivity
    experiment_data <- experiment_data()
    experiment_name <- input$experiment_name
    assay_name <- input$assay_name
    gene_var <- input$x_var

    req(gene_var)
    validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))
    req(isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)))
    mae_data <- datasets$get_data(mae_name, filtered = TRUE)
    adtte_data <- datasets$get_data(adtte_name, filtered = TRUE)
    adtte_data$CNSR <- as.logical(adtte_data$CNSR)

    h_km_mae_to_adtte(
      adtte_data,
      mae_data,
      gene_var = gene_var,
      experiment_name = experiment_name,
      assay_name = assay_name
    )
  })

  # After post processing ADTTE, we recompute endpoints.
  endpoints <- eventReactive(input$x_var, ignoreNULL = TRUE, {

    # Resolve reactivity
    adtte_data <- adtte_data()

    # Get the endpoints from post processed ADTTE
    unique(adtte_data$PARAMCD)

  })

  # When the endpoints are recomputed, update the choices for endpoints in the UI.
  observeEvent(endpoints(), {
    endpoint_choices <- endpoints()

    updateSelectizeInput(
      session,
      "paramcd",
      choices = endpoint_choices,
      selected = endpoint_choices[1],
      server = TRUE
    )
  })

  output$km_plot <- renderPlot({
    # Resolve all reactivity.
    experiment_name <- input$experiment_name
    assay_name <- input$assay_name
    gene_var <- input$x_var
    endpoint <- input$paramcd
    strata_var <- input$strata_var
    percentiles <- input$percentiles
    adtte_data <- adtte_data()

    # require endpoint for plot to generate
    req(endpoint)

    # validate that adtte_data is not empty
    validate(need(nrow(adtte_data) > 0, message = "ADTTE is empty - please relax the filter criteria"))

    # We need the gene counts column name (the selected gene_var/x_var) to add to the 'arm'
    # variable in the list.
    arm_name <- attr(adtte_data, "gene_cols")
    adtte_data[, arm_name] <- as.numeric(adtte_data[, arm_name])
    adtte_data <- dplyr::filter(adtte_data, .data$PARAMCD == endpoint)
    adtte_data <- droplevels(adtte_data)

    percentiles_without_borders <- setdiff(percentiles, c(0, 1))

    binned_adtte <- tryCatch({
      dplyr::mutate(
        adtte_data,
        gene_factor = tern::cut_quantile_bins(adtte_data[, arm_name], probs = percentiles_without_borders)
      )},
      error = function(e) {
        if (grepl("Duplicate quantiles produced", e)) {
          validate("please select (slightly) different quantiles to avoid duplicate quantiles")
        } else {
          stop(e)
        }
      }
    )

    variables <- list(tte = "AVAL", is_event = "CNSR", arm = "gene_factor", strat = strata_var)
    tern::g_km(binned_adtte, variables = variables, annot_coxph = TRUE)
  })
}

#' @describeIn tm_g_km sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_km()
#' }
sample_tm_g_km <- function() { # nolint # nousage

  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte |>
    dplyr::mutate(CNSR = as.logical(.data$CNSR))

  data <- teal_data(
    dataset(
      "ADTTE",
      adtte,
      code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte |>
        dplyr::mutate(CNSR = as.logical(.data$CNSR))'
    ),
    dataset("mae", mae)
  )

  modules <- root_modules(
    tm_g_km(
      label = "KM PLOT",
      adtte_name = "ADTTE",
      mae_name = "mae",
      strata_var = c("SEX", "STRATA1")
    )
  )

  app <- init(
    data = data,
    modules = modules
  )

  shinyApp(ui = app$ui, server = app$server)
}
