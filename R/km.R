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
#'   the original gene IDs (or labels), in particular white space, dots and colons are removed,
#'   see [tern::make_names()] for details.
#'
#' @export
#' @examples
#' mae <- hermes::multi_assay_experiment
#' adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'   dplyr::mutate(CNSR = as.logical(CNSR))
#'
#' new_adtte <- h_km_mae_to_adtte(
#'   adtte,
#'   mae,
#'   genes = hermes::gene_spec("GeneID:1820"),
#'   experiment_name = "hd2"
#' )
#' new_adtte2 <- h_km_mae_to_adtte(
#'   adtte,
#'   mae,
#'   genes = hermes::gene_spec(c("GeneID:1820", "GeneID:94115"), fun = colMeans),
#'   experiment_name = "hd2"
#' )
#' new_adtte3 <- h_km_mae_to_adtte(
#'   adtte,
#'   mae,
#'   genes = hermes::gene_spec(c(A = "GeneID:1820", B = "GeneID:94115")),
#'   experiment_name = "hd2"
#' )
h_km_mae_to_adtte <- function(adtte,
                              mae,
                              genes,
                              experiment_name = "hd1",
                              assay_name = "counts") {
  assert_choice(
    assay_name,
    c("counts", "cpm", "rpkm", "tpm", "voom")
  )
  assert_class(genes, "GeneSpec")
  assert_string(experiment_name)

  mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
  samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
  patients_in_experiment <- samplemap_experiment$primary

  assert_character(patients_in_experiment, unique = TRUE)

  merge_samplemap <- samplemap_experiment[, c("primary", "colname")]
  merge_samplemap <- as.data.frame(merge_samplemap)
  colnames(merge_samplemap) <- c("USUBJID", "SampleID")

  hd <- suppressWarnings(MultiAssayExperiment::getWithColData(mae, experiment_name))
  assert_class(hd, "AnyHermesData")

  assay_matrix <- SummarizedExperiment::assay(hd, assay_name)
  gene_assay <- genes$extract(assay_matrix)
  if (!is.matrix(gene_assay)) {
    gene_assay <- t(gene_assay)
  }
  colnames(gene_assay) <- colnames(hd)
  gene_assay <- as.data.frame(gene_assay)
  num_genes <- nrow(gene_assay)
  gene_names <- if (num_genes == 1) {
    genes$get_label()
  } else {
    genes$get_gene_labels()
  }
  gene_names <- tern::make_names(gene_names)
  merged_names <- paste(gene_names, assay_name, sep = "_")
  rownames(gene_assay) <- merged_names
  gene_assay <- data.frame(
    t(gene_assay),
    SampleID = colnames(gene_assay)
  )
  se_col_data <- SummarizedExperiment::colData(hd)
  se_col_data$SampleID <- rownames(se_col_data)
  merge_se_data <- merge(merge_samplemap, gene_assay, by = "SampleID")
  merge_se_data <- merge(merge_se_data, se_col_data, by = c("USUBJID", "SampleID"))

  adtte_patients <- unique(adtte$USUBJID)
  se_patients <- merge_se_data$USUBJID

  patients_not_in_adtte <- setdiff(se_patients, adtte_patients)
  if (length(patients_not_in_adtte) > 0) {
    warn_message <- paste(
      "Patients", paste(patients_not_in_adtte, collapse = ", "),
      "removed from MAE because not contained in ADTTE."
    )
    if (is.null(getDefaultReactiveDomain())) {
      warning(warn_message)
    } else {
      showNotification(warn_message, type = "warning")
    }
  }

  # Now do the inner join.
  cols_to_take_from_col_data <- setdiff(names(se_col_data), "USUBJID")
  adtte_reduced <- adtte[, - which(names(adtte) %in% cols_to_take_from_col_data)]
  merged_adtte <- merge(adtte_reduced, merge_se_data, by = "USUBJID")
  merged_adtte <- tern::df_explicit_na(
    merged_adtte,
    char_as_factor = TRUE,
    logical_as_factor = FALSE  # TODO sometimes we want this sometimes not...
  )

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
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' mae <- hermes::multi_assay_experiment
#' adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'   dplyr::mutate(CNSR = as.logical(CNSR))
#'
#' data <- teal_data(
#'   dataset(
#'     "ADTTE",
#'     adtte,
#'     code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'       dplyr::mutate(CNSR = as.logical(CNSR))'
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
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_km <- function(label,
                    adtte_name,
                    mae_name,
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
      exclude_assays = exclude_assays,
      summary_funs = summary_funs
    ),
    ui = ui_g_km,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
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
                    mae_name,
                    summary_funs,
                    pre_output,
                    post_output) {

  ns <- NS(id)

  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      selectizeInput(ns("paramcd"), "Select Endpoint", choices = ""),
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
                     summary_funs,
                     exclude_assays) {
  experiment <- experimentSpecServer(
    "experiment",
    datasets = datasets,
    mae_name = mae_name,
    sample_vars_as_factors = FALSE  # To avoid converting logical `event` to factor.
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

  # When the gene changes, post process ADTTE.
  adtte_data <- reactive({
    # Resolve all reactivity
    experiment_data <- strata$experiment_data()
    experiment_name <- experiment$name()
    assay <- assay()
    genes <- genes()

    validate(need(genes$get_genes(), "please select at least one gene"))
    req(
      genes$returns_vector(),
      experiment_name,
      assay
    )

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    adtte <- datasets$get_data(adtte_name, filtered = TRUE)
    adtte$CNSR <- as.logical(adtte$CNSR)

    mae[[experiment_name]] <- experiment_data
    h_km_mae_to_adtte(
      adtte,
      mae,
      genes = genes,
      experiment_name = experiment_name,
      assay_name = assay
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

  output$km_plot <- renderPlot({
    endpoint <- input$paramcd
    strata_var <- strata$sample_var()
    percentiles <- input$percentiles
    adtte_data <- adtte_data()

    # Require endpoint for plot to generate.
    req(endpoint)

    # Validate that adtte_data is not empty.
    validate(need(
      nrow(adtte_data) > 0,
      "ADTTE is empty - please relax the filter criteria"
    ))

    # We need the gene counts column name (the selected gene_var/x_var) to add to the 'arm'
    # variable in the list.
    arm_name <- attr(adtte_data, "gene_cols")
    adtte_data[, arm_name] <- as.numeric(adtte_data[, arm_name])
    adtte_data <- dplyr::filter(adtte_data, .data$PARAMCD == endpoint)
    adtte_data <- droplevels(adtte_data)

    percentiles_without_borders <- setdiff(percentiles, c(0, 1))
    validate(need(
      length(percentiles_without_borders) > 0,
      "Please select at least one quantile other than 0 and 1"
    ))

    binned_adtte <- tryCatch({
      dplyr::mutate(
        adtte_data,
        gene_factor = tern::cut_quantile_bins(
          adtte_data[, arm_name],
          probs = percentiles_without_borders
        )
      )},
      error = function(e) {
        if (grepl("Duplicate quantiles produced", e)) {
          validate("please select (slightly) different quantiles to avoid duplicate quantiles")
        } else {
          stop(e)
        }
      }
    )

    variables <- list(
      tte = "AVAL",
      is_event = "CNSR",
      arm = "gene_factor",
      strat = strata_var
    )
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
