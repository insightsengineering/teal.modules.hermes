#' Data Preprocessing for ADTTE Module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function to help with merging of MAE to `ADTTE`.
#'
#' @inheritParams function_arguments
#'
#' @return A data frame containing all columns/rows from `adtte` that match
#'   by subject ID with the row names of the MAE and have the gene samples available
#'   in the given experiment. The attribute `gene_cols` contains the column names
#'   for the gene columns.
#'
#' @note The final gene column names can start with a different string than
#'   the original gene IDs (or labels), in particular white space and colons are removed.
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
                              assay_name = "counts",
                              usubjid_var = "USUBJID") {
  assert_class(mae, "MultiAssayExperiment")
  assert_string(experiment_name)
  assert_string(usubjid_var)
  assert_names(names(adtte), must.include = usubjid_var)

  # Check subject ID across experiment, sample map, and MAE colData.
  mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
  samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
  sm_usubjid <- as.character(samplemap_experiment$primary)

  hd <- suppressWarnings(MultiAssayExperiment::getWithColData(mae, experiment_name))
  assert_class(hd, "AnyHermesData")
  hd_usubjid <- as.character(SummarizedExperiment::colData(hd)[[usubjid_var]])

  assert_subset(
    x = hd_usubjid,
    choices = sm_usubjid
  )

  mae_coldata <- MultiAssayExperiment::colData(mae)
  if (usubjid_var %in% colnames(mae_coldata)) {
    mae_usubjid <- as.character(mae_coldata[[usubjid_var]])
    assert_subset(
      x = sm_usubjid,
      choices = mae_usubjid
    )
  }

  gene_data <- hermes::col_data_with_genes(
    object = hd,
    assay_name = assay_name,
    genes = genes
  )
  merged_adtte <- hermes::inner_join_cdisc(
    gene_data = gene_data,
    cdisc_data = adtte,
    patient_key = usubjid_var
  )
  structure(
    merged_adtte,
    gene_cols = attr(gene_data, "gene_cols")
  )
}

#' Module Input for ADTTE Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the input for the ADTTE specification.
#'
#' @inheritParams module_arguments
#' @param label_paramcd (`string`)\cr label for the endpoint (`PARAMCD`) selection.
#'
#' @return The UI part.
#' @seealso [adtteSpecServer()] for the module server and a complete example.
#' @export
adtteSpecInput <- function(inputId,
                           label_paramcd = "Select Endpoint") {
  assert_string(inputId)
  assert_string(label_paramcd, min.chars = 1L)

  ns <- NS(inputId)

  selectInput(
    inputId = ns("paramcd"),
    label = label_paramcd,
    choices = ""
  )
}

#' Module Server for ADTTE Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the ADTTE specification.
#'
#' @inheritParams module_arguments
#' @param probs (reactive `numeric`)\cr probabilities to bin the gene or gene signature
#'   into.
#'
#' @return List with the following elements:
#'   - `binned_adtte_subset`: reactive containing the joined ADTTE and gene data.
#'   - `gene_col`: reactive containing the string with the column name of the original
#'        numeric gene variable.
#'   - `gene_factor`: string with the variable name for the binned gene data.
#'   - `time_unit`: reactive string with the time unit for the current subset.
#'
#' @seealso [adtteSpecInput()] for the module UI.
#'
#' @export
#'
#' @examples
#' ui <- function(id,
#'                datasets) {
#'   ns <- NS(id)
#'
#'   teal.devel::standard_layout(
#'     encoding = div(
#'       experimentSpecInput(ns("experiment"), datasets = datasets, mae_name = "MAE"),
#'       assaySpecInput(ns("assay")),
#'       geneSpecInput(ns("genes"), funs = list(Mean = colMeans)),
#'       adtteSpecInput(ns("adtte"))
#'     ),
#'     output = verbatimTextOutput(ns("summary"))
#'   )
#' }
#'
#' server <- function(input,
#'                    output,
#'                    session,
#'                    datasets) {
#'   experiment <- experimentSpecServer(
#'     "experiment",
#'     datasets = datasets,
#'     mae_name = "MAE"
#'   )
#'   assay <- assaySpecServer(
#'     "assay",
#'     assays = experiment$assays
#'   )
#'   genes <- geneSpecServer(
#'     "genes",
#'     funs = list(Mean = colMeans),
#'     gene_choices = experiment$genes
#'   )
#'   adtte <- adtteSpecServer(
#'     "adtte",
#'     datasets = datasets,
#'     adtte_name = "ADTTE",
#'     mae_name = "MAE",
#'     adtte_vars = list(
#'       aval = "AVAL",
#'       avalu = "AVALU",
#'       is_event = "is_event",
#'       paramcd = "PARAMCD",
#'       usubjid = "USUBJID"
#'     ),
#'     experiment_data = experiment$data,
#'     experiment_name = experiment$name,
#'     assay = assay,
#'     genes = genes,
#'     probs = reactive({0.5})
#'   )
#'   output$summary <- renderPrint({
#'     binned_adtte_subset <- adtte$binned_adtte_subset()
#'     summary(binned_adtte_subset)
#'   })
#' }
#'
#' my_app <- function() {
#'   mae <- hermes::multi_assay_experiment
#'   adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'     dplyr::mutate(is_event = .data$CNSR == 0)
#'
#'   data <- teal_data(
#'     dataset(
#'       "ADTTE",
#'       adtte,
#'       code = 'adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
#'         dplyr::mutate(is_event = .data$CNSR == 0)'
#'     ),
#'     dataset("MAE", mae)
#'   )
#'
#'   app <- init(
#'     data = data,
#'     modules = root_modules(
#'       module(
#'         label = "adtteSpec example",
#'         server = server,
#'         ui = ui,
#'         filters = "all"
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#'
#' if (interactive()) {
#'   my_app()
#' }
adtteSpecServer <- function(id,
                            datasets,
                            mae_name,
                            adtte_name,
                            adtte_vars,
                            experiment_data,
                            experiment_name,
                            assay,
                            genes,
                            probs) {
  assert_string(id)
  assert_r6(datasets)
  assert_string(mae_name)
  assert_string(adtte_name)
  assert_adtte_vars(adtte_vars)
  assert_reactive(experiment_data)
  assert_reactive(experiment_name)
  assert_reactive(assay)
  assert_reactive(genes)
  assert_reactive(probs)

  moduleServer(id, function(input, output, session) {

    # Join ADTTE with gene data.
    adtte_joined <- reactive({
      experiment_data <- experiment_data()
      experiment_name <- experiment_name()
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

      mae[[experiment_name]] <- experiment_data
      h_km_mae_to_adtte(
        adtte,
        mae,
        genes = genes,
        experiment_name = experiment_name,
        assay_name = assay,
        usubjid_var = adtte_vars$usubjid
      )
    })

    gene_col <- reactive({
      attr(adtte_joined(), "gene_cols")
    })

    # After joining, we recompute endpoints.
    paramcd_choices <- reactive({
      adtte_joined <- adtte_joined()
      unique(adtte_joined[[adtte_vars$paramcd]])
    })

    # Update the choices for endpoints in the UI.
    observeEvent(paramcd_choices(), {
      paramcd_choices <- paramcd_choices()

      updateSelectInput(
        session,
        "paramcd",
        choices = paramcd_choices,
        selected = paramcd_choices[1]
      )
    })

    # Subset zooming in on a specified endpoint.
    adtte_subset <- reactive({
      endpoint <- input$paramcd
      adtte_joined <- adtte_joined()

      req(endpoint)

      # Validate that adtte_data is not empty.
      validate(need(
        nrow(adtte_joined) > 0,
        "Joined ADTTE is empty - please relax the filter criteria"
      ))

      result <- adtte_joined[adtte_joined[[adtte_vars$paramcd]] == endpoint, , drop = FALSE]
      droplevels(result)
    })

    binned_adtte_subset <- reactive({
      gene_col <- gene_col()
      probs <- probs()
      adtte_subset <- adtte_subset()

      result <- tryCatch({
        dplyr::mutate(
          adtte_subset,
          gene_factor = tern::cut_quantile_bins(
            adtte_subset[, gene_col],
            probs = probs
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
      result
    })

    time_unit <- reactive({
      adtte_subset <- adtte_subset()
      result <- unique(as.character(adtte_subset[[adtte_vars$avalu]]))
      assert_string(result)
      result
    })

    list(
      binned_adtte_subset = binned_adtte_subset,
      gene_col = gene_col,
      gene_factor = "gene_factor",
      time_unit = time_unit
    )
  })
}