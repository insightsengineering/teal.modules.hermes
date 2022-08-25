library(teal.modules.hermes)

ui <- function(id,
               data) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = div(
      experimentSpecInput(ns("experiment"), data = data, mae_name = "MAE"),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), funs = list(Mean = colMeans)),
      adtteSpecInput(ns("adtte"))
    ),
    output = verbatimTextOutput(ns("summary"))
  )
}

server <- function(id,
                   data) {
  moduleServer(id, function(input, output, session) {
    experiment <- experimentSpecServer(
      "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
      mae_name = "MAE"
    )
    assay <- assaySpecServer(
      "assay",
      assays = experiment$assays
    )
    genes <- geneSpecServer(
      "genes",
      funs = list(Mean = colMeans),
      gene_choices = experiment$genes
    )
    adtte <- adtteSpecServer(
      "adtte",
      data = data,
      adtte_name = "ADTTE",
      mae_name = "MAE",
      adtte_vars = list(
        aval = "AVAL",
        avalu = "AVALU",
        is_event = "is_event",
        paramcd = "PARAMCD",
        usubjid = "USUBJID"
      ),
      experiment_data = experiment$data,
      experiment_name = experiment$name,
      assay = assay,
      genes = genes,
      probs = reactive({
        0.5
      }) # nolint
    )
    output$summary <- renderPrint({
      binned_adtte_subset <- adtte$binned_adtte_subset()
      summary(binned_adtte_subset)
    })
  })
}

my_app <- function() {
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
      teal::module(
        label = "adtteSpec example",
        server = server,
        ui = ui,
        filters = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
