% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleVarSpec.R
\name{sampleVarSpecServer}
\alias{sampleVarSpecServer}
\title{Module Server for Sample Variable Specification}
\usage{
sampleVarSpecServer(
  id,
  experiment_name,
  original_data,
  transformed_data = original_data,
  assign_lists = reactiveValues(),
  num_levels = NULL,
  categorical_only = !is.null(num_levels),
  explicit_na = FALSE,
  label_modal_title = "Please click to group the original factor levels"
)
}
\arguments{
\item{id}{(\code{string}) the shiny module id.}

\item{experiment_name}{(reactive \code{string})\cr name of the input experiment.}

\item{original_data}{(reactive \code{SummarizedExperiment})\cr input experiment where the
sample variables extracted via \code{\link[SummarizedExperiment:SummarizedExperiment-class]{SummarizedExperiment::colData()}} should be eligible for
selection.}

\item{transformed_data}{(reactive \code{SummarizedExperiment})\cr used when multiple sample
variables can be selected in the app. In that case, pass here the pre-transformed data.}

\item{assign_lists}{(\code{reactivevalues})\cr object to share factor level groupings across multiple
sample variables.}

\item{num_levels}{(\code{count} or \code{NULL})\cr required number of levels after combining original levels.
If \code{NULL} then all numbers of levels are allowed.}

\item{categorical_only}{(\code{flag})\cr whether only categorical variables should be selected
from.}

\item{explicit_na}{(\code{flag})\cr whether the \code{colData} of \code{original_data} will be transformed with
\link[hermes:h_df_factors_with_explicit_na]{hermes::h_df_factors_with_explicit_na} before further processing. That means also that
\code{NA} will be made an explicit factor level and counted for \code{num_levels}.}

\item{label_modal_title}{(\code{string})\cr title for the dialog that asks for the text input.}
}
\value{
Reactive \code{\link[SummarizedExperiment:SummarizedExperiment-class]{SummarizedExperiment::SummarizedExperiment}} which can be used as
input for the relevant \code{hermes} functions.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This defines the server part for the sample variable specification.
}
\note{
Only atomic columns (e.g. not \code{DataFrame} columns) of the \code{colData}
which are not completely missing (\code{NA}) will be shown for selection.
If \code{num_levels} is specified then only factor columns will be available.
}
\examples{
ui <- function(id) {
  checkmate::assert_class(data, "teal_data")
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = uiOutput(ns("encoding_ui")),
    output = plotOutput(ns("plot"))
  )
}
server <- function(id,
                   data) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    output$encoding_ui <- renderUI({
      mae <- data()[["MAE"]]
      experiment_name_choices <- names(mae)
      tags$div(
        selectInput(session$ns("experiment_name"), "Select experiment", experiment_name_choices),
        sampleVarSpecInput(session$ns("facet_var"), "Select faceting variable")
      )
    })
    experiment_data <- reactive({
      req(input$experiment_name)
      mae <- data()[["MAE"]]
      object <- mae[[input$experiment_name]]
      SummarizedExperiment::colData(object) <-
        hermes::df_cols_to_factor(SummarizedExperiment::colData(object))
      object
    })
    facet_var_spec <- sampleVarSpecServer(
      "facet_var",
      experiment_name = reactive({
        input$experiment_name
      }),
      original_data = experiment_data
    )
    output$plot <- renderPlot({
      experiment_data_final <- facet_var_spec$experiment_data()
      facet_var <- facet_var_spec$sample_var()
      hermes::draw_boxplot(
        experiment_data_final,
        assay_name = "counts",
        genes = hermes::gene_spec(hermes::genes(experiment_data_final)[1]),
        facet_var = facet_var
      )
    })
  })
}
my_app <- function() {
  data <- teal_data(MAE = hermes::multi_assay_experiment)
  app <- init(
    data = data,
    modules = modules(
      module(
        label = "sampleVarSpec example",
        server = server,
        ui = ui,
        datanames = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
if (interactive()) {
  my_app()
}
}
\seealso{
\code{\link[=sampleVarSpecInput]{sampleVarSpecInput()}} for the module UI.
}
