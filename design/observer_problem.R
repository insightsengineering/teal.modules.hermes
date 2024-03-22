library(teal)
library(hermes)


tm_g_scatterplot <- function(label,
                             mae_name,
                             pre_output = NULL,
                             post_output = NULL) {
  module(
    label = label,
    server = srv_g_scatterplot,
    server_args = list(
      mae_name = mae_name
    ),
    ui = ui_g_scatterplot,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    datanames = "all"
  )
}

#' @describeIn tm_g_scatterplot sets up the user interface.
#' @inheritParams module_arguments
ui_g_scatterplot <- function(id,
                             datasets,
                             mae_name,
                             pre_output,
                             post_output) {
  ns <- NS(id)
  experiment_name_choices <- c("a", "b", "c")

  teal.widgets::standard_layout(
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices, experiment_name_choices[1]),
      selectizeInput(ns("x_var"), "Select x gene", choices = ""),
      selectizeInput(ns("y_var"), "Select y gene", choices = "")
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_scatterplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @importFrom SummarizedExperiment colData
#' @importFrom hermes draw_scatterplot
srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              mae_name) {
  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_name <- reactive({
    # if we put this req() then we are fine:
    req(input$experiment_name)
    res <- input$experiment_name
    stopifnot(!is.null(res))
    res
  })

  # When the genes are recomputed, update the choices for genes in the UI.
  observeEvent(experiment_name(), ignoreInit = TRUE, {
    experiment_name()
  })
  # Problem: If I add this observer in, then I jump into the reactive above when the input is not yet initialized. Why?

  output$plot <- renderPlot({
    # First: resolve all reactivity.
    my_experiment_name <- experiment_name()
    x_var <- input$x_var
    y_var <- input$y_var
    assay_name <- input$assay_name

    plot(1:5, main = my_experiment_name, xlab = assay_name)
  })
}

mae <- hermes::multi_assay_experiment
for (i in seq_along(mae)) {
  mae[[i]] <- HermesData(mae[[i]])
}

data <- teal_data(dataset("MAE", mae))
app <- init(
  data = data,
  modules = modules(
    static = {
      tm_g_scatterplot(
        label = "scatterplot",
        mae_name = "MAE"
      )
    }
  )
)

shinyApp(app$ui, app$server)
