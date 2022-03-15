# Install teal
remotes::install_github(
  "insightsengineering/teal",
  ref = "1185_dataset_specific_filter_panel",
  upgrade = "never",
  force = TRUE
)

library(teal)
library(dplyr)
library(ggplot2)
library(random.cdisc.data)
library(hermes)

# 2) Try simplest example.
tm_made_up_merge_pr <- function(label = "Simple MAE module",
                                info = NULL,
                                dataname = NULL,
                                pre_output = NULL,
                                post_output = NULL) {
  args <- as.list(environment())
  module(
    label = label,
    server = srv_made_up_merge_pr,
    ui = ui_made_up_merge_pr,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = "all"
  )
}

ui_made_up_merge_pr <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tabsetPanel(
        tabPanel(title = "MAE", verbatimTextOutput(outputId = ns("col_data_table")))
      )
    )
  )
}
srv_made_up_merge_pr <- function(input, output, session, datasets, dataname) {

  output$col_data_table <- renderText({
    mae <- datasets$get_data(dataname, filtered = TRUE)
    paste(capture.output(print(mae)), collapse = "\n")
  })
}

mae <- multi_assay_experiment # from hermes
data <- teal_data(dataset("MAE", mae))

app <- init(
  data = data,
  modules = modules(
    static = {
      tm_made_up_merge_pr(
        label = "static",
        dataname = "MAE"
      )
    }
  )
)

shinyApp(app$ui, app$server)
