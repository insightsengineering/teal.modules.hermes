source("https://raw.github.roche.com/gist/sabanesd/0e839ca7d4920fab342d8ed4b9d668fc/raw/eeaf752448e1b07641cc0c2a3af2172af7b99c94/install_nest.R")
install_nest("teal", "1185_dataset_specific_filter_panel")

library(teal)
library(teal.devel)
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
  
  standard_layout(
    output = white_small_well(
      tabsetPanel(
        tabPanel(title = "MAE", verbatimTextOutput(outputId = ns("col_data_table")))
      )
    )
  )
}
srv_made_up_merge_pr <- function(input, output, session, datasets, dataname) {
  
  output$col_data_table <- renderText({
    MAE <- datasets$get_data(dataname, filtered = TRUE)
    paste(capture.output(print(MAE)), collapse = "\n")
  })
}

MAE <- multi_assay_experiment # from hermes
mae <- dataset("MAE", MAE)
data <- teal_data(mae)

app <- init(
  data = data,
  modules = root_modules(
    static = {
      tm_made_up_merge_pr(
        label = "static",
        dataname = "MAE"
      )
    }
  )
)

shinyApp(app$ui, app$server)
