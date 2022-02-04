tm_made_up_merge_pr <- function(
    label = "PR merge",
    info = NULL,
    dataname = NULL,
    pre_output = NULL,
    post_output = NULL
  ) {

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
      verbatimTextOutput(outputId = ns("filter_expr")),
      tabsetPanel(
        tabPanel(title = "MAE", verbatimTextOutput(outputId = ns("col_data_table"))),
        tabPanel(title = "ADSL", verbatimTextOutput(outputId = ns("adsl_data_table")))
      )
    ),
    encoding = div(
      optionalSelectInput(
        inputId = "select_assay",
        label = "Select assay",
        choices = c("a", "b", "c")
      ),
      optionalSelectInput(
        inputId = "select_patient",
        label = "Select patient",
        choices = 1:10
      ),
      optionalSelectInput(
        inputId = "select_column",
        label = "Select column",
        choices = 1:10
      )
    ),
    forms = div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%"))
  )
}
srv_made_up_merge_pr <- function(input, output, session, datasets, dataname) {
  init_chunks()

  output$filter_expr <- renderText({
    paste(
      c(
        as.character(datasets$get_call(dataname)),
        as.character(datasets$get_call("ADSL"))
      ),
      collapse = "\n"
    )
  })

  output$col_data_table <- renderText({
    mae <- datasets$get_data(dataname, filtered = TRUE)
    chunks_reset()
    chunks_push(bquote({
      paste(capture.output(print(mae)), collapse = "\n")
    }))
    chunks_safe_eval()
  })

  output$adsl_data_table <- renderText({
    adsl <- datasets$get_data("ADSL", filtered = TRUE)
    chunks_reset()
    chunks_push(bquote({
      paste(capture.output(str(adsl)), collapse = "\n")
    }))
    chunks_safe_eval()
  })


  observeEvent(input$show_rcode, {

    show_rcode_modal(
      title = "R Code for MAE analysis",
      rcode = get_rcode(
        datasets = datasets,
        title = "",
        description = ""
      )
    )
  })
}

library(teal)
library(teal.devel)
library(hermes)
library(random.cdisc.data)
adsl <- cdisc_dataset("ADSL", radsl(cached = TRUE, na_percentage = 0.2)) %>%
  mutate_dataset(
    "ADSL$SEX[1:20] <- NA
    ADSL$AGE[21:30] <- Inf
    ADSL$AGE[31:40] <- NaN
    ADSL$EOSDT[51:60] <- NA
    ADSL$EOSDT[71:70] <- NA
    ADSL$all_na <- NA
    ADSL$unknown <- as.list(ADSL$SEX)"
  )
adtte <- cdisc_dataset("ADTTE", radtte(cached = TRUE))  %>%
  mutate_dataset(
    "ADTTE$CNSR <- as.logical(ADTTE$CNSR)
    ADTTE$CNSR[100:110] <- NA"
  )


mae <- multi_assay_experiment # from hermes

data <- cdisc_data(dataset("MAE", mae), adsl, adtte) %>%
  mutate_join_keys("MAE", "MAE", c("STUDYID", "USUBJID"))

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
