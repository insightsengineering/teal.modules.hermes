tm_made_up_merge_pr <- function(label = "PR merge",
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
    datanames = "all"
  )
}
ui_made_up_merge_pr <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      verbatimTextOutput(outputId = ns("filter_expr")),
      tabsetPanel(
        tabPanel(title = "MAE", verbatimTextOutput(outputId = ns("col_data_table"))),
        tabPanel(title = "ADSL", verbatimTextOutput(outputId = ns("adsl_data_table")))
      )
    ),
    encoding = div(
      teal.widgets::optionalSelectInput(
        inputId = "select_assay",
        label = "Select assay",
        choices = c("a", "b", "c")
      ),
      teal.widgets::optionalSelectInput(
        inputId = "select_patient",
        label = "Select patient",
        choices = 1:10
      ),
      teal.widgets::optionalSelectInput(
        inputId = "select_column",
        label = "Select column",
        choices = 1:10
      )
    ),
    forms = div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")
    )
  )
}
srv_made_up_merge_pr <- function(input, output, session, datasets, dataname) {
  teal.code::init_chunks()

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
    teal.code::chunks_reset()
    teal.code::chunks_push(bquote({
      paste(capture.output(print(mae)), collapse = "\n")
    }))
    teal.code::chunks_safe_eval()
  })

  output$adsl_data_table <- renderText({
    adsl <- datasets$get_data("ADSL", filtered = TRUE)
    teal.code::chunks_reset()
    teal.code::chunks_push(bquote({
      paste(capture.output(str(adsl)), collapse = "\n")
    }))
    teal.code::chunks_safe_eval()
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
library(hermes)
library(random.cdisc.data)
adsl = radsl(cached = TRUE, na_percentage = 0.2)
adsl$SEX[1:20] <- NA
adsl$AGE[21:30] <- Inf
adsl$AGE[31:40] <- NaN
adsl$EOSDT[51:60] <- NA
adsl$EOSDT[71:70] <- NA
adsl$all_na <- NA
adsl$unknown <- as.list(ADSL$SEX)

adtte = radtte(cached = TRUE)
adtte$CNSR <- as.logical(ADTTE$CNSR)
adtte$CNSR[100:110] <- NA

mae <- multi_assay_experiment # from hermes

data <- cdisc_data(MAE = mae, ADSL = adsl, ADTTE = adtte)
datanames <- datanames(data)
join_keys(data) <- cdisc_join_keys(!!!datanames)
data@join_keys$mutate(
  "MAE", "MAE", c("STUDYID", "USUBJID")
)

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
