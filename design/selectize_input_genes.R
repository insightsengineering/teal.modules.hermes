library(shiny)

# choices: 97,310 names
set.seed(123)
baby_names <- replicate(n = 97310, paste(sample(letters, size = 10), collapse = ""))
baby_names <- setNames(baby_names, seq_along(baby_names))

# ui
ui <- fluidPage(
  selectizeInput(
    inputId = "ID",
    label = "Select Something",
    choices = NULL,
    multiple = TRUE,
    selected = 1,
    options = list(
      render = I("{
        option: function(item, escape) { return '<div>' + item.value + '-' + item.label + '</div>'; }
      }"),
      searchField = c("value", "label")
    )
  )
)
# server
server <- function(input, output, session) {
  updateSelectizeInput(
    session = session,
    inputId = "ID",
    choices = baby_names,
    server = TRUE
  )
}
# app
shinyApp(ui = ui, server = server)
