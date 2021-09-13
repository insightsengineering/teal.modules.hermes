# teal.modules.hermes: Teal Modules for RNAseq Data Analysis

Welcome to `teal.modules.hermes`! Jump right into an ad-hoc module example or read a bit more about what is what, i.e. how the pieces fit together.

## Ad-hoc module example

Let's assume you have a function `awesome_plot()` which takes a count matrix and makes an awesome plot out of it. Now you would like to make a Shiny app where you can filter patients, samples, select the experiment out of your `MultiAssayExperiment` (MAE), select the count matrix from the experiment, etc. 
Nothing is easier than that with `teal.modules.hermes`! 
We show you below how to quickly spin up your UI, server and put them together into a nice little app.

### UI function

In `teal.modules.hermes` we provide modules that make the experiment and assay
selection super easy, see here for the UI part:

```r
ui <- function(id,
               datasets,
               mae_name) {
  ns <- NS(id)

  teal.devel::standard_layout(
    encoding = div(
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay"))
    ),
    output = plotOutput(ns("awesome_plot"))
  )
}
```

### Server function

Similary for the server we use the modules, and call then our awesome plotting function.

```r
srv <- function(input,
                output,
                session,
                datasets,
                mae_name) {
  experiment <- experimentSpecServer(
    "experiment",
    datasets = datasets,
    mae_name = mae_name,
    name_annotation = NULL  # If you have a gene name column in your rowData, can specify here.
  )
  assay <- assaySpecServer("assay", experiment$assays)
  output$awesome_plot <- renderPlot({
    data <- experiment$data()
    assay <- assay()
    counts <- SummarizedExperiment::assay(data, assay)
    awesome_plot(counts)
  })
}
```

### App function

Now let's assume you want to spin up your app for an MAE. 

```r
library(teal.modules.hermes)
awesome_app <- function(mae, label = "My awesome app") {
  mae_name <- "MAE"
  for (i in seq_along(mae)) {
    mae[[i]] <- hermes::HermesData(mae[[i]])
  }
  mae_data <- dataset(mae_name, mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      module(
        label = label,
        server = srv,
        server_args = list(mae_name = mae_name),
        ui = ui,
        ui_args = list(mae_name = mae_name),
        filters = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
```

### Testing it

Let's test this.

```r
awesome_plot <- image
awesome_app(hermes::multi_assay_experiment)
```

## What is what

### What is `teal`?

`teal` is a shiny-based interactive exploration framework for analyzing clinical trials data. `teal` currently provides a dynamic filtering facility and diverse data viewers. `teal` shiny applications are built using standard [shiny modules](https://shiny.rstudio.com/articles/modules.html).
See [github](https://github.com/insightsengineering/teal) and [training material](http://pdwebdev01.gene.com/groups/devo/multimedia/Gen_Teal/story_html5.html?lms=1) for more details. 

### What is `hermes`?

`hermes` facilitates preprocessing, analyzing, and reporting of RNA-seq data. 
The core functionality is built on the BioConductor ecosystem, especially the `SummarizedExperiment` class from which the `HermesData` class inherits.
See the [vignette](https://docs.nest.roche.com/releases/2021_07_07/embedded/hermes/articles/introduction.html) for more details.

### So what is then `teal.modules.hermes`?

`teal.modules.hermes` provides `teal` modules (which can be used as part of any `teal` app), for interactive RNA-seq data analysis using `hermes`. Again it is heavily built on the BioConductor classes, in particular `MultiAssayExperiment` which is expected to contain the `HermesData` experiments.
