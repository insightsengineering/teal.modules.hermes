# teal.modules.hermes: Teal Modules for RNAseq Data Analysis

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering//actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering//actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering//actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io//)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering//main?color=purple\&label=package%20version)](https://github.com/insightsengineering//tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/?color=red\&label=open%20issues)](https://github.com/insightsengineering//issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

# teal.modules.hermes: Teal Modules for RNAseq Data Analysis

<!-- start badges -->
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/teal.modules.hermes/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/teal.modules.hermes/_xml_coverage_reports/data/main/coverage.xml)
<!-- end badges -->

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
               data,
               mae_name) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = div(
      experimentSpecInput(ns("experiment"), data, mae_name),
      assaySpecInput(ns("assay"))
    ),
    output = plotOutput(ns("awesome_plot"))
  )
}
```

### Server function

Similarly for the server we use the modules, and call then our awesome plotting function.

```r
srv <- function(input,
                output,
                session,
                data,
                filter_panel_api,
                mae_name) {
  experiment <- experimentSpecServer(
    "experiment",
    data = data,
    filter_panel_api = filter_panel_api,
    mae_name = mae_name,
    name_annotation = NULL  # If you have a gene name column in your rowData, can specify here.
  )
  assay <- assaySpecServer("assay", experiment$assays)
  output$awesome_plot <- renderPlot({
    data <- experiment$data()
    assay <- assay()
    req(assay %in% SummarizedExperiment::assayNames(data))
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
  mae <- hermes::lapply(mae, hermes::HermesData)
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
        filters = mae_name
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
See [github](https://insightsengineering.github.io/teal) for more details.

### What is `hermes`?

`hermes` facilitates preprocessing, analyzing, and reporting of RNA-seq data.
The core functionality is built on the BioConductor ecosystem, especially the `SummarizedExperiment` class from which the `HermesData` class inherits.
See the [vignette](https://insightsengineering.github.io/hermes/articles/hermes.html) for more details.

### So what is then `teal.modules.hermes`?

`teal.modules.hermes` provides `teal` modules (which can be used as part of any `teal` app), for interactive RNA-seq data analysis using `hermes`. Again it is heavily built on the BioConductor classes, in particular `MultiAssayExperiment` (MAE) which is expected to contain the `HermesData` experiments.

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.modules.hermes@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.modules.hermes.svg)](https://starchart.cc/insightsengineering/teal.modules.hermes)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.modules.hermes](https://reporoster.com/stars/insightsengineering/teal.modules.hermes)](https://github.com/insightsengineering/teal.modules.hermes/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.modules.hermes](https://reporoster.com/forks/insightsengineering/teal.modules.hermes)](https://github.com/insightsengineering/teal.modules.hermes/network/members)
