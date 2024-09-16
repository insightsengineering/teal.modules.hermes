# teal.modules.hermes: Teal Modules for RNAseq Data Analysis

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/teal.modules.hermes/actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering/teal.modules.hermes/actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering/teal.modules.hermes/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.modules.hermes/latest-tag/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.modules.hermes/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/teal.modules.hermes/_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.modules.hermes?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.modules.hermes?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.modules.hermes)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.modules.hermes)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.modules.hermes)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.modules.hermes)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.modules.hermes)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.modules.hermes)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.modules.hermes/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.modules.hermes/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.modules.hermes?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.modules.hermes/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

## What is what

### What is `teal`?

`teal` is a shiny-based interactive exploration framework for analyzing clinical trials data. `teal` currently provides a dynamic filtering facility and diverse data viewers. `teal` shiny applications are built using standard [shiny modules](https://shiny.rstudio.com/articles/modules.html).
See [`teal` page](https://insightsengineering.github.io/teal/) for more details.

### What is `hermes`?

`hermes` facilitates preprocessing, analyzing, and reporting of RNA-seq data.
The core functionality is built on the `BioConductor` ecosystem, especially the `SummarizedExperiment` class from which the `HermesData` class inherits.
See the [vignette](https://insightsengineering.github.io/hermes/articles/hermes.html) for more details.

### So what is then `teal.modules.hermes`?

`teal.modules.hermes` provides `teal` modules (which can be used as part of any `teal` app), for interactive RNA-seq data analysis using `hermes`. Again it is heavily built on the `BioConductor` classes, in particular `MultiAssayExperiment` (MAE) which is expected to contain the `HermesData` experiments.

## Installation

```r
# stable versions
# install.packages("pak")
pak::pkg_install("insightsengineering/teal.modules.hermes@*release")

# beta versions
# install.packages("pak")
pak::pkg_install("insightsengineering/teal.modules.hermes")
```

See package vignettes `browseVignettes(package = "teal.modules.hermes")` for usage of this package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.modules.hermes.svg)](https://starchart.cc/insightsengineering/teal.modules.hermes)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.modules.hermes](https://reporoster.com/stars/insightsengineering/teal.modules.hermes)](https://github.com/insightsengineering/teal.modules.hermes/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.modules.hermes](https://reporoster.com/forks/insightsengineering/teal.modules.hermes)](https://github.com/insightsengineering/teal.modules.hermes/network/members)
