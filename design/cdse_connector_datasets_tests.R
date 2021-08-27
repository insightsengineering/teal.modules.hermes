# Test toy example from issue #41.

library(teal)
library(teal.modules.hermes)
library(hermes)
#library(magrittr)

data <- cdse_data(
  connection = cdse_connection("demo"),
  cdse_dataset_connector(
    dataname = "MAE",
    cid = "cid6828341065561714688"
  ) #%>% mutate_dataset("MAE[[1]] <- HermesData(MAE[[1]])")
)

app <- init(
  data = teal_data(data),
  root_modules(
    teal.modules.hermes::tm_g_barplot("barplot", "MAE")
  )
)

shinyApp(app$ui, app$server)

# Accessing CDSE datasets: https://github.roche.com/sabanesd/nest/blob/master/mae/cdse.R

# # CDISC data set (ADTTE) with existing teal.module.clinical (tm_)
library(teal)
library(teal.modules.hermes)
library(teal.modules.clinical)
library(hermes)
library(CDSE)
cdse_login()
cdse_tokens()
cdse_get_environment()
cdse_set_environment("prod")

# Using Asthma Data Pilot -- Pooled Integrated Datasets -- ADAM ADSL from https://cdse.roche.com/details/cid6736608260612358144
dat_single <- cdse_read_dataset("http://clinical.roche.com/dataset/cid6736608260612358144")

char_vars_adsl <- names(Filter(isTRUE, sapply(dat_single, is.character)))
app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL",
                  dat_single)
  ),
  root_modules(
    teal.modules.clinical::tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(c("ACTARM", "ACTARMCD"), "ACTARMCD"),
      summarize_vars = choices_selected(c("SAFFL", "DTHFL", "AAGE"), c("SAFFL")),
      add_total = TRUE,
      useNA = c("ifany"),
      na_level = "<Missing>",
      denominator = c("N", "n", "omit"),
      drop_arm_levels = TRUE,
      pre_output = NULL,
      post_output = NULL
    )
  )
)

shinyApp(app$ui, app$server)

# Using Asthma Data Pilot -- Pooled Integrated Datasets - ADaM ADZB from https://cdse.roche.com/details/cid6736609907921174529
dat_single <- cdse_read_dataset("http://clinical.roche.com/dataset/cid6736609908885864448")

char_vars_adsl <- names(Filter(isTRUE, sapply(dat_single, is.character)))
app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL",
                  dat_single)
  ),
  root_modules(
    teal.modules.clinical::tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(c("ACTARM", "ACTARMCD", "ARM"), "ACTARMCD"),
      summarize_vars = choices_selected(c("SAFFL", "AGE"), c("SAFFL")),
      add_total = TRUE,
      useNA = c("ifany"),
      na_level = "<Missing>",
      denominator = c("N", "n", "omit"),
      drop_arm_levels = TRUE,
      pre_output = NULL,
      post_output = NULL
    )
  )
)

shinyApp(app$ui, app$server)


# Test on CDSE MAE data from https://cdse.roche.com/details/cid6740627690476584961

data <- cdse_data(
  connection = cdse_connection("prod"),
  cdse_dataset_connector(
    dataname = "MAE",
    cid = "cid6740627690476584961"
  ) %>% mutate_dataset("MAE[[1]] <- HermesData(MAE[[1]])")
)

app <- init(
  data = teal_data(data),
  root_modules(
    teal.modules.hermes::tm_g_barplot("barplot", "MAE")
  )
)

shinyApp(app$ui, app$server)

