#' Data Preprocessing for KM Module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function to help with merging of MAE to `ADTTE` for use with `g_km`.
#'
#' @note We require that each patient only has one sample.
#'
#' @inheritParams function_arguments
#'
#' @return A data frame containing all columns/rows from `adtte` that match
#'   by `USUBJID` with the row names of the MAE and have the gene samples available
#'   in the given experiment. The attributes `sample_id`
#'   and `gene_cols` contain the column names for the sample ID and gene columns.
#'
#' @note The final gene column names can start with a different string than
#'   the original gene IDs, in particular white space, dots and colons are removed,
#'   see [tern::make_names()] for details.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' mae <- hermes::multi_assay_experiment
#' adtte <- radtte(cached = TRUE) %>%
#'   mutate(CNSR = as.logical(CNSR))
#'
#' new_adtte <- h_km_mae_to_adtte(
#'   adtte,
#'   mae,
#'   gene_var = "GeneID:1820",
#'   experiment_name = "hd2"
#' )
#' new_adtte2 <- h_km_mae_to_adtte(
#'   adtte,
#'   mae,
#'   gene_var = c("GeneID:1820", "GeneID:94115"),
#'   experiment_name = "hd2"
#' )
h_km_mae_to_adtte <- function(adtte,
                              mae,
                              gene_var,
                              experiment_name = "hd1",
                              assay_name = "counts") {
  assert_choice(
    assay_name,
    c("counts", "cpm", "rpkm", "tpm", "voom")
  )
  assert_character(gene_var)
  assert_string(experiment_name)

  mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
  samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
  patients_in_experiment <- samplemap_experiment$primary

  assert_character(patients_in_experiment, unique = TRUE)

  merge_samplemap <- samplemap_experiment[, c("primary", "colname")]
  merge_samplemap <- as.data.frame(merge_samplemap)
  colnames(merge_samplemap) <- c("USUBJID", "SampleID")

  hd <- mae[[experiment_name]]
  assert_class(hd, "AnyHermesData")

  num_genes <- length(gene_var)
  gene_assay <- SummarizedExperiment::assay(hd, assay_name)[gene_var,]
  gene_assay <- as.data.frame(gene_assay)
  gene_names <- tern::make_names(gene_var)
  merged_names <- paste(gene_names, assay_name, sep = "_")

  if (num_genes == 1){
    colnames(gene_assay) <- merged_names
    gene_assay$SampleID <- rownames(gene_assay)
  } else {
    rownames(gene_assay) <- merged_names
    gene_assay <- data.frame(t(gene_assay), SampleID = colnames(gene_assay))
  }

  merge_se_data <- merge(merge_samplemap, gene_assay, by = "SampleID")

  adtte_patients <- unique(adtte$USUBJID)
  se_patients <- merge_se_data$USUBJID
  assert_true(all(se_patients %in% adtte_patients))

  merged_adtte <- merge(adtte, merge_se_data, by = "USUBJID")
  merged_adtte <- tern::df_explicit_na(merged_adtte)

  structure(
    merged_adtte,
    sample_id = "SampleID",
    gene_cols = merged_names
  )
}

#' Template: Kaplan-Meier MAE
#'
#' @inheritParams template_arguments
#' @inheritParams tern::g_km
#' @inheritParams tern::control_coxreg
#' @param facet_var (`character`)\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
#' @seealso [tm_g_km()]
#'
#' @importFrom grid grid.newpage grid.layout viewport pushViewport
template_g_km_mae <- function(dataname = "ANL",
                          arm_var = "GENE",
                          quantiles = c(0.33, 0.66),
                          ref_arm = NULL,
                          comp_arm = NULL,
                          compare_arm = FALSE,
                          combine_comp_arms = FALSE,
                          aval_var = "AVAL",
                          cnsr_var = "CNSR",
                          xticks = NULL,
                          strata_var = NULL,
                          time_points = NULL,
                          facet_var = "SEX",
                          font_size = 8,
                          conf_level = 0.95,
                          ties = "efron",
                          xlab = "Survival time",
                          time_unit_var = "AVALU",
                          yval = "Survival",
                          pval_method = "log-rank",
                          annot_surv_med = TRUE,
                          annot_coxph = TRUE,
                          ci_ribbon = FALSE,
                          title = "KM Plot") {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(arm_var),
    tern::is_proportion_vector(quantiles, include_boundaries = TRUE),
    assertthat::is.string(aval_var),
    assertthat::is.string(cnsr_var),
    assertthat::is.string(time_unit_var),
    assertthat::is.flag(compare_arm),
    assertthat::is.flag(combine_comp_arms),
    is.null(xticks) | is.numeric(xticks),
    assertthat::is.string(title)
  )

  ref_arm_val <- paste(ref_arm, collapse = "/")
  y <- list()

  preprocessing_list <- list()

  preprocessing_list <- add_expr(
    preprocessing_list,
    substitute(
      expr = dataname,
      env = list(dataname = as.name(dataname))
    )
  )

  preprocessing_list <- add_expr(
    preprocessing_list,
    utils.nest::substitute_names(
      expr = dplyr::mutate(arm_var = tern::cut_quantile_bins(arm_var, probs = quantiles)),
      names = list(arm_var = as.name(arm_var)),
      others = list(quantiles = quantiles)
    )
  )

  y$preprocessing <- substitute(
    expr = {
      anl <- preprocessing_pipe
    },
    env = list(
      preprocessing_pipe = pipe_expr(preprocessing_list)
    )
  )

  data_list <- list()

  data_list <- add_expr(
    data_list,
    prepare_arm(
      dataname = "anl",
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      compare_arm = compare_arm,
      ref_arm_val = ref_arm_val
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = dplyr::mutate(
        is_event = cnsr_var == 0
      ),
      env = list(
        anl = as.name(dataname),
        cnsr_var = as.name(cnsr_var)
      )
    )
  )

  if (compare_arm && combine_comp_arms) {
    comp_arm_val <- paste(comp_arm, collapse = "/")
    data_list <- add_expr(
      data_list,
      utils.nest::substitute_names(
        expr = dplyr::mutate(arm_var = tern::combine_levels(arm_var, levels = comp_arm, new_level = comp_arm_val)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm, comp_arm_val = comp_arm_val)
      )
    )
  }

  y$data <- substitute(
    expr = {
      anl <- data_pipe
    },
    env = list(
      data_pipe = pipe_expr(data_list)
    )
  )

  y$variables <- if (!is.null(strata_var) && length(strata_var) != 0) {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm, strat = strata_var),
      env = list(tte = aval_var, arm = arm_var, strata_var = strata_var)
    )
  } else {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm),
      env = list(tte = aval_var, arm = arm_var)
    )
  }
  graph_list <- list()

  if (!is.null(facet_var) && length(facet_var) != 0) {
    graph_list <- add_expr(
      graph_list,
      quote(grid::grid.newpage())
    )
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = lyt <- grid::grid.layout(nrow = nlevels(df$facet_var), ncol = 1) %>%
          grid::viewport(layout = .) %>%
          grid::pushViewport(),
        env = list(
          df = as.name(dataname),
          facet_var = as.name(facet_var)
        )
      )
    )

    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          result <- mapply(
            df = split(anl, f = anl$facet_var), nrow = seq_along(levels(anl$facet_var)),
            FUN = function(df_i, nrow_i) {
              if (nrow(df_i) == 0) {
                grid::grid.text(
                  "No data found for a given facet value.",
                  x = 0.5,
                  y = 0.5,
                  vp = grid::viewport(layout.pos.row = nrow_i, layout.pos.col = 1)
                )
              } else {
                tern::g_km(
                  df = df_i,
                  variables = variables,
                  font_size = font_size,
                  xlab = paste0(
                    xlab,
                    " (",
                    gsub(
                      "(^|[[:space:]])([[:alpha:]])",
                      "\\1\\U\\2",
                      tolower(anl$time_unit_var[1]),
                      perl = TRUE
                    ),
                    ")"
                  ),
                  yval = yval,
                  xticks = xticks,
                  newpage = FALSE,
                  title = paste(
                    title, ",", quote(facet_var),
                    "=", as.character(unique(df_i$facet_var))
                  ),
                  ggtheme = theme_minimal(),
                  annot_surv_med = annot_surv_med,
                  annot_coxph = annot_coxph,
                  control_surv = tern::control_surv_timepoint(conf_level = conf_level),
                  control_coxph_pw = tern::control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
                  ci_ribbon = ci_ribbon,
                  vp = grid::viewport(layout.pos.row = nrow_i, layout.pos.col = 1),
                  draw = TRUE
                )
              }
            },
            SIMPLIFY = FALSE
          )
          km_grobs <- tern::stack_grobs(grobs = result)
          km_grobs
        },
        env = list(
          font_size = font_size,
          facet_var = as.name(facet_var),
          xticks = xticks,
          xlab = xlab,
          time_unit_var = as.name(time_unit_var),
          yval = yval,
          conf_level = conf_level,
          pval_method = pval_method,
          annot_surv_med = annot_surv_med,
          annot_coxph = annot_coxph,
          ties = ties,
          ci_ribbon = ci_ribbon,
          title = title
        )
      )
    )
  } else {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          result <- tern::g_km(
            df = anl,
            variables = variables,
            font_size = font_size,
            xlab = paste0(
              xlab,
              " (",
              gsub(
                "(^|[[:space:]])([[:alpha:]])",
                "\\1\\U\\2",
                tolower(anl$time_unit_var[1]),
                perl = TRUE
              ),
              ")"
            ),
            yval = yval,
            xticks = xticks,
            newpage = TRUE,
            ggtheme = theme_minimal(),
            control_surv = tern::control_surv_timepoint(conf_level = conf_level),
            control_coxph_pw = tern::control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
            annot_surv_med = annot_surv_med,
            annot_coxph = annot_coxph,
            ci_ribbon = ci_ribbon,
            title = title,
          )
          result
        },
        env = list(
          font_size = font_size,
          xticks = xticks,
          xlab = xlab,
          time_unit_var = as.name(time_unit_var),
          yval = yval,
          conf_level = conf_level,
          pval_method = pval_method,
          annot_surv_med = annot_surv_med,
          annot_coxph = annot_coxph,
          ties = ties,
          ci_ribbon = ci_ribbon,
          title = title
        )
      )
    )
  }

  y$graph <- bracket_expr(graph_list)

  y
}

#' Teal Module: Kaplan-Meier
#'
#' This teal module produces a grid style Kaplan-Meier plot for data with
#' ADaM structure.
#'
#' @inheritParams module_arguments
#' @param facet_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' mae <- hermes::multi_assay_experiment
#' adtte <- radtte(cached = TRUE) %>%
#'     mutate(CNSR = as.logical(CNSR))
#' new_adtte <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "hd2")
#'
#' ADTTE <- new_adtte
#'
#' arm_ref_comp = list(
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   ),
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   )
#' )
#'
#'data <- teal_data(
#'     dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     dataset("ADTTE", ADTTE, code = 'mae <- hermes::multi_assay_experiment
#' adtte <- radtte(cached = TRUE) %>%
#'     mutate(CNSR = as.logical(CNSR))
#' new_adtte <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "hd2")
#'
#' ADTTE <- new_adtte'),
#' dataset("mae", mae)
#'   )
#'
#' modules <- root_modules(
#'     tm_g_km_mae(
#'       label = "KM PLOT",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(
#'         variable_choices(ADTTE, c("GeneID1820_counts")),
#'         "GeneID1820_counts"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, c("PARAMCD")),
#'         "OS"
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         "SEX"
#'       ),
#'       facet_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         NULL
#'       )
#'     )
#'   )
#'
#' app <- init(
#'   data = data,
#'   modules = modules
#' )
#'
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_km_mae <- function(label,
                    dataname,
                    mae_name,
                    parentname = ifelse(is(arm_var, "data_extract_spec"), teal.devel::datanames_input(arm_var), "ADSL"),
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    strata_var,
                    facet_var,
                    time_unit_var = choices_selected(variable_choices(dataname, "AVALU"), "AVALU", fixed = TRUE),
                    aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                    cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                    conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                    plot_height = c(1200L, 400L, 5000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL) {

 utils.nest::stop_if_not(
   utils.nest::is_character_single(label),
   utils.nest::is_character_single(dataname),
   utils.nest::is_character_single(mae_name),
   utils.nest::is_character_single(parentname),
    is.choices_selected(conf_level),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  utils.nest::check_slider_input(plot_height, allow_null = FALSE)
  utils.nest::check_slider_input(plot_width)

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    facet_var = cs_to_des_select(facet_var, dataname = parentname, multiple = FALSE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
  )
  args <- list(
    experiment_name = NULL,
    mae_name = mae_name,
    arm_ref_comp = arm_ref_comp,
    conf_level = conf_level,
    pre_output = pre_output,
    post_output = post_output
  )

  module(
    label = label,
    server = srv_g_km_mae,
    ui = ui_g_km_mae,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        mae_name = mae_name,
        label = label,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        plot_height = plot_height,
        plot_width = plot_width
      )
    ),
    filters = "all"
    # filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}


#' User Interface for KM Module
#' @noRd
#'
#' @importFrom shinyWidgets switchInput
ui_g_km_mae <- function(id,
                        datasets,
                        mae_name,
                        arm_var,
                        paramcd,
                        strata_var,
                        facet_var,
                        aval_var,
                        cnsr_var,
                        time_unit_var,
                        experiment_name,
                        arm_ref_comp,
                        conf_level,
                        pre_output,
                        post_output) {

  is_single_dataset_value <- teal.devel::is_single_dataset(
    arm_var,
    paramcd,
    strata_var,
    facet_var,
    aval_var,
    cnsr_var,
    time_unit_var,
    experiment_name
  )

  ns <- NS(id)

  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      teal.devel::plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(list(
        arm_var,
        experiment_name,
        paramcd,
        strata_var,
        facet_var,
        aval_var,
        cnsr_var
      )),
      selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),
      selectInput(ns("assay_name"), "Select assay", choices = ""),
      selectizeInput(ns("x_var"), "Select gene", choices = ""),
      # teal.devel::data_extract_input(
      #   id = ns("arm_var"),
      #   label = "Select Gene Variable",
      #   data_extract_spec = arm_var,
      #   is_single_dataset = is_single_dataset_value
      # ),

      #Will this change based on experiment chosen?
      teal.devel::data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_input(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_input(
        id = ns("facet_var"),
        label = "Facet Plots by",
        data_extract_spec = facet_var,
        is_single_dataset = is_single_dataset_value
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Treatments"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(arm_ref_comp),
          size = "mini"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          div(
            selectInput(
              ns("ref_arm"),
              "Reference Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            helpText("Multiple reference groups are automatically combined into a single group."),
            selectInput(
              ns("comp_arm"),
              "Comparison Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            teal.devel::data_extract_input(
              id = ns("strata_var"),
              label = "Stratify by",
              data_extract_spec = strata_var,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        teal.devel::panel_group(
          teal.devel::panel_item(
            "Comparison settings",
            radioButtons(
              ns("pval_method_coxph"),
              label = HTML(
                paste(
                  "p-value method for ",
                  tags$span(style = "color:darkblue", "Coxph"), # nolint
                  " (Hazard Ratio)",
                  sep = ""
                )
              ),
              choices = c("wald", "log-rank", "likelihood"),
              selected = "log-rank"
            ),
            radioButtons(
              ns("ties_coxph"),
              label = HTML(
                paste(
                  "Ties for ",
                  tags$span(style = "color:darkblue", "Coxph"), # nolint
                  " (Hazard Ratio)",
                  sep = ""
                )
              ),
              choices = c("exact", "breslow", "efron"),
              selected = "exact"
            )
          )
        )
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional plot settings",
          textInput(
            inputId = ns("xticks"),
            label = "Specify break intervals for x-axis e.g. 0 ; 500"
          ),
          radioButtons(
            ns("yval"),
            tags$label("Value on y-axis", class = "text-primary"),
            choices = c("Survival probability", "Failure probability"),
            selected = c("Survival probability"),
          ),
          numericInput(
            inputId = ns("font_size"),
            label = "Plot tables font size",
            value = 8,
            min = 5,
            max = 15,
            step = 1,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_ci_ribbon"),
            label = "Show CI ribbon",
            value = FALSE,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_km_table"),
            label = "Show KM table",
            value = TRUE,
            width = "100%"
          ),
          optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            conf_level$choices,
            conf_level$selected,
            multiple = FALSE,
            fixed = conf_level$fixed
          ),
          textInput(ns("xlab"), "X-axis label", "Time"),
          teal.devel::data_extract_input(
            id = ns("time_unit_var"),
            label = "Time Unit Variable",
            data_extract_spec = time_unit_var,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' Server for KM Module
#' @noRd
#'
srv_g_km_mae <- function(input,
                     output,
                     session,
                     datasets,
                     dataname,
                     mae_name,
                     parentname,
                     paramcd,
                     arm_var,
                     experiment_name,
                     arm_ref_comp,
                     strata_var,
                     facet_var,
                     aval_var,
                     cnsr_var,
                     label,
                     time_unit_var,
                     plot_height,
                     plot_width) {
  # stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the filtered data set or the chosen experiment changes, update
  # the call that creates the chosen experiment data object.
  experiment_call <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    dat <- datasets$get_filtered_datasets(mae_name)
    dat$get_filter_states(input$experiment_name)$get_call()
  })

  # When the chosen experiment changes, recompute the assay names.
  assay_names <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
  })

  # When the assay names change, update the choices for assay.
  observeEvent(assay_names(), {
    assay_name_choices <- assay_names()

    updateSelectInput(
      session,
      "assay_name",
      choices = assay_name_choices,
      selected = assay_name_choices[1]
    )
  })

  # When the chosen experiment call changes, we recompute gene names.
  genes <- eventReactive(experiment_call(), ignoreNULL = FALSE, {
    object <- experiment_data()
    rownames(object)
  })

  # When the genes are recomputed, update the choices for genes in the UI.
  observeEvent(genes(), {
    gene_choices <- genes()

    updateSelectizeInput(
      session,
      "x_var",
      choices = gene_choices,
      selected = gene_choices[1],
      server = TRUE
    )
  })

  # Setup arm variable selection, default reference arms and default
  # comparison arms for encoding panel
  teal.devel::arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", # from UI
    id_comp = "comp_arm", # from UI
    id_arm_var = extract_input("arm_var", dataname),
    datasets = datasets,
    dataname = parentname,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(aval_var, cnsr_var, arm_var, paramcd, strata_var, facet_var, time_unit_var),
    input_id = c("aval_var", "cnsr_var", "arm_var", "paramcd", "strata_var", "facet_var", "time_unit_var"),
    merge_function = "dplyr::inner_join"
  )

  validate_checks <- reactive({

    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_facet_var <- as.vector(anl_m$columns_source$facet_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)
    input_xticks <- gsub(";", ",", trimws(input$xticks)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_time_unit_var),
      arm_var = input_arm_var
    )

    # validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
    }
    if (input$compare_arms) {
      validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    }

    utils.nest::call_with_colon("teal.devel::validate_standard_inputs", unlist_args = validate_args)

    # validate xticks
    if (length(input_xticks) == 0) {
      input_xticks <- NULL
    }
    else {
      validate(need(all(!is.na(input_xticks)), "Not all values entered were numeric"))
      validate(need(all(input_xticks >= 0), "All break intervals for x-axis must be non-negative"))
      validate(need(any(input_xticks > 0), "At least one break interval for x-axis must be positive"))
    }

    validate(need(
      input$conf_level > 0 && input$conf_level < 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(need(utils.nest::is_character_single(input_aval_var), "Analysis variable should be a single column."))
    validate(need(utils.nest::is_character_single(input_cnsr_var), "Censor variable should be a single column."))

    NULL
  })

  call_preparation <- reactive({
    validate_checks()

    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()

    ANL <- teal.devel::chunks_get_var("ANL") # nolint
    teal.devel::validate_has_data(ANL, 2)

    input_xticks <- gsub(";", ",", trimws(input$xticks)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()

    if (length(input_xticks) == 0) {
      input_xticks <- NULL
    }

    input_paramcd <- as.character(unique(anl_m$data()[[as.vector(anl_m$columns_source$paramcd)]]))
    title <- paste("KM Plot of", input_paramcd)

    my_calls <- template_g_km_mae(
      dataname = "ANL",
      # arm_var = input$arm_var,
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      compare_arm = input$compare_arms,
      combine_comp_arms = input$combine_comp_arms,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
      strata_var = as.vector(anl_m$columns_source$strata_var),
      time_points = NULL,
      time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
      facet_var = as.vector(anl_m$columns_source$facet_var),
      annot_surv_med = input$show_km_table,
      annot_coxph = input$compare_arms,
      xticks = input_xticks,
      font_size = input$font_size,
      pval_method = input$pval_method_coxph,
      conf_level = as.numeric(input$conf_level),
      ties = input$ties_coxph,
      xlab = input$xlab,
      yval = ifelse(input$yval == "Survival probability", "Survival", "Failure"),
      ci_ribbon = input$show_ci_ribbon,
      title = title
    )
    mapply(expression = my_calls, teal.devel::chunks_push)
  })

  km_plot <- reactive({
    call_preparation()
    teal.devel::chunks_safe_eval()
  })


  # Insert the plot into a plot with settings module from teal.devel
  callModule(
    teal.devel::plot_with_settings_srv,
    id = "myplot",
    plot_r = km_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(
      list(arm_var, paramcd, strata_var, facet_var, aval_var, cnsr_var, time_unit_var)
    ),
    modal_title = label
  )
}

sample_tm_g_km_mae <- function() {
  library(scda)
  library(dplyr)
  library(random.cdisc.data)

  ADSL <- synthetic_cdisc_data("latest")$adsl
  mae <- hermes::multi_assay_experiment
  adtte <- radtte(cached = TRUE) %>%
      mutate(CNSR = as.logical(CNSR))
  new_adtte <- h_km_mae_to_adtte(
    adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = "hd2"
  )

  ADTTE <- new_adtte

  arm_ref_comp = list(
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    ),
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    )
  )

  data <- teal_data(
      dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
      dataset("ADTTE", ADTTE, code = 'mae <- hermes::multi_assay_experiment
  adtte <- radtte(cached = TRUE) %>%
      mutate(CNSR = as.logical(CNSR))
  new_adtte <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "hd2")

  ADTTE <- new_adtte'),
  dataset("mae", mae)
    )

  modules <- root_modules(
      tm_g_km_mae(
        label = "KM PLOT",
        dataname = "ADTTE",
        mae_name = "mae",
        arm_var = choices_selected(
          variable_choices(ADTTE, c("GeneID1820_counts")),
          "GeneID1820_counts"
        ),
        paramcd = choices_selected(
          value_choices(ADTTE, c("PARAMCD")),
          "OS"
        ),
        strata_var = choices_selected(
          variable_choices(ADSL, c("SEX", "BMRKR2")),
          "SEX"
        ),
        facet_var = choices_selected(
          variable_choices(ADSL, c("SEX", "BMRKR2")),
          NULL
        )
      )
    )

  app <- init(
    data = data,
    modules = modules
  )

  shinyApp(ui = app$ui, server = app$server)
}
