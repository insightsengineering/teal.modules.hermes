# Standard Module Arguments

The documentation to this function lists all the conventional arguments
in `hermes` teal modules.

## Arguments

- data:

  (`reactive`)  
  `reactive(<teal_data>)` holding all the data sets provided during app
  initialization after going through the filters.

- label:

  (`string`)  
  menu item label of the module in the teal app.

- inputId:

  (`string`)  
  the ID used to call the module input.

- adtte_name:

  (`string`)  
  name of the `ADTTE` dataset.

- adtte_vars:

  (named `list` of `string`)  
  names of the variables to use in the `ADTTE` dataset. It should
  comprise elements:

  - `aval`: the numeric time-to-event variable.

  - `avalu`: the variable holding the unit of `aval`.

  - `is_event`: the logical event variable. It needs to be `TRUE` when
    there was an observed event, and `FALSE` if the time is censored
    without observed event.

  - `paramcd`: the character or factor parameter code variable, defining
    the type of time-to-event for selection in the module.

  - `usubjid`: the subject ID variable.

- mae_name:

  (`string`)  
  name of the MAE data used in the teal module.

- exclude_assays:

  (`character`)  
  names of the assays which should not be included in choices in the
  teal module.

- summary_funs:

  (named `list` of functions or `NULL`)  
  functions which can be used in the the gene signatures. For modules
  that support also multiple genes without summary, `NULL` can be
  included to not summarize the genes but provide all of them.

- reporter:

  (`Reporter`) object

- pre_output:

  (`shiny.tag` or `NULL`)  
  placed before the output to put the output into context (for example a
  title).

- post_output:

  (`shiny.tag` or `NULL`)  
  placed after the output to put the output into context (for example
  the [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements can be useful).

- id:

  (`string`) the shiny module id.

- plot_height:

  (`list`)  
  list of integers to set the default, minimum, and maximum plot height.

- plot_width:

  (`list`)  
  list of integers to set the default, minimum, and maximum plot width.

- filter_panel_api:

  (`FilterPanelAPI`)  
  object describing the actual filter panel API.

- .test:

  (`flag`)  
  whether to display the internal structure of the plot for testing
  purposes.
