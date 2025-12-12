# Standard Function Arguments

The documentation to this function lists all the conventional arguments
in functions.

## Arguments

- adtte:

  (`data frame`)  
  an `adtte` dataset.

- usubjid_var:

  (`string`)  
  variable name of the subject ID variable.

- mae:

  (`MultiAssayExperiment`)  
  contains `AnyHermesData` objects.

- object:

  (`AnyHermesData`)  
  contains RNA-seq values for one experiment.

- genes:

  (`GeneSpec`)  
  specification for gene(s) (signature), e.g. using
  [`hermes::gene_spec()`](https://insightsengineering.github.io/hermes/latest-tag/reference/gene_spec.html).

- experiment_name:

  (`string`)  
  the desired `HermesData` to use.

- assay_name:

  (`string`)  
  the assay to define the groups.
