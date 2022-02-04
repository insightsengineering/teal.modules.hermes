my_mae <- setClass("MyMae", contains = "MultiAssayExperiment")

setReplaceMethod("[[", "MyMae", function(x, i, j, ..., value) {
  if (!missing(j) || length(list(...)))
    stop("invalid replacement")
  if (is.list(value) || (is(value, "List") && !is(value, "DataFrame")))
    stop("Provide a compatible API object for replacement")
  if (!any(colnames(value) %in% colnames(x)[[i]]) && !MultiAssayExperiment:::.isEmpty(value))
    stop("'colnames(value)' have no match in 'colnames(x)[[i]]';\n",
         "See '?renameColname' for renaming colname identifiers")

  experiments(x)[i] <- S4Vectors::setListElement(experiments(x)[i], 1L, value)

  return(x)
})

mae <- my_mae(multi_assay_experiment)

lobstr::ref(mae)
lobstr::ref(mae[[1]])
lobstr::ref(mae[[2]])
lobstr::ref(mae[[3]])

mae[[1]] <- subset(mae[[1]], subset = chromosome_name == "1", select = TechnicalFailureFlag)

lobstr::ref(mae)
lobstr::ref(mae[[1]])
lobstr::ref(mae[[2]])
lobstr::ref(mae[[3]])

experiments(mae)

# Problem is that also something in sampleMap is changed:
# harmonizing input:
#   removing 5 sampleMap rows with 'colname' not in colnames of experiments

setReplaceMethod("experiments", c("MultiAssayExperiment", "ExperimentList"),
                 function(object, value) {

                   # here the whole MAE is recalculated:
                   rebliss <- .harmonize(value, colData(object), sampleMap(object))
                   if (!any(names(object) %in% names(value)) && !isEmpty(object)) {
                     drops(object) <-
                       list(experiments = setdiff(names(object), names(value)))
                     warning("'experiments' dropped; see 'metadata'", call. = FALSE)
                   }

                   BiocGenerics:::replaceSlots(
                     object = object,
                     ExperimentList = rebliss[["experiments"]],
                     colData = rebliss[["colData"]],
                     sampleMap = rebliss[["sampleMap"]],
                     check = FALSE
                   )
                 }
)
