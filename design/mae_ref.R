mae <- multi_assay_experiment

library(MultiAssayExperiment)
experiments(mae)

lobstr::ref(mae)
lobstr::ref(mae[[1]])
lobstr::ref(mae[[2]])
lobstr::ref(mae[[3]])

mae[[1]] <- subset(mae[[1]], subset = chromosome_name == "1", select = TechnicalFailureFlag)

lobstr::ref(mae)
lobstr::ref(mae[[1]])
lobstr::ref(mae[[2]])
lobstr::ref(mae[[3]])
