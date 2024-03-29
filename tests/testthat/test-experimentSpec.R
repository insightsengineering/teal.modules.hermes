# h_order_genes ----
test_that("h_order_genes works as expected to sort only by name", {
  genes <- data.frame(
    id = c("2533", "2534", "2535", "2536", "2537"),
    name = c("e", "d", "c", "b", "a")
  )
  result <- h_order_genes(genes)
  expected <- genes[5:1, ]
  expect_identical(result, expected)
})

test_that("h_order_genes works as expected to sort only by id", {
  genes <- data.frame(
    id = c("7", "1", "2", "345346", "0"),
    name = rep("", 5)
  )
  result <- h_order_genes(genes)
  expected <- genes[c(5, 2, 3, 4, 1), ] # Note: alphabetical sorting.
  expect_identical(result, expected)
})

test_that("h_order_genes works as expected with mixed id and name sorting", {
  genes <- data.frame(
    id = c("7", "1", "2", "345346", "0"),
    name = c("e", "", "c", "", "a")
  )
  result <- h_order_genes(genes)
  expected <- genes[c(5, 3, 1, 2, 4), ]
  expect_identical(result, expected)
})

test_that("h_order_genes does not fail when given empty data frame", {
  genes <- data.frame(
    id = character(),
    name = character()
  )
  result <- h_order_genes(genes)
  expect_identical(result, genes)
})

# h_gene_data ----

test_that("h_gene_data works as expected", {
  object <- hermes::hermes_data[1:10, ]
  result <- h_gene_data(object, "symbol")
  expected <- data.frame(
    id = c(
      "GeneID:11185", "GeneID:10677", "GeneID:101928428",
      "GeneID:100422835", "GeneID:102466731", "GeneID:64881", "GeneID:286205",
      "GeneID:8365", "GeneID:6804", "GeneID:100423018"
    ),
    name = c(
      "INMT", "AVIL", "LOC101928428", "MIR3183", "MIR6769A", "PCDH20",
      "SCAI", "HIST1H4H", "STX1A", "MIR3156-3"
    )
  )
  expect_identical(result, expected)
})

test_that("h_gene_data does not fail when object does not contain any genes", {
  object <- hermes::hermes_data[NULL, ]
  result <- expect_silent(h_gene_data(object, "symbol"))
  expected <- data.frame(
    id = character(),
    name = character()
  )
  expect_identical(result, expected)
})
