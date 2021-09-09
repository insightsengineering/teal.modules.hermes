# Mockup class to represent teal Datasets behavior as needed in tests.
MockDatasets <- R6::R6Class(  # nolint
  classname = "Datasets",
  public = list(
    initialize = function(data_list) {
      private$data_list <- data_list
    },
    get_data = function(dataname, filtered = FALSE) {
      assert_string(dataname)
      assert_flag(filtered)
      if (!(dataname %in% names(private$data_list)))
        stop("not available in data_list")
      private$data_list[[dataname]]
    }
  ),
  private = list(
    data_list = NULL
  )
)

mock_datasets <- function(...) {
  MockDatasets$new(...)
}
