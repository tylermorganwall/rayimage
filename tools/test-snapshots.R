Sys.setenv(
  RAYIMAGE_RUN_SNAPSHOT_TESTS = "true",
  NOT_CRAN = "true"
)

library(testthat)
library(rayimage)

test_files = c(
  "tests/testthat/test-plot_image.R",
  "tests/testthat/test-render_functions.R"
)

run_snapshot_file = function(test_file) {
  if (!file.exists(test_file)) {
    stop("Missing snapshot test file: ", test_file, call. = FALSE)
  }

  result = tryCatch(
    testthat::test_file(test_file),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    message(conditionMessage(result))
    return(1L)
  }

  result_df = as.data.frame(result)
  if (!nrow(result_df)) {
    return(0L)
  }

  if (any(result_df$failed > 0) || any(result_df$error)) {
    return(1L)
  }

  0L
}

status = max(vapply(test_files, run_snapshot_file, integer(1)))
quit(status = status, save = "no")
