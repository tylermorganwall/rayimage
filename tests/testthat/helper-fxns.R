save_test_png = function(code, path) {
  grDevices::png(filename = path, width = 500, height = 400)
  dev_id = grDevices::dev.cur()
  code()
  on.exit(dev.off(dev_id), add = TRUE)
  path
}

compare_image = function(
  path1,
  path2,
  quantile_diff = 0.001,
  cdf_diff = 0.1,
  max_diff = 0.3
) {
  image1 = ray_read_image(path1)
  image2 = ray_read_image(path2)
  all_dim_same = all(dim(image1) == dim(image2))

  diffs = abs(image2 - image1)
  #Ignore small differences
  mostly_identical = sum(diffs > cdf_diff) < length(diffs) * quantile_diff
  diffs_are_minor = max(diffs) < max_diff
  if (!mostly_identical) {
    warning(sprintf(
      "Number greater than CDF diff: %i vs %i/%i",
      sum(diffs > cdf_diff),
      length(diffs) * quantile_diff,
      length(diffs)
    ))
  }
  if (!diffs_are_minor) {
    warning(sprintf("Max diff: %f vs %f", max(diffs), max_diff))
  }
  if (!all_dim_same) {
    warning(sprintf(
      "Dim not the same: %s vs %s",
      paste0(dim(image1), collapse = "x"),
      paste0(dim(image2), collapse = "x")
    ))
  }
  return(mostly_identical && diffs_are_minor && all_dim_same)
}

expect_snapshot_file_info = function(path, name, compare, variant, args, i) {
  tryCatch(
    {
      expect_snapshot_file(
        path = path,
        name = name,
        compare = compare,
        variant = variant
      )
    },
    error = function(e) {
      stop(sprintf(
        "Snapshot test failed on row %d with arguments: %s\n\nOriginal error: %s",
        i,
        paste(names(args), unname(args), sep = "=", collapse = ", "),
        e$message
      ))
    }
  )
}

expect_no_error_info = function(code, args, i) {
  tryCatch(
    {
      expect_no_error(
        code
      )
    },
    error = function(e) {
      is_mat_args = unlist(lapply(args, is.array))
      for (i in seq_along(args)) {
        if (is_mat_args[i]) {
          args[[i]] = "[[ matrix / array ]]"
        }
      }
      stop(sprintf(
        "Snapshot test failed on row %d with arguments: %s\n\nOriginal error: %s",
        i,
        paste(names(args), unname(args), sep = "=", collapse = ", "),
        e$message
      ))
    }
  )
}

run_tests = function(
  func,
  argument_grid,
  plot_prefix = "",
  warning_rows = NULL,
  error_rows = NULL,
  ...
) {
  stopifnot(inherits(argument_grid, "data.frame"))
  for (i in seq_len(nrow(argument_grid))) {
    args = unlist(argument_grid[i, , drop = FALSE], recursive = FALSE)
    test_filename = sprintf("%s_test%i.png", plot_prefix, i)

    args = append(args, ...)
    if (i %in% warning_rows) {
      expect_snapshot_warning(
        do.call(func, args = args),
        variant = Sys.info()[["sysname"]]
      )
    } else if (i %in% error_rows) {
      expect_snapshot_error(
        do.call(func, args = args),
        variant = Sys.info()[["sysname"]]
      )
    }
    if (interactive()) {
      do.call(func, args = args) |>
        expect_no_error_info(args = args, i = i)
    } else {
      path = tempfile(fileext = ".png")
      save_test_png(
        function() {
          do.call(func, args = args)
        },
        path
      ) |>
        # suppressMessages() |>
        # suppressWarnings() |>
        expect_snapshot_file_info(
          name = test_filename,
          compare = compare_image,
          variant = Sys.info()[["sysname"]],
          args = args,
          i = i
        )
    }
  }
}
