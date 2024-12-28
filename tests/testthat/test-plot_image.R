save_test_png = function(code, path) {
  grDevices::png(filename=path, width=500, height=400)
  code
  dev.off()
  path
}

compare_image = function(path1, path2, quantile_diff = 0.001, 
                         cdf_diff = 0.1,
                         max_diff = 0.25) {
  image1 = ray_read_image(path1)
  image2 = ray_read_image(path2)
  all_dim_same = all(dim(image1) == dim(image2))

  diffs = abs(image2 - image1)
  #Ignore small differences
  mostly_identical = sum(diffs > cdf_diff) < length(diffs) * quantile_diff
  diffs_are_minor = max(diffs) < max_diff
  return(mostly_identical && diffs_are_minor && all_dim_same)
}

expect_snapshot_file_info = function(path, name, compare, variant, args, i) {
  tryCatch({
    expect_snapshot_file(
      path = path,
      name = name,
      compare = compare,
      variant = variant
    )
  }, error = function(e) {
    stop(sprintf(
      "Snapshot test failed on row %d with arguments: %s\n\nOriginal error: %s",
      i,
      paste(names(args), unname(args), sep = "=", collapse = ", "),
      e$message
    ))
  })
}

expect_no_error_info = function(code, args, i) {
  tryCatch({
    expect_no_error(
      code
    )
  }, error = function(e) {
    is_mat_args = unlist(lapply(args, is.array))
    for(i in seq_along(args)) {
      if(is_mat_args[i]) {
        args[[i]] = "[[ matrix / array ]]"
      }
    }
    stop(sprintf(
      "Snapshot test failed on row %d with arguments: %s\n\nOriginal error: %s",
      i,
      paste(names(args), unname(args), sep = "=", collapse = ", "),
      e$message
    ))
  })
}

run_tests = function(func, argument_grid, plot_prefix="", 
                     warning_rows = NULL, error_rows = NULL, ...) {
  stopifnot(inherits(argument_grid,"data.frame"))
  for(i in seq_len(nrow(argument_grid))){
    args = unlist(argument_grid[i,], recursive = FALSE)
    test_filename = sprintf("%s_test%i.png",
                            plot_prefix , i)
    path = tempfile(fileext = ".png")

    args = append(args, ...)
    if(i %in% warning_rows) {
      expect_snapshot_warning(do.call(func, args = args),
                              variant = Sys.info()[["sysname"]])
    } else if(i %in% error_rows) {
      expect_snapshot_error(do.call(func, args = args),
                            variant = Sys.info()[["sysname"]])
    }
    if(interactive()) {
      do.call(func, args = args) |> 
        expect_no_error_info(args = args, i = i)
    } else {
      save_test_png(do.call(func, args = args), path) |>
        suppressMessages() |>
        suppressWarnings() |>
        expect_snapshot_file_info(name = test_filename, 
          compare = compare_image,
          variant = Sys.info()[["sysname"]], args = args, i = i)
    }
  }
}

test_that("Checking plot_image", {
  # skip_on_os(c("windows", "linux", "solaris"))
  plt_img_args = expand.grid(rotate   = list(0,90,180,270),
                             draw_grid = list(FALSE, TRUE),
                             asp = list(0.5,1,2),
                             new_page = list(TRUE, FALSE))

  run_tests("plot_image", plt_img_args, plot_prefix = "plt_img",
            warning_rows = NULL, error_rows = NULL,
            list(image = dragon))
})

test_that("Checking plot_image_grid", {
  # skip_on_os(c("windows", "linux", "solaris"))
  plt_img_args = expand.grid(input_list = list(list(dragon, NULL, NULL, dragon)),
                             dim = list(c(1,1),
                                        c(2,2),
                                        c(1,2),
                                        c(2,1)),
                             draw_grid = list(FALSE, TRUE),
                             asp = list(c(0.5,1,2,3)))
  # plt_img_args = plt_img_args[-6,] #False positive

  warning_rows = c(1,3,4,5,7,8)

  run_tests("plot_image_grid", plt_img_args, plot_prefix = "plt_img_grid",
            warning_rows = warning_rows, error_rows = NULL,
            list())
})


test_that("Checking ray_read/ray_write", {
  rayfile_png = tempfile(fileext = ".png")
  clamped_dragon = render_clamp(dragon)
  rayimage::ray_write_image(filename = rayfile_png, image = clamped_dragon)
  dragon_png = rayimage::ray_read_image(image = rayfile_png)
  expect_true(compare_image(clamped_dragon, dragon_png))

  rayfile_jpg = tempfile(fileext = ".jpg")
  rayimage::ray_write_image(filename = rayfile_jpg, image = clamped_dragon, quality = 1)
  dragon_jpg = rayimage::ray_read_image(image = rayfile_jpg)
  expect_true(compare_image(clamped_dragon, dragon_jpg))

  rayfile_tiff = tempfile(fileext = ".tiff")
  rayimage::ray_write_image(filename = rayfile_tiff, image = rayimage::dragon)
  dragon_tiff = rayimage::ray_read_image(image = rayfile_tiff)
  expect_true(compare_image(clamped_dragon, dragon_tiff))
})

  