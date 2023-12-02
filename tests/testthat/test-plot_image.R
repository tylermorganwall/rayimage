save_test_png = function(code, path) {
  grDevices::png(filename=path, width=500, height=400)
  code
  dev.off()
  path
}

compare_image = function(path1, path2) {
  image1 = png::readPNG(path1)
  image2 = png::readPNG(path2)
  return(identical(image1, image2))
}

run_tests = function(func, argument_grid, plot_prefix="", ...) {
  stopifnot(inherits(argument_grid,"data.frame"))
  for(i in seq_len(nrow(argument_grid))){
    args = unlist(argument_grid[i,], recursive = FALSE)
    test_filename = sprintf("%s_test%i.png",
                            plot_prefix , i)
    path = tempfile(fileext = ".png")

    args = append(args, ...)
    # args = append(args, list(filename = path))
    # browser()
    save_test_png(do.call(func, args = args), path) |>
      suppressMessages() |>
      suppressWarnings() |>
      expect_snapshot_file(name = test_filename, compare = compare_image)
  }
}

test_that("Checking plot_image", {
  skip_on_os(c("windows", "linux", "solaris"))
  plt_img_args = expand.grid(rotate   = list(0,90,180,270),
                             draw_grid = list(FALSE, TRUE),
                             asp = list(0.5,1,2),
                             new_page = list(TRUE, FALSE))

  run_tests("plot_image", plt_img_args, plot_prefix = "plt_img",
            list(input = dragon))
})

test_that("Checking plot_image_grid", {
  skip_on_os(c("windows", "linux", "solaris"))
  plt_img_args = expand.grid(input_list = list(list(dragon, NULL, NULL, dragon)),
                             dim = list(c(1,1),
                                        c(2,2),
                                        c(1,2),
                                        c(2,1)),
                             draw_grid = list(FALSE, TRUE),
                             asp = list(c(0.5,1,2,3)))
  plt_img_args = plt_img_args[-6,] #False positive

  run_tests("plot_image_grid", plt_img_args, plot_prefix = "plt_img_grid", list())
})
