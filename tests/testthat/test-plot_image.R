test_that("Checking plot_image", {
  # skip_on_os(c("windows", "linux", "solaris"))
  plt_img_args = expand.grid(
    draw_grid = list(FALSE, TRUE),
    asp = list(0.5, 1, 2),
    new_page = list(TRUE, FALSE)
  )

  run_tests(
    "plot_image",
    plt_img_args,
    plot_prefix = "plt_img",
    warning_rows = NULL,
    error_rows = NULL,
    list(image = dragon)
  )
})

test_that("Checking plot_image_grid", {
  # skip_on_os(c("windows", "linux", "solaris"))
  plt_img_args = expand.grid(
    dim = list(c(1, 1), c(2, 2), c(1, 2), c(2, 1)),
    draw_grid = list(FALSE, TRUE),
    asp = list(c(0.5, 1, 2, 3))
  )
  # plt_img_args = plt_img_args[-6,] #False positive

  # warning_rows = c(1, 3, 4, 5, 7, 8)

  run_tests(
    "plot_image_grid",
    plt_img_args,
    plot_prefix = "plt_img_grid",
    warning_rows = NULL,
    # warning_rows = warning_rows,
    error_rows = NULL,
    list(input_list = list(dragon, NULL, NULL, dragon))
  )
})

test_that("checking print functions", {
  volcano_print = ray_read_image(volcano)
  expect_snapshot_output(print(volcano_print))

  dragon_print1 = ray_read_image(dragon)
  dragon_print2 = dragon_print1
  dragon_print2[,, 4] = 0.5
  expect_snapshot_output(print(dragon_print1))
  expect_snapshot_output(print(dragon_print1[,, 1]))
  expect_snapshot_output(print(dragon_print1[,, 2]))
  expect_snapshot_output(print(dragon_print1[,, 3]))
  expect_snapshot_output(print(dragon_print1[,, 4]))
  expect_snapshot_output(print(dragon_print1[,, c(1, 3)]))
  expect_snapshot_output(print(dragon_print1[,, 1:3]))
  expect_snapshot_output(print(dragon_print1[,, c(1, 2)]))
  expect_snapshot_output(print(dragon_print1[,, c(1, 4)]))
  expect_error(print(dragon_print1[,, c(1, 1, 1, 1, 1)]), regexp = "fewer")

  expect_snapshot_output(print(dragon_print1[1:10, , ]))
  expect_snapshot_output(print(dragon_print1[1:10, , 1]))
  expect_snapshot_output(print(dragon_print1[1:10, , 2]))
  expect_snapshot_output(print(dragon_print1[1:10, , 3]))
  expect_snapshot_output(print(dragon_print1[1:10, , 4]))
  expect_snapshot_output(print(dragon_print1[1:10, , c(1, 3)]))
  expect_snapshot_output(print(dragon_print1[1:10, , 1:3]))
  expect_snapshot_output(print(dragon_print1[1:10, , c(1, 2)]))
  expect_snapshot_output(print(dragon_print1[1:10, , c(1, 4)]))
  expect_error(print(dragon_print1[1:10, , c(1, 1, 1, 1, 1)]), regexp = "fewer")

  expect_snapshot_output(print(dragon_print1[, 1:10, ]))
  expect_snapshot_output(print(dragon_print1[, 1:10, 1]))
  expect_snapshot_output(print(dragon_print1[, 1:10, 2]))
  expect_snapshot_output(print(dragon_print1[, 1:10, 3]))
  expect_snapshot_output(print(dragon_print1[, 1:10, 4]))
  expect_snapshot_output(print(dragon_print1[, 1:10, c(1, 3)]))
  expect_snapshot_output(print(dragon_print1[, 1:10, 1:3]))
  expect_snapshot_output(print(dragon_print1[, 1:10, c(1, 2)]))
  expect_snapshot_output(print(dragon_print1[, 1:10, c(1, 4)]))
  expect_error(print(dragon_print1[, 1:10, c(1, 1, 1, 1, 1)]), regexp = "fewer")

  expect_snapshot_output(print(dragon_print1[1:10, 1:10, ]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, 1]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, 2]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, 3]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, 4]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, c(1, 3)]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, 1:3]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, c(1, 2)]))
  expect_snapshot_output(print(dragon_print1[1:10, 1:10, c(1, 4)]))
  expect_error(
    print(dragon_print1[1:10, 1:10, c(1, 1, 1, 1, 1)]),
    regexp = "fewer"
  )

  expect_snapshot_output(print(dragon_print1[, 1:9, ]))
  expect_snapshot_output(print(dragon_print1[, 1:9, 1]))
  expect_snapshot_output(print(dragon_print1[, 1:9, 2]))
  expect_snapshot_output(print(dragon_print1[, 1:9, 3]))
  expect_snapshot_output(print(dragon_print1[, 1:9, 4]))
  expect_snapshot_output(print(dragon_print1[, 1:9, c(1, 3)]))
  expect_snapshot_output(print(dragon_print1[, 1:9, 1:3]))
  expect_snapshot_output(print(dragon_print1[, 1:9, c(1, 2)]))
  expect_snapshot_output(print(dragon_print1[, 1:9, c(1, 4)]))
  expect_error(print(dragon_print1[, 1:10, c(1, 1, 1, 1, 1)]), regexp = "fewer")

  expect_snapshot_output(print(dragon_print1[1:9, 1:9, ]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, 1]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, 2]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, 3]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, 4]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, c(1, 3)]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, 1:3]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, c(1, 2)]))
  expect_snapshot_output(print(dragon_print1[1:9, 1:9, c(1, 4)]))
  expect_error(
    print(dragon_print1[1:10, 1:10, c(1, 1, 1, 1, 1)]),
    regexp = "fewer"
  )

  #Maybe need to change this
  expect_snapshot_output(print(dragon_print1[1, 1:10, ]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, 1]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, 2]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, 3]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, 4]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, c(1, 3)]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, 1:3]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, c(1, 2)]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, c(1, 4)]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, c(1, 2, 4)]))
  expect_snapshot_output(print(dragon_print1[1, 1:10, c(4, 3, 1)]))

  expect_error(
    print(dragon_print1[1, 1:10, c(1, 1, 1, 1, 1, 1)]),
    regexp = "fewer"
  )

  expect_snapshot_output(print(dragon_print2[1:2, 1:2, 1]))
  expect_snapshot_output(print(dragon_print2[1, 1:10, 1:4]))
  expect_snapshot_output(print(dragon_print2[1:2, 1:10, 1:4]))
  expect_snapshot_output(print(dragon_print2[1:2, 1:10, 1]))
  expect_snapshot_output(print(dragon_print2[1:2, 1:10, 2]))
  expect_snapshot_output(print(dragon_print2[1:2, 1:10, 3]))
  expect_snapshot_output(print(dragon_print2[1:2, 1:10, 4]))
})
