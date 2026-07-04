test_that("estimate_environment_white integrates an upper lat-long map", {
  env = array(0, dim = c(8, 16, 4))
  env[,, 1] = 2
  env[,, 2] = 4
  env[,, 3] = 8
  env[,, 4] = 1
  env = ray_read_image(
    env,
    assume_colorspace = CS_SRGB,
    assume_white = "D65"
  )

  white = estimate_environment_white(env)
  expected_xyz = as.vector(CS_SRGB$rgb_to_xyz %*% c(2, 4, 8))
  expected_xyz = expected_xyz / expected_xyz[2]

  expect_equal(as.numeric(white), expected_xyz, tolerance = 1e-12)
  expect_equal(
    attr(white, "rgb"),
    c(2, 4, 8) * attr(white, "sum_weights"),
    tolerance = 1e-10
  )
  expect_equal(attr(white, "rgb_ratio"), c(2, 4, 8) / mean(c(2, 4, 8)))
  expect_equal(attr(white, "weighting"), "cosine")
  expect_equal(attr(white, "hemisphere"), "upper")
})

test_that("render_environment_white_balance uses estimated environment white", {
  env = array(0, dim = c(8, 16, 4))
  env[,, 1] = 2
  env[,, 2] = 4
  env[,, 3] = 8
  env[,, 4] = 1
  env = ray_read_image(
    env,
    assume_colorspace = CS_SRGB,
    assume_white = "D65"
  )

  img = array(1, dim = c(2, 2, 4))
  img = ray_read_image(
    img,
    assume_colorspace = CS_SRGB,
    assume_white = "D65"
  )

  out = render_environment_white_balance(
    img,
    env,
    target_white = "D65",
    bake = TRUE
  )
  expected = render_white_balance(
    img,
    reference_white = estimate_environment_white(env),
    target_white = "D65",
    bake = TRUE
  )

  expect_s3_class(out, "rayimg")
  expect_equal(as.numeric(out), as.numeric(expected), tolerance = 1e-12)
  expect_equal(
    attr(out, "environment_white"),
    estimate_environment_white(env),
    tolerance = 1e-12
  )
})
