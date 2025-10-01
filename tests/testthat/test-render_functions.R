#Test with all types: Grey, Grey + A, RGB, RGBA

# Fixtures shared across render function tests
dragon_small = render_resized(dragon, dims = c(64, 64))
dragon_small_2layer = dragon_small[,, c(1, 4)]
dragon_small_rect = dragon_small[1:48, , ]
dragon_small_rect_2layer = dragon_small[,, c(1, 4)]
dragon_small_rgb = dragon_small[,, 1:3]
dragon_small_rgb_rect = dragon_small[1:48, , 1:3]

all_test_images = list(
  dragon_small,
  dragon_small_2layer,
  dragon_small_rect,
  dragon_small_rect_2layer,
  dragon_small_rgb,
  dragon_small_rgb_rect,
  volcano
)

dragon_bw = dragon_small[,, 1]
dragon_small = dragon[1:64, 1:64, ]

dragondepth_small = dragondepth[1:64, 1:64]
volcano_small = volcano[seq_len(60), seq_len(60)]

overlay_gradient = array(1, dim = c(64, 64, 4))
overlay_gradient[,, 1] = matrix(
  seq(0, 1, length.out = 64),
  64,
  64,
  byrow = TRUE
)
overlay_gradient[,, 2] = 0
overlay_gradient[,, 3] = matrix(seq(0, 1, length.out = 64), 64, 64)
overlay_gradient[,, 4] = 0.6
overlay_gradient_small = overlay_gradient[1:32, 1:32, ]

edge_kernel = matrix(
  c(
    -1,
    -1,
    -1,
    -1,
    8,
    -1,
    -1,
    -1,
    -1
  ),
  nrow = 3,
  byrow = TRUE
)

# generate_2d_* -----------------------------------------------------------------

test_that("generate_2d_disk combinations render correctly", {
  disk_args = expand.grid(
    dim = list(c(11, 11), c(15, 9)),
    radius = list(0.55, 0.85),
    rescale_unity = list(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(dim, radius, rescale_unity) {
      mat = generate_2d_disk(
        dim = dim,
        radius = radius,
        rescale_unity = rescale_unity
      )
      plot_image(mat)
    },
    disk_args,
    plot_prefix = "generate_2d_disk",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

test_that("generate_2d_exponential combinations render correctly", {
  exp_args = expand.grid(
    falloff = list(0.5, 1.5),
    dim = list(c(13, 9)),
    width = list(3, 5),
    rescale_unity = list(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(falloff, dim, width, rescale_unity) {
      mat = generate_2d_exponential(
        falloff = falloff,
        dim = dim,
        width = width,
        rescale_unity = rescale_unity
      )
      plot_image(mat)
    },
    exp_args,
    plot_prefix = "generate_2d_exponential",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

test_that("generate_2d_gaussian combinations render correctly", {
  gauss_args = expand.grid(
    sd = list(0.8, 1.6),
    power = list(1, 2),
    dim = list(c(13, 9)),
    width = list(3),
    rescale_unity = list(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(sd, power, dim, width, rescale_unity) {
      mat = generate_2d_gaussian(
        sd = sd,
        power = power,
        dim = dim,
        width = width,
        rescale_unity = rescale_unity
      )
      plot_image(mat)
    },
    gauss_args,
    plot_prefix = "generate_2d_gaussian",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_boolean_distance -------------------------------------------------------

test_that("render_boolean_distance handles boolean matrices", {
  boolean_inputs = list(
    volcano_small > 150,
    volcano_small < 120
  )

  bool_args = expand.grid(
    boolean = boolean_inputs,
    rescale = list(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(boolean, rescale) {
      mat = render_boolean_distance(boolean = boolean, rescale = rescale)
      plot_image(mat)
    },
    bool_args,
    plot_prefix = "render_boolean_distance",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_clamp -----------------------------------------------------------------

test_that("render_clamp clamps image ranges", {
  clamp_args = expand.grid(
    image = seq_along(all_test_images),
    min_value = list(0, -0.2),
    max_value = list(0.8, 1),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, min_value, max_value) {
      img = render_clamp(
        image = all_test_images[[image]] * 1.2,
        min_value = min_value,
        max_value = max_value,
        preview = FALSE
      )
      plot_image(img)
    },
    clamp_args,
    plot_prefix = "render_clamp",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_bw --------------------------------------------------------------------

test_that("render_bw applies grayscale coefficients", {
  rgb_list = list(
    c(0.2126, 0.7152, 0.0722),
    rep(1 / 3, 3),
    c(0.4, 0.4, 0.2)
  )
  coef_args = expand.grid(
    image = seq_along(all_test_images),
    rgb_coef = seq_along(rgb_list)
  )

  run_tests(
    function(image, rgb_coef) {
      img = render_bw(
        image = all_test_images[[image]],
        rgb_coef = rgb_list[[rgb_coef]],
        preview = FALSE
      )
      plot_image(img)
    },
    coef_args,
    plot_prefix = "render_bw",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_exposure --------------------------------------------------------------

test_that("render_exposure scales luminance", {
  exposure_args = expand.grid(
    image = seq_along(all_test_images),
    exposure = c(-1, 0, 1),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, exposure) {
      img = render_exposure(
        image = all_test_images[[image]],
        exposure = exposure,
        preview = FALSE
      )
      plot_image(render_clamp(img))
    },
    exposure_args,
    plot_prefix = "render_exposure",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_crop ------------------------------------------------------------------

test_that("render_crop crops using limit ranges", {
  width_limits = list(
    c(0, 0.5),
    c(0.25, 0.75),
    c(10, 45)
  )
  height_limits = list(
    c(0, 1),
    c(0.3, 0.8),
    c(5, 40)
  )
  grid = expand.grid(
    i = seq_along(width_limits),
    j = seq_along(height_limits),
    image = seq_along(all_test_images)
  )
  crop_limit_args = data.frame(
    image = grid$image,
    width_limits = I(width_limits[grid$i]),
    height_limits = I(height_limits[grid$j])
  )

  run_tests(
    function(image, width_limits, height_limits) {
      img = render_crop(
        image = all_test_images[[image]],
        width_limits = width_limits,
        height_limits = height_limits,
        preview = FALSE
      )
      plot_image(img)
    },
    crop_limit_args,
    plot_prefix = "render_crop_limits",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

test_that("render_crop crops using center specification", {
  center = list(c(0.5, 0.5), c(32, 32), c(0.75, 0.4))
  grid = expand.grid(i = seq_along(center), image = seq_along(all_test_images))
  crop_limit_args = data.frame(
    center = I(center[grid$i]),
    image = grid$image
  )

  run_tests(
    function(image, center) {
      img = render_crop(
        image = all_test_images[[image]],
        center = center,
        preview = FALSE
      )
      plot_image(img)
    },
    crop_limit_args,
    plot_prefix = "render_crop_center",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_reorient --------------------------------------------------------------

test_that("render_reorient flips and transposes", {
  reorient_args = expand.grid(
    image = seq_along(all_test_images),
    flipx = list(FALSE, TRUE),
    flipy = list(FALSE, TRUE),
    transpose = list(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, flipx, flipy, transpose) {
      img = render_reorient(
        image = all_test_images[[image]],
        flipx = flipx,
        flipy = flipy,
        transpose = transpose,
        preview = FALSE
      )
      plot_image(img)
    },
    reorient_args,
    plot_prefix = "render_reorient",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_resized ---------------------------------------------------------------

test_that("render_resized scales images", {
  mag = list(0.5, 1, 1.5)
  dims = list(NA, c(48, 96))
  method = list("tri", "catmull")

  grid = expand.grid(
    i = seq_along(mag),
    j = seq_along(dims),
    k = seq_along(method),
    image = seq_along(all_test_images)
  )
  resize_args = data.frame(
    mag = I(mag[grid$i]),
    dims = I(dims[grid$j]),
    method = I(method[grid$k]),
    image = grid$image
  )

  run_tests(
    function(image, mag, dims, method) {
      dims_val = if (all(is.na(dims))) NULL else dims
      img = render_resized(
        image = all_test_images[[image]],
        mag = mag,
        dims = dims_val,
        method = method,
        preview = FALSE
      )
      plot_image(render_clamp(img))
    },
    resize_args,
    plot_prefix = "render_resized",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_image_overlay / add_image_overlay -------------------------------------

test_that("render_image_overlay composites overlays", {
  overlay_file = tempfile(fileext = ".png")
  on.exit(unlink(overlay_file), add = TRUE)
  ray_write_image(overlay_gradient_small, overlay_file)

  overlay_inputs = list(
    overlay_gradient,
    overlay_gradient_small,
    overlay_file
  )

  overlay_args = expand.grid(
    image_overlay = seq_along(overlay_inputs),
    rescale_original = list(FALSE, TRUE),
    alpha = list(NA, 0.5),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE,
    image = seq_along(all_test_images)
  )

  run_tests(
    function(image, image_overlay, rescale_original, alpha) {
      img = render_image_overlay(
        image = all_test_images[[image]],
        image_overlay = overlay_inputs[[image_overlay]],
        rescale_original = rescale_original,
        alpha = alpha,
        preview = FALSE
      )
      plot_image(img)
    },
    overlay_args,
    plot_prefix = "render_image_overlay",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_bokeh -----------------------------------------------------------------

test_that("render_bokeh blurs using depth information", {
  bokeh_args = expand.grid(
    focus = list(900, 1050),
    focallength = list(140),
    bokehshape = list("circle", "hex"),
    aberration = list(0.1),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, focus, focallength, bokehshape, aberration) {
      img = render_bokeh(
        image = dragon_small,
        depthmap = dragondepth_small,
        focus = focus,
        focallength = focallength,
        fstop = 4,
        preview = FALSE,
        preview_focus = FALSE,
        bokehshape = bokehshape,
        bokehintensity = 1.5,
        bokehlimit = 0.7,
        rotation = if (identical(bokehshape, "hex")) 15 else 0,
        aberration = aberration,
        progress = FALSE
      )
      plot_image(render_clamp(img))
    },
    bokeh_args,
    plot_prefix = "render_bokeh",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_convolution -----------------------------------------------------------

test_that("render_convolution applies kernels", {
  conv_args = expand.grid(
    image = seq_along(all_test_images),
    kernel = list("gaussian", 2, edge_kernel),
    kernel_dim = list(7),
    kernel_extent = list(2),
    absolute = list(TRUE, FALSE),
    min_value = list(NA, 0.7),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, kernel, kernel_dim, kernel_extent, absolute, min_value) {
      min_arg = if (is.na(min_value)) NULL else min_value
      img = render_convolution(
        image = all_test_images[[image]],
        kernel = "gaussian",
        kernel_dim = 7,
        kernel_extent = 2,
        absolute = TRUE,
        min_value = NA,
        preview = FALSE,
        progress = FALSE
      )
      plot_image(render_clamp(img))
    },
    conv_args,
    plot_prefix = "render_convolution",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_convolution_fft -------------------------------------------------------

test_that("render_convolution_fft applies frequency-domain kernels", {
  fft_args = expand.grid(
    image = seq_along(all_test_images),
    kernel = list("gaussian", edge_kernel),
    kernel_dim = list(c(11, 11)),
    kernel_extent = list(3),
    absolute = list(TRUE, FALSE),
    pad = list(0, 8),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, kernel, kernel_dim, kernel_extent, absolute, pad) {
      img = render_convolution_fft(
        image = all_test_images[[image]],
        kernel = kernel,
        kernel_dim = kernel_dim,
        kernel_extent = kernel_extent,
        absolute = absolute,
        pad = pad,
        preview = FALSE
      )
      plot_image(render_clamp(img))
    },
    fft_args,
    plot_prefix = "render_convolution_fft",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_text_image ------------------------------------------------------------

test_that("render_text_image renders text into images", {
  text_args = expand.grid(
    text = list("Rayimage", "Ray\nImage"),
    size = list(18, 28),
    check_text_height = list(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(text, size, check_text_height) {
      img = render_text_image(
        text = text,
        size = size,
        lineheight = 1.1,
        just = "center",
        background_color = "#f8f9fa",
        background_alpha = 0.8,
        use_ragg = FALSE,
        check_text_width = TRUE,
        check_text_height = check_text_height,
        preview = TRUE,
        filename = NULL
      )
      plot_image(img)
    },
    text_args,
    plot_prefix = "render_text_image",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_title / add_title -----------------------------------------------------

test_that("render_title overlays text using grid", {
  title_args = expand.grid(
    image = seq_along(all_test_images),
    title_text = list("Dragon", "Rayimage Title"),
    title_bar_color = list(NA, "#003366"),
    title_just = list("left", "center"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, title_text, title_bar_color, title_just) {
      img = render_title(
        image = all_test_images[[image]],
        title_text = title_text,
        title_size = 24,
        title_offset = c(12, 12),
        title_lineheight = 1.2,
        title_color = "white",
        title_bar_color = title_bar_color,
        title_bar_alpha = 0.6,
        title_bar_width = NULL,
        title_just = title_just,
        use_magick = FALSE,
        preview = FALSE
      )
      plot_image(img)
    },
    title_args,
    plot_prefix = "render_title",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

test_that("render_title supports magick backend", {
  skip_if_not_installed("magick")

  magick_args = expand.grid(
    image = seq_along(all_test_images),
    title_text = list("Dragon", "Magick Title"),
    title_position = list("northwest", "south"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, title_text, title_position) {
      img = render_title(
        image = all_test_images[[image]],
        title_text = title_text,
        title_size = 20,
        title_bar_color = "#222222",
        title_bar_alpha = 0.7,
        title_position = title_position,
        title_color = "white",
        use_magick = TRUE,
        preview = FALSE
      )
      plot_image(img)
    },
    magick_args,
    plot_prefix = "render_title_magick",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# render_vignette  ----------------------------------------------

test_that("render_vignette adds vignette effect", {
  skip_if_not_installed("magick")

  vignette_args = expand.grid(
    image = seq_along(all_test_images),
    vignette = list(0.5, c(0.8, 1.5)),
    color = list("#000000", "#FFFFFF"),
    radius = list(1.1, 1.4),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, vignette, color, radius) {
      img = render_vignette(
        image = all_test_images[[image]],
        vignette = vignette,
        color = color,
        radius = radius,
        preview = FALSE
      )
      plot_image(img)
    },
    vignette_args,
    plot_prefix = "render_vignette",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# ray_read_iage --------------------------------------------------------------

test_that("ray_read_image ingests various sources", {
  png_path = tempfile(fileext = ".png")
  jpg_path = tempfile(fileext = ".jpg")
  tiff_path = tempfile(fileext = ".tiff")
  on.exit(unlink(c(png_path, jpg_path, tiff_path)), add = TRUE)

  ray_write_image(dragon_small, png_path)
  ray_write_image(dragon_small, jpg_path, quality = 0.7)
  ray_write_image(dragon_small, tiff_path)

  image_inputs = list(
    dragon_small,
    png_path,
    jpg_path,
    tiff_path,
    volcano_small
  )

  read_args = expand.grid(
    image = image_inputs,
    convert_to_array = list(TRUE, FALSE),
    source_linear = list(NA, FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, convert_to_array, source_linear) {
      img = ray_read_image(
        image = image,
        convert_to_array = convert_to_array,
        preview = FALSE,
        source_linear = source_linear
      )
      plot_image(render_clamp(img))
    },
    read_args,
    plot_prefix = "ray_read_image",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# ray_write_image -------------------------------------------------------------

test_that("ray_write_image writes supported formats", {
  write_args = expand.grid(
    image = seq_along(all_test_images),
    fileext = list(".png", ".jpg", ".tiff"),
    clamp = list(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_tests(
    function(image, fileext, clamp) {
      tmp = tempfile(fileext = fileext)
      on.exit(unlink(tmp), add = TRUE)

      write_params = list(
        image = all_test_images[[image]] * 1.4,
        filename = tmp,
        clamp = clamp
      )
      if (tolower(fileext) %in% c(".jpg", ".jpeg")) {
        write_params$quality = 0.6
      }
      do.call(ray_write_image, write_params)

      img = ray_read_image(tmp)
      plot_image(img)
    },
    write_args,
    plot_prefix = "ray_write_image",
    warning_rows = NULL,
    error_rows = NULL,
    list()
  )
})

# get_string_dimensions -------------------------------------------------------

test_that("get_string_dimensions returns expected structure", {
  string_args = expand.grid(
    string = list("Rayimage", "Ray image"),
    font = list("sans"),
    size = list(16, 28),
    align = list("left", "center"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(string_args))) {
    args = string_args[i, ]
    metrics = get_string_dimensions(
      string = args$string[[1]],
      font = args$font[[1]],
      size = args$size[[1]],
      align = args$align[[1]]
    )
    expect_s3_class(metrics, "data.frame")
    expect_true(all(
      c(
        "string",
        "width",
        "height",
        "left_bearing",
        "right_bearing",
        "top_bearing",
        "bottom_bearing",
        "left_border",
        "top_border",
        "pen_x",
        "pen_y"
      ) %in%
        names(metrics)
    ))
    expect_true(all(metrics$width >= 0))
    expect_true(all(metrics$height >= 0))
  }
})

# interpolate_array -----------------------------------------------------------

test_that("interpolate_array supports matrices and images", {
  interp_points = list(
    list(x = c(5, 10.5, 15.2), y = c(6, 12.5, 18.1)),
    list(x = c(20.2, 30.6), y = c(22.7, 28.4))
  )

  interp_args = expand.grid(
    image = seq_along(all_test_images),
    points = interp_points,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(interp_args))) {
    args = interp_args[i, ]
    pts = args$points[[1]]
    img = args$image[[1]]
    image_val = all_test_images[[img]]

    result = interpolate_array(
      image = image_val,
      x = pts$x,
      y = pts$y
    )

    d = dim(image_val)
    if (is.matrix(image_val) && !inherits(image_val, "rayimg")) {
      expect_type(result, "double")
      expect_length(result, length(pts$x))
    } else {
      expect_type(result, "list")
      if (length(d) == 2) {
        expect_type(result, "double")
        expect_length(result, length(pts$x))
      } else if (length(d) == 3) {
        if (d[2] == 2) {
          expect_equal(names(result), c("grey", "alpha"))
          expect_length(result$grey, length(pts$x))
        } else if (d[2] == 3) {
          expect_equal(names(result), c("red", "green", "blue"))
          expect_length(result$red, length(pts$x))
        } else if (d[2] == 4) {
          expect_equal(names(result), c("red", "green", "blue", "alpha"))
          expect_length(result$red, length(pts$x))
        }
      }
    }
  }
})
