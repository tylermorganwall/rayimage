#' @title Render Color Correction Matrix (3x3, linear RGB)
#'
#' @description Apply a technical/look 3x3 matrix to an image
#'
#' @param image Default `NULL`. 3-layer RGB/4-layer RGBA array, `rayimg`, or filename.
#' @param matrix Default `diag(3)`. 3x3 numeric matrix.
#' @param filename Default `NULL`. Output path.
#' @param preview Default `FALSE`. If `TRUE`, display the image.
#' @return A `rayimg` RGBA array.
#' @export
#' @examples
#' # We will start with an image that's too warm--we want to correct it to
#' # match the color of the second as closely as possible.
#' dragon_D50 = render_white_balance(dragon, target_white = "D50", bake = TRUE)
#' dragon_D75 = render_white_balance(dragon, target_white = "D75", bake = TRUE)
#' plot_image(dragon_D50)
#' plot_image(dragon_D75)
#'
#' # Fucntion to fit a color correction matrix
#' fit_cc_matrix = function(src, tgt) {
#' 	stopifnot(ncol(src) == 3, ncol(tgt) == 3, nrow(src) == nrow(tgt))
#' 	M_t = apply(tgt, 2, function(y) qr.solve(src, y))
#' 	t(M_t)
#' }
#'
#' # Sample N RGBs from an image for fit
#' rand_samples = function(img, n = 5000, seed=1) {
#' 	set.seed(seed)
#' 	d = dim(img)
#' 	ys = sample.int(d[1], n, TRUE); xs = sample.int(d[2], n, TRUE)
#' 	cbind(img[cbind(ys,xs,1)], img[cbind(ys,xs,2)], img[cbind(ys,xs,3)])
#' }
#'
#' S = rand_samples(dragon_D50)
#' T = rand_samples(dragon_D75)
#'
#' M = fit_cc_matrix(S, T)
#' # Optionally, regularize toward identity to avoid overfitting
#' #  M = 0.8 * M + 0.2 * diag(3)
#'
#' updated_source = render_color_correction(dragon_D50, matrix = M)
#'
#' # Matching the old image (grey) to the target image (orange),
#' # giving a new image (black).
#' plot(S,pch=16,cex=0.5,col="grey")
#' points(T,pch=16,cex=0.5,col="orange")
#' points(rand_samples(updated_source,seed=2),pch=16,cex=0.5,col="black")
#'
#' # Plot the images
#' plot_image(render_title(dragon_D50, "OG Image",
#'            title_color = "white", title_size = 12))
#' plot_image(render_title(dragon_D75, "Target Image",
#'            title_color = "white", title_size = 12))
#' plot_image(render_title(updated_source, "Corrected OG Image",
#'            title_color = "white", title_size = 12))
render_color_correction = function(
	image,
	matrix = diag(3),
	filename = NULL,
	preview = FALSE
) {
	stopifnot(is.matrix(matrix), all(dim(matrix) == c(3, 3)), is.numeric(matrix))
	src = ray_read_image(image, convert_to_array = TRUE, normalize = FALSE)
	imagetype = attr(src, "filetype")
	img_source_linear = attr(src, "source_linear")
	colorspace = attr(src, "colorspace")
	white_current = attr(src, "white_current")

	if (!isTRUE(attr(src, "source_linear"))) {
		warning(
			"render_color_correction(): input is not linear; convert with render_gamma_linear(..., TRUE) first."
		)
	}
	d = dim(src)
	if (length(d) != 3L) {
		return(src)
	}
	out = apply_color_matrix(src, matrix)
	out[,, 1:3][out[,, 1:3] < 0] = 0
	out = ray_read_image(
		out,
		filetype = imagetype,
		source_linear = img_source_linear,
		assume_colorspace = colorspace,
		assume_white = white_current
	)
	handle_image_output(out, filename = filename, preview = preview)
}
