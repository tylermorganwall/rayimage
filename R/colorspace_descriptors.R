#' @title Build RGB-to-XYZ matrices from primaries + white
#' @param primaries Default `list(r=c(0.713,0.293), g=c(0.165,0.830), b=c(0.128,0.044))`. xy chromaticities (AP1).
#' @param white_xy Default `c(0.32168,0.33767)`. White xy (D60).
#' @return List with 3x3 numeric matrices: `rgb_to_xyz`, `xyz_to_rgb`.
#' @keywords internal
make_rgb_xyz_matrices = function(
	primaries = list(
		r = c(0.713, 0.293),
		g = c(0.165, 0.830),
		b = c(0.128, 0.044)
	),
	white_xy = c(0.32168, 0.33767) # D60
) {
	xy_to_XYZ = function(xy) c(xy[1] / xy[2], 1, (1 - xy[1] - xy[2]) / xy[2])
	Xr = xy_to_XYZ(primaries$r)
	Xg = xy_to_XYZ(primaries$g)
	Xb = xy_to_XYZ(primaries$b)
	M = cbind(Xr, Xg, Xb)
	W = xy_to_XYZ(white_xy)
	S = solve(M, W)
	rgb_to_xyz = M %*% diag(S)
	xyz_to_rgb = solve(rgb_to_xyz)
	list(rgb_to_xyz = rgb_to_xyz, xyz_to_rgb = xyz_to_rgb)
}

#' @title Build a colorspace descriptor
#' @param name Default `"ACEScg"`.
#' @param primaries Default AP1.
#' @param white_xy Default D60.
#' @return list(name, rgb_to_xyz, xyz_to_rgb, white_xyz, white_name)
#' @keywords internal
make_colorspace = function(
	name = "ACEScg",
	primaries = list(
		r = c(0.713, 0.293),
		g = c(0.165, 0.830),
		b = c(0.128, 0.044)
	),
	white_xy = c(0.32168, 0.33767), # D60
	white_name = "D60"
) {
	mats = make_rgb_xyz_matrices(primaries = primaries, white_xy = white_xy)
	xy_to_XYZ = function(xy) c(xy[1] / xy[2], 1, (1 - xy[1] - xy[2]) / xy[2])
	list(
		name = name,
		primaries = primaries,
		rgb_to_xyz = mats$rgb_to_xyz,
		xyz_to_rgb = mats$xyz_to_rgb,
		white_xyz = xy_to_XYZ(white_xy),
		white_name = white_name
	)
}

#' Prebuilt RGB Working/Display Color Spaces
#'
#' @title Prebuilt RGB Working/Display Color Spaces
#'
#' @description
#' These objects are **color space descriptors** used by `rayimage` to tag images
#' and to convert between RGB spaces. Each descriptor is a named list with:
#'
#' - `name`: Character. Human-readable name of the space.
#' - `primaries`: List with `r`, `g`, `b` entries giving xy chromaticities.
#' - `rgb_to_xyz`: 3x3 numeric matrix (linear RGB -> CIE XYZ, Y=1 white).
#' - `xyz_to_rgb`: 3x3 numeric matrix (CIE XYZ -> linear RGB).
#' - `white_xyz`: Length-3 numeric XYZ of the reference white (normalized s.t. Y = 1).
#' - `white_name`: Character label for the white (e.g., `"D60"`, `"D65"`).
#'
#' These are used as:
#' - `attr(img, "colorspace")`: the current working/display space for an image.
#' - `attr(img, "white_current")`: the current assumed scene/display white (XYZ, Y=1).
#'
#' @details
#' - **CS_ACESCG**: ACEScg (AP1 primaries, D60, scene-linear). Recommended working space.
#' - **CS_SRGB**: sRGB/Rec.709 (D65). Typical display space; used for PNG/JPEG/TIFF output.
#' - **CS_P3D65**: Display P3 (D65). Wide-gamut display space.
#' - **CS_BT2020**: BT.2020 (D65). Very wide-gamut; common for HDR mastering.
#' - **CS_ADOBE**: Adobe RGB (D65). Photo workflows.
#'
#' @seealso
#' - [render_convert_colorspace()] to convert images between spaces.
#' - [render_white_balance()] for Bradford CAT within a working space.
#' - [plot_image()], [ray_write_image()] for display/output conversion.
#'
#' @examples
#' if (run_documentation()) {
#'   # Tag a raw array as sRGB, then convert to ACEScg:
#'   arr = array(runif(64*64*4), dim = c(64, 64, 4))
#'   ri  = ray_read_image(arr, assume_colorspace = CS_SRGB, assume_white = CS_SRGB$white_xyz)
#'   riA = render_convert_colorspace(ri, to_mats = CS_ACESCG)
#'   plot_image(riA) # display-converted to sRGB automatically
#'
#'   # Convert an sRGB JPEG to ACEScg on ingest:
#'   # img = ray_read_image("photo.jpg", normalize = FALSE)
#'   # img_aces = render_convert_colorspace(img, to_mats = CS_ACESCG)
#' }
#'
#' @name colorspace_descriptors
NULL

#' @rdname colorspace_descriptors
#' @export
CS_ACESCG = make_colorspace(
	name = "ACEScg",
	primaries = list(
		r = c(0.713, 0.293),
		g = c(0.165, 0.830),
		b = c(0.128, 0.044)
	),
	white_xy = c(0.32168, 0.33767), # D60
	white_name = "D60"
)

#' @rdname colorspace_descriptors
#' @export
CS_SRGB = make_colorspace(
	name = "sRGB",
	primaries = list(
		r = c(0.64, 0.33),
		g = c(0.30, 0.60),
		b = c(0.15, 0.06)
	),
	white_xy = c(0.3127, 0.3290), # D65
	white_name = "D65"
)

#' @rdname colorspace_descriptors
#' @export
CS_P3D65 = make_colorspace(
	name = "P3-D65",
	primaries = list(
		r = c(0.680, 0.320),
		g = c(0.265, 0.690),
		b = c(0.150, 0.060)
	),
	white_xy = c(0.3127, 0.3290), # D65
	white_name = "D65"
)

#' @rdname colorspace_descriptors
#' @export
CS_BT2020 = make_colorspace(
	name = "BT.2020",
	primaries = list(
		r = c(0.708, 0.292),
		g = c(0.170, 0.797),
		b = c(0.131, 0.046)
	),
	white_xy = c(0.3127, 0.3290), # D65
	white_name = "D65"
)

#' @rdname colorspace_descriptors
#' @export
CS_ADOBE = make_colorspace(
	name = "AdobeRGB",
	primaries = list(
		r = c(0.64, 0.33),
		g = c(0.21, 0.71),
		b = c(0.15, 0.06)
	),
	white_xy = c(0.3127, 0.3290), # D65
	white_name = "D65"
)
