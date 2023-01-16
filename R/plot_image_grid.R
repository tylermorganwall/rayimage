#'@title Plot Image Grid
#'
#'@description Displays the image in the current device.
#'
#'@param input_list List of array (or matrix) image inputs.
#'@param dim Default `c(1,1)`. Width by height of output grid.
#'@export
#'@examples
#'if(rayimage:::run_documentation()){
#'#Plot the dragon array
#'plot_image_grid(list(dragon, dragon), dim = c(1,2))
#'}
#'if(rayimage:::run_documentation()){
#'plot_image_grid(list(dragon, dragon), dim = c(2,1))
#'}
#'if(rayimage:::run_documentation()){
#'plot_image_grid(list(dragon, dragon), dim = c(2,2))
#'}
#'if(rayimage:::run_documentation()){
#'#Plot alongside the depth matrix
#'dragon_depth_reoriented = render_reorient(dragondepth,
#'                                          transpose = TRUE,
#'                                          flipx = TRUE)/2000
#'plot_image_grid(list(dragondepth/2000, dragon, dragon, dragondepth/2000),
#'                dim = c(2,2))
#'}
plot_image_grid = function(input_list, dim = c(1,1)) {
  if(length(dim) != 2) {
    stop("length of `dim` argument must equal 2")
  }
  if(!inherits(input_list, "list")) {
    stop("`input_list` must be a list of image arrays")
  }
  if(length(input_list) > dim[1]*dim[2]) {
    warning(sprintf("`input_list` of length %i, but grid only has %ix%i=%i slots: truncating list of images",
                    length(input_list), dim[1], dim[2], dim[1]*dim[2]))
    input_list = input_list[1:(dim[1]*dim[2])]
  }
  grob_list = list()
  for(i in seq_len(length(input_list))) {
    grob_list[[i]] = rayimage::plot_image(input_list[[i]], return_grob = TRUE,
                                          new_page = FALSE)
  }
  layout_matrix = matrix(seq_len(dim[1] * dim[2]), nrow = dim[2], ncol = dim[1])
  gridExtra::grid.arrange(grobs=grob_list, layout_matrix = layout_matrix, respect = TRUE)
}
