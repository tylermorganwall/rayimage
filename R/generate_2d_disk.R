#'@title Generate 2D Disk
#'
#'@description Generates a 2D disk with a gradual falloff.
#'
#'Disk generated using the following formula:
#'
#'\ifelse{html}{\out{(-22.35 * cos(1.68 * r<sup>2</sup>) + 85.91 * sin(1.68 * r<sup>2</sup>) ) * exp(-4.89 * r<sup>2</sup>) +
#'(35.91 * cos(4.99 * r<sup>2</sup>) - 28.87 * sin(4.99 * r<sup>2</sup>)) * exp(-4.71 * r<sup>2</sup>) +
#'(-13.21 * cos(8.24 * r<sup>2</sup>) - 1.57 * sin(8.24 * r<sup>2</sup>)) * exp(-4.05 * r<sup>2</sup>) +
#'(0.50 * cos(11.90 * r<sup>2</sup>) + 1.81 * sin(11.90 * r<sup>2</sup>)) * exp(-2.92 * r<sup>2</sup>) +
#'(0.13 * cos(16.11 * r<sup>2</sup>) - 0.01 * sin(16.11 * r<sup>2</sup>)) * exp(-1.51 * r<sup>2</sup>)}}{(-22.35 cos(1.68 r^2) + 85.91 sin(1.68 r^2) ) exp(-4.89 r^2) +
#'(35.91 cos(4.99 r^2) - 28.87 sin(4.99 r^2)) exp(-4.71 r^2) +
#'(-13.21 cos(8.24 r^2) - 1.57 sin(8.24 r^2)) exp(-4.05 r^2) +
#'(0.50 cos(11.90 r^2) + 1.81 sin(11.90 r^2)) exp(-2.92 r^2) +
#'(0.13 cos(16.11 r^2) - 0.01 sin(16.11 r^2)) exp(-1.51 r^2)}
#'
#'The origin of the coordinate system is the center of the matrix.
#'
#'@param dim Default `c(11, 11)`. The dimensions of the matrix.
#'@param radius Default `1`. Radius of the disk, compared to the dimensions. Should be less than one.
#'@export
#'@examples
#'#if(interactive()){
#'image(generate_2d_disk(101), asp=1)
#'#end}
generate_2d_disk = function(dim = c(11,11), radius = 1) {
  dim = rev(dim)
  mindim = min(dim)
  add_offset_x = FALSE
  add_offset_y = FALSE
  if(length(dim) == 2) {
    if(dim[2] - dim[1] > 0) {
      if(abs(dim[2] - dim[1]) %% 2 != 0) {
        add_offset_x = TRUE
      }
    } else if(dim[2] - dim[1] < 0) {
      if(abs(dim[2] - dim[1]) %% 2 != 0) {
        add_offset_y = TRUE
      }
    }
  }
  disk = generate_disk((1/radius)*1.18, mindim, add_offset_x,add_offset_y)
  if(length(dim) == 2) {
    disk = pad_to_fit(dim,disk)
  }
  disk = (disk - min(disk))/(max(disk)-min(disk))
  disk = disk/sum(disk)
  return(disk)
}
