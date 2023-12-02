#'@title add_padding
#'
#'@description Adds padding to the matrix
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@return Hillshade with edges padded
#'@keywords internal
add_padding = function(heightmap) {
  temp = matrix(0, nrow = nrow(heightmap) + 2, ncol = ncol(heightmap) + 2)
  temp[2:(nrow(temp)-1), 2:(ncol(temp)-1)] = heightmap
  temp[2:(nrow(temp)-1), 1] = heightmap[,1]
  temp[1, 2:(ncol(temp)-1)] = heightmap[1,]
  temp[2:(nrow(temp)-1), ncol(temp)] = heightmap[,ncol(heightmap)]
  temp[nrow(temp), 2:(ncol(temp)-1)] = heightmap[nrow(heightmap),]
  temp[1,1] = temp[1,2]
  temp[1,ncol(temp)] = temp[1,ncol(temp)-1]
  temp[nrow(temp),1] = temp[nrow(temp)-1,2]
  temp[nrow(temp),ncol(temp)] = temp[nrow(temp)-1,ncol(temp)]
  temp
}

#'@title add_multi_padding
#'
#'@description Adds multiple levels padding to the matrix
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param pad Number of padding entries
#'@return Hillshade with edges padded
#'@keywords internal
add_multi_padding = function(heightmap, pad = 1) {
  if(length(dim(heightmap)) == 2) {
    temp = matrix(0, nrow = nrow(heightmap) + 2*pad, ncol = ncol(heightmap) + 2*pad)
    temp[(1+pad):(pad + nrow(heightmap)), (1+pad):(pad+ncol(heightmap))] = heightmap
    temp[(1+pad):(pad + nrow(heightmap)), 1:pad] = heightmap[,1]
    for(i in 1:pad) {
      temp[i, (1+pad):(pad + ncol(heightmap))] = heightmap[1,]
    }
    for(i in (pad+ncol(heightmap)+1):ncol(temp)) {
      temp[(1+pad):(pad+nrow(heightmap)), i] = heightmap[,ncol(heightmap)]
    }
    for(i in (pad+nrow(heightmap)+1):nrow(temp)) {
      temp[i, (1+pad):(pad+ncol(heightmap))] = heightmap[nrow(heightmap),]
    }
    temp[1:pad,1:pad] = heightmap[1,1]
    temp[1:pad,(pad+ncol(heightmap)+1):ncol(temp)] = heightmap[1,ncol(heightmap)]
    temp[(pad+nrow(heightmap)+1):nrow(temp),1:pad] = heightmap[nrow(heightmap),1]
    temp[(pad+nrow(heightmap)+1):nrow(temp),(pad+ncol(heightmap)+1):ncol(temp)] = heightmap[nrow(heightmap),ncol(heightmap)]
    return(temp)
  } else {
    temp_height = list()
    for(i in seq_len(dim(heightmap)[3])) {
      temp_height[[i]] = add_multi_padding(heightmap[,,i], pad)
    }
    temp_array = array(0,dim = c(dim(temp_height[[i]]), dim(heightmap)[3]))
    for(i in seq_len(dim(heightmap)[3])) {
      temp_array[,,i] = temp_height[[i]]
    }
    return(temp_array)
  }
}

#'@title trim_padding
#'
#'@description Trims padding
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param pad Number of padding entries
#'@return Hillshade with edges trimmed
#'@keywords internal
trim_padding = function(heightmap, pad = 1) {
  if(length(dim(heightmap)) == 2) {
    return(heightmap[(1+pad):(nrow(heightmap)-pad), (1+pad):(ncol(heightmap)-pad)])
  } else {
    return(heightmap[(1+pad):(nrow(heightmap)-pad), (1+pad):(ncol(heightmap)-pad),])
  }
}

#'@title pad to fit
#'
#'@description Pads to a rect
#'
#'@return Hillshade with edges trimmed
#'@keywords internal
pad_to_fit = function(dim, kernel) {
  whichdim = which.min(dim)
  if(dim[1] != dim[2]) {
    if(whichdim == 1) {
      extra_rows = dim[2]-dim[1]
      if(extra_rows %% 2 == 0) {
        pad = matrix(0, nrow = extra_rows/2, ncol = ncol(kernel))
        kernel = rbind(pad,kernel,pad)
      } else {
        pad1 = matrix(0, nrow = floor(extra_rows/2)+1, ncol = ncol(kernel))
        pad2 = matrix(0, nrow = floor(extra_rows/2), ncol = ncol(kernel))
        kernel = rbind(pad1,kernel,pad2)
      }
    } else {
      extra_cols = dim[1]-dim[2]
      if(extra_cols %% 2 == 0) {
        pad = matrix(0, nrow = nrow(kernel), ncol = extra_cols/2)
        kernel = cbind(pad,kernel,pad)
      } else {
        pad1 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols/2)+1)
        pad2 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols/2))
        kernel = cbind(pad1,kernel,pad2)
      }
    }
  }
  return(kernel)
}

#'@title expand to fit
#'
#'@description Pads to a rect
#'
#'@return Hillshade with edges trimmed
#'@keywords internal
expand_to_fit = function(dim, kernel) {
  kdim = dim(kernel)
  if(!all(kdim <= dim)) {
    stop("kernel dimensions must be less than or equal to image")
  }
  if(kdim[1] != dim[1]) {
    extra_rows = dim[1] - kdim[1]
    if(extra_rows %% 2 == 0) {
      pad = matrix(0, nrow = extra_rows/2, ncol = ncol(kernel))
      kernel = rbind(pad,kernel,pad)
    } else {
      pad1 = matrix(0, nrow = floor(extra_rows/2)+1, ncol = ncol(kernel))
      pad2 = matrix(0, nrow = floor(extra_rows/2), ncol = ncol(kernel))
      kernel = rbind(pad1,kernel,pad2)
    }
  }
  if(kdim[2] != dim[2]) {
    extra_cols = dim[2] - kdim[2]
    if(extra_cols %% 2 == 0) {
      pad = matrix(0, nrow = nrow(kernel), ncol = extra_cols/2)
      kernel = cbind(pad,kernel,pad)
    } else {
      pad1 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols/2)+1)
      pad2 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols/2))
      kernel = cbind(pad1,kernel,pad2)
    }
  }
  return(kernel)
}

