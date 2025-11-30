#include <vector>
#include <memory>
#include <string>
#include <algorithm>
#include "Rcpp.h"
#include <filesystem>

#define STB_IMAGE_IMPLEMENTATION
#ifndef STBIMAGEH
#define STBIMAGEH
#include "ext/stb_image.h"
#endif

namespace fs = std::filesystem;

std::string StandardizeFilename(const std::string& filename) {
  std::string result = filename;
  std::transform(result.begin(), result.end(), result.begin(),
                 [](unsigned char c) { return std::tolower(c); });
  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector load_image_stb(const std::string& filename,
                                   int& width,
                                   int& height,
                                   int& channels,
                                   int desired_channels) {
  width = height = channels = 0;

  std::string standardizedFilename = StandardizeFilename(filename);
  fs::path filepath(standardizedFilename);

  float* data = stbi_loadf(filename.c_str(), &width, &height, &channels, desired_channels);
  if (!data) {
    throw std::runtime_error(
        "Loading of '" + filename + "' (float) failed: " +
          std::string(stbi_failure_reason()) +
          " -- nx/ny/channels: " + std::to_string(width) + "/" +
          std::to_string(height) + "/" + std::to_string(channels));
  }

  if (desired_channels != 0) {
    channels = desired_channels;
  }

  if (width == 0 || height == 0 || channels == 0) {
    stbi_image_free(data);
    throw std::runtime_error("Could not find " + filename);
  }

  // Enforce RGBA
  if (channels != 4) {
    stbi_image_free(data);
    throw std::runtime_error(
        "Expected 4 channels (RGBA) but got " + std::to_string(channels) +
          ". Call with desired_channels = 4.");
  }

  // Allocate R array: dim = c(height, width, 4) (RGBA in the 3rd dimension)
  std::size_t n_pixels = static_cast<std::size_t>(width) *
    static_cast<std::size_t>(height);
  std::size_t n_vals = n_pixels * 4;

  Rcpp::NumericVector arr_return(n_vals);
  Rcpp::IntegerVector dim(3);
  dim[0] = height;   // rows (y)
  dim[1] = width;    // cols (x)
  dim[2] = 4;        // RGBA
  arr_return.attr("dim") = dim;

  // Reorder from STB layout (y, x, c; row-major) to R layout (y, x, c; column-major)
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      for (int c = 0; c < 4; ++c) {
        std::size_t stb_idx =
          (static_cast<std::size_t>(y) * width + x) * 4 + c;

        // R index: y + height * (x + width * c)
        std::size_t r_idx =
          static_cast<std::size_t>(y) +
          static_cast<std::size_t>(height) *
          (x + static_cast<std::size_t>(width) * c);

        arr_return[r_idx] = static_cast<double>(data[stb_idx]);
      }
    }
  }

  stbi_image_free(data);
  return arr_return;
}
