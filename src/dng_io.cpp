#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

#include <Rcpp.h>

#define TINY_DNG_LOADER_IMPLEMENTATION
#define TINY_DNG_LOADER_NO_STB_IMAGE_INCLUDE
#define TINY_DNG_LOADER_USE_STB_IMAGE
#include "ext/stb_image.h"
#include "tinydng/tiny_dng_loader.h"

#define TINY_DNG_WRITER_IMPLEMENTATION
#include "tinydng/tiny_dng_writer.h"

namespace {

bool HasListElement(const Rcpp::List& list, const char* name) {
  return list.containsElementNamed(name);
}

Rcpp::NumericVector GetNumericVector(const Rcpp::List& list, const char* name) {
  if (!HasListElement(list, name)) {
    return Rcpp::NumericVector();
  }
  return Rcpp::as<Rcpp::NumericVector>(list[name]);
}

Rcpp::IntegerVector GetIntegerVector(const Rcpp::List& list, const char* name) {
  if (!HasListElement(list, name)) {
    return Rcpp::IntegerVector();
  }
  return Rcpp::as<Rcpp::IntegerVector>(list[name]);
}

bool HasNaNumeric(const Rcpp::NumericVector& vec, int required) {
  if (vec.size() < required) {
    return true;
  }
  for (int i = 0; i < required; ++i) {
    if (Rcpp::NumericVector::is_na(vec[i])) {
      return true;
    }
  }
  return false;
}

bool HasNaInteger(const Rcpp::IntegerVector& vec, int required) {
  if (vec.size() < required) {
    return true;
  }
  for (int i = 0; i < required; ++i) {
    if (vec[i] == NA_INTEGER) {
      return true;
    }
  }
  return false;
}

double Clamp01(double v) {
  if (v < 0.0) return 0.0;
  if (v > 1.0) return 1.0;
  return v;
}

double DefaultWhiteLevel(int bits_per_sample) {
  if (bits_per_sample <= 0) {
    return 0.0;
  }
  return std::pow(2.0, static_cast<double>(bits_per_sample)) - 1.0;
}

const int kTagLinearizationTable = 50712;
const int kTagBaselineExposure = 50730;

void AppendWarn(std::string* warn, const std::string& msg) {
  if (!warn || msg.empty()) {
    return;
  }
  if (!warn->empty()) {
    (*warn) += " ";
  }
  (*warn) += msg;
}

const tinydng::FieldData* FindCustomField(const tinydng::DNGImage& image,
                                          int tag) {
  for (const auto& field : image.custom_fields) {
    if (field.tag == tag) {
      return &field;
    }
  }
  return nullptr;
}

const tinydng::FieldData* FindCustomFieldAny(
    const std::vector<tinydng::DNGImage>& images,
    int tag,
    std::size_t* image_idx) {
  for (std::size_t i = 0; i < images.size(); ++i) {
    const tinydng::FieldData* field = FindCustomField(images[i], tag);
    if (field) {
      if (image_idx) {
        *image_idx = i;
      }
      return field;
    }
  }
  return nullptr;
}

bool ReadCustomShort(const tinydng::FieldData& field, uint16_t* value) {
  if ((field.type != tinydng::TYPE_SHORT) &&
      (field.type != tinydng::TYPE_SSHORT)) {
    return false;
  }
  if (field.data.size() < sizeof(uint16_t)) {
    return false;
  }
  uint16_t out = 0;
  std::memcpy(&out, field.data.data(), sizeof(uint16_t));
  if (value) {
    *value = out;
  }
  return true;
}

bool ReadCustomRational(const tinydng::FieldData& field,
                        double* value,
                        bool signed_value) {
  if (field.data.size() < sizeof(uint32_t) * 2) {
    return false;
  }
  uint32_t num_u = 0;
  uint32_t den_u = 0;
  std::memcpy(&num_u, field.data.data(), sizeof(uint32_t));
  std::memcpy(&den_u, field.data.data() + sizeof(uint32_t), sizeof(uint32_t));
  if (den_u == 0) {
    return false;
  }
  double num = signed_value ? static_cast<double>(static_cast<int32_t>(num_u))
                            : static_cast<double>(num_u);
  double den = signed_value ? static_cast<double>(static_cast<int32_t>(den_u))
                            : static_cast<double>(den_u);
  if (value) {
    *value = num / den;
  }
  return true;
}

std::vector<uint16_t> ExtractLinearizationTable(
    const tinydng::FieldData& field) {
  std::vector<uint16_t> table;
  if (field.type != tinydng::TYPE_SHORT) {
    return table;
  }
  if (field.data.size() < sizeof(uint16_t)) {
    return table;
  }
  std::size_t count = field.data.size() / sizeof(uint16_t);
  table.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    uint16_t value = 0;
    std::memcpy(&value, field.data.data() + i * sizeof(uint16_t),
                sizeof(uint16_t));
    table.push_back(value);
  }
  return table;
}

std::vector<double> RowMajorToR(const std::vector<double>& values,
                                int width,
                                int height,
                                int spp) {
  std::vector<double> out(values.size());
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      for (int c = 0; c < spp; ++c) {
        std::size_t src_idx = (static_cast<std::size_t>(y) *
          static_cast<std::size_t>(width) + static_cast<std::size_t>(x)) *
          static_cast<std::size_t>(spp) + static_cast<std::size_t>(c);
        std::size_t dst_idx = static_cast<std::size_t>(y) +
          static_cast<std::size_t>(height) *
          (static_cast<std::size_t>(x) + static_cast<std::size_t>(width) *
          static_cast<std::size_t>(c));
        out[dst_idx] = values[src_idx];
      }
    }
  }
  return out;
}

std::vector<double> RToRowMajor(const Rcpp::NumericVector& pixels,
                                int width,
                                int height,
                                int spp) {
  std::vector<double> out(static_cast<std::size_t>(width) *
    static_cast<std::size_t>(height) * static_cast<std::size_t>(spp));
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      for (int c = 0; c < spp; ++c) {
        std::size_t src_idx = static_cast<std::size_t>(y) +
          static_cast<std::size_t>(height) *
          (static_cast<std::size_t>(x) + static_cast<std::size_t>(width) *
          static_cast<std::size_t>(c));
        std::size_t dst_idx = (static_cast<std::size_t>(y) *
          static_cast<std::size_t>(width) + static_cast<std::size_t>(x)) *
          static_cast<std::size_t>(spp) + static_cast<std::size_t>(c);
        out[dst_idx] = pixels[src_idx];
      }
    }
  }
  return out;
}

bool ActiveAreaValid(const int active_area[4], int width, int height) {
  int top = active_area[0];
  int left = active_area[1];
  int bottom = active_area[2];
  int right = active_area[3];
  if (top < 0 || left < 0 || bottom <= top || right <= left) {
    return false;
  }
  if (bottom > height || right > width) {
    return false;
  }
  return true;
}

bool ApplyActiveAreaCrop(const std::vector<double>& values,
                         int width,
                         int height,
                         int spp,
                         const int active_area[4],
                         std::vector<double>* out_values,
                         int* out_width,
                         int* out_height) {
  if (!ActiveAreaValid(active_area, width, height)) {
    return false;
  }
  int top = active_area[0];
  int left = active_area[1];
  int bottom = active_area[2];
  int right = active_area[3];
  int crop_height = bottom - top;
  int crop_width = right - left;

  std::vector<double> cropped(static_cast<std::size_t>(crop_width) *
    static_cast<std::size_t>(crop_height) * static_cast<std::size_t>(spp));

  for (int y = 0; y < crop_height; ++y) {
    int src_y = y + top;
    for (int x = 0; x < crop_width; ++x) {
      int src_x = x + left;
      for (int c = 0; c < spp; ++c) {
        std::size_t src_idx = (static_cast<std::size_t>(src_y) *
          static_cast<std::size_t>(width) + static_cast<std::size_t>(src_x)) *
          static_cast<std::size_t>(spp) + static_cast<std::size_t>(c);
        std::size_t dst_idx = (static_cast<std::size_t>(y) *
          static_cast<std::size_t>(crop_width) + static_cast<std::size_t>(x)) *
          static_cast<std::size_t>(spp) + static_cast<std::size_t>(c);
        cropped[dst_idx] = values[src_idx];
      }
    }
  }

  if (out_values) {
    *out_values = std::move(cropped);
  }
  if (out_width) {
    *out_width = crop_width;
  }
  if (out_height) {
    *out_height = crop_height;
  }
  return true;
}

double SampleGainMap(const tinydng::GainMap& gmap,
                     int x,
                     int y,
                     int plane,
                     std::string* warn) {
  if (gmap.map_points_v == 0 || gmap.map_points_h == 0 ||
      gmap.map_planes == 0) {
    AppendWarn(warn, "GainMap has invalid dimensions.");
    return 1.0;
  }
  if (gmap.map_spacing_h <= 0.0 || gmap.map_spacing_v <= 0.0) {
    AppendWarn(warn, "GainMap spacing is invalid.");
    return 1.0;
  }
  double fx = (static_cast<double>(x) - gmap.map_origin_h) / gmap.map_spacing_h;
  double fy = (static_cast<double>(y) - gmap.map_origin_v) / gmap.map_spacing_v;

  if (fx < 0.0) fx = 0.0;
  if (fy < 0.0) fy = 0.0;
  double max_x = static_cast<double>(gmap.map_points_h - 1);
  double max_y = static_cast<double>(gmap.map_points_v - 1);
  if (fx > max_x) fx = max_x;
  if (fy > max_y) fy = max_y;

  int x0 = static_cast<int>(std::floor(fx));
  int y0 = static_cast<int>(std::floor(fy));
  int x1 = std::min(x0 + 1, static_cast<int>(gmap.map_points_h - 1));
  int y1 = std::min(y0 + 1, static_cast<int>(gmap.map_points_v - 1));
  double tx = fx - static_cast<double>(x0);
  double ty = fy - static_cast<double>(y0);

  auto idx = [&](int gx, int gy, int gp) {
    return (static_cast<std::size_t>(gy) *
      static_cast<std::size_t>(gmap.map_points_h) +
      static_cast<std::size_t>(gx)) *
      static_cast<std::size_t>(gmap.map_planes) +
      static_cast<std::size_t>(gp);
  };

  if (plane < 0 || plane >= static_cast<int>(gmap.map_planes)) {
    return 1.0;
  }

  float g00 = gmap.pixels[idx(x0, y0, plane)];
  float g10 = gmap.pixels[idx(x1, y0, plane)];
  float g01 = gmap.pixels[idx(x0, y1, plane)];
  float g11 = gmap.pixels[idx(x1, y1, plane)];

  double gx0 = g00 + (g10 - g00) * tx;
  double gx1 = g01 + (g11 - g01) * tx;
  return gx0 + (gx1 - gx0) * ty;
}

bool ApplyGainMaps(const std::vector<tinydng::GainMap>& gainmaps,
                   int width,
                   int height,
                   int spp,
                   std::vector<double>* values,
                   std::string* warn) {
  if (!values || gainmaps.empty()) {
    return false;
  }

  bool applied = false;
  for (const auto& gmap : gainmaps) {
    if (gmap.pixels.empty()) {
      continue;
    }

    int top = static_cast<int>(gmap.top);
    int left = static_cast<int>(gmap.left);
    int bottom = static_cast<int>(gmap.bottom);
    int right = static_cast<int>(gmap.right);

    if (bottom <= top || right <= left) {
      top = 0;
      left = 0;
      bottom = height;
      right = width;
    }

    int plane_start = static_cast<int>(gmap.plane);
    int plane_count = static_cast<int>(gmap.planes);
    if (plane_count <= 0) {
      plane_start = 0;
      plane_count = spp;
    }

    for (int y = top; y < bottom; ++y) {
      if (y < 0 || y >= height) {
        continue;
      }
      for (int x = left; x < right; ++x) {
        if (x < 0 || x >= width) {
          continue;
        }
        for (int c = 0; c < spp; ++c) {
          if (c < plane_start || c >= plane_start + plane_count) {
            continue;
          }
          int plane = (gmap.map_planes == 1)
            ? 0
            : (c - plane_start);
          double gain = SampleGainMap(gmap, x, y, plane, warn);
          std::size_t idx = (static_cast<std::size_t>(y) *
            static_cast<std::size_t>(width) + static_cast<std::size_t>(x)) *
            static_cast<std::size_t>(spp) + static_cast<std::size_t>(c);
          (*values)[idx] *= gain;
        }
      }
    }
    applied = true;
  }
  return applied;
}

}  // namespace

// [[Rcpp::export]]
Rcpp::List read_dng_cpp(const std::string& filename,
                        bool normalize = true,
                        std::string pick = "largest") {
  std::vector<tinydng::FieldInfo> custom_fields;
  std::vector<tinydng::DNGImage> images;
  std::string warn;
  std::string err;

  auto add_custom_field = [&](int tag, tinydng::DataType type, const char* name) {
    tinydng::FieldInfo field;
    field.tag = tag;
    field.type = type;
    field.name = name;
    custom_fields.push_back(field);
  };

  add_custom_field(kTagLinearizationTable, tinydng::TYPE_SHORT,
                   "LinearizationTable");
  add_custom_field(kTagBaselineExposure, tinydng::TYPE_SRATIONAL,
                   "BaselineExposure");
  add_custom_field(tinydng::TAG_EXPOSURE_TIME, tinydng::TYPE_RATIONAL,
                   "ExposureTime");
  add_custom_field(tinydng::TAG_ISO, tinydng::TYPE_SHORT, "ISOSpeedRatings");
  add_custom_field(tinydng::TAG_F_NUMBER, tinydng::TYPE_RATIONAL, "FNumber");

  bool ok = tinydng::LoadDNG(filename.c_str(), custom_fields, &images, &warn, &err);
  if (!ok) {
    throw std::runtime_error(err.empty() ? "Failed to load DNG." : err);
  }

  std::string pick_lower = pick;
  std::transform(pick_lower.begin(), pick_lower.end(), pick_lower.begin(),
                 [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
  if (pick_lower != "largest") {
    throw std::runtime_error("Only pick = \"largest\" is supported.");
  }

  std::size_t best_idx = images.size();
  std::size_t best_pixels = 0;
  for (std::size_t i = 0; i < images.size(); ++i) {
    const tinydng::DNGImage& img = images[i];
    if (img.data.empty()) {
      continue;
    }
    std::size_t pixels = static_cast<std::size_t>(img.width) *
      static_cast<std::size_t>(img.height);
    if (pixels > best_pixels) {
      best_pixels = pixels;
      best_idx = i;
    }
  }

  if (best_idx == images.size()) {
    std::string msg = "No DNG image data found.";
    if (!warn.empty()) {
      msg += " Warning: " + warn;
    }
    if (!err.empty()) {
      msg += " Error: " + err;
    }
    throw std::runtime_error(msg);
  }

  const tinydng::DNGImage& image = images[best_idx];
  short orientation = image.orientation;
  if (orientation <= 0 || orientation == 1) {
    for (std::size_t i = 0; i < images.size(); ++i) {
      short candidate = images[i].orientation;
      if (candidate > 1 && candidate <= 8) {
        orientation = candidate;
        break;
      }
    }
  }
  if (image.width <= 0 || image.height <= 0 || image.samples_per_pixel <= 0) {
    throw std::runtime_error("Invalid DNG image dimensions.");
  }

  int width = image.width;
  int height = image.height;
  int spp = image.samples_per_pixel;
  int bps = image.bits_per_sample;
  bool processing = normalize;

  std::string warn_out = warn;
  bool linearization_present = false;
  bool linearization_applied = false;
  bool active_area_applied = false;
  bool opcodes_present = !image.opcodelist2_gainmap.empty();
  bool opcodes_applied = false;
  bool baseline_exposure_seen = false;
  bool baseline_exposure_undone = false;
  double baseline_exposure = NA_REAL;
  double exposure_time = NA_REAL;
  double iso = NA_REAL;
  double f_number = NA_REAL;
  bool exposure_normalized_applied = false;

  std::vector<uint16_t> linearization_table;
  const tinydng::FieldData* linear_field =
    FindCustomField(image, kTagLinearizationTable);
  if (!linear_field) {
    linear_field = FindCustomFieldAny(images, kTagLinearizationTable, nullptr);
  }
  if (linear_field) {
    linearization_present = true;
    linearization_table = ExtractLinearizationTable(*linear_field);
  }

  const tinydng::FieldData* baseline_field =
    FindCustomField(image, kTagBaselineExposure);
  if (!baseline_field) {
    baseline_field = FindCustomFieldAny(images, kTagBaselineExposure, nullptr);
  }
  if (baseline_field) {
    double value = 0.0;
    if (ReadCustomRational(*baseline_field, &value, true)) {
      baseline_exposure = value;
      baseline_exposure_seen = true;
    } else {
      AppendWarn(&warn_out, "BaselineExposure tag present but could not be read.");
    }
  }

  const tinydng::FieldData* exposure_field =
    FindCustomField(image, tinydng::TAG_EXPOSURE_TIME);
  if (!exposure_field) {
    exposure_field = FindCustomFieldAny(images, tinydng::TAG_EXPOSURE_TIME,
                                        nullptr);
  }
  if (exposure_field) {
    double value = 0.0;
    if (ReadCustomRational(*exposure_field, &value, false)) {
      exposure_time = value;
    } else {
      AppendWarn(&warn_out, "ExposureTime tag present but could not be read.");
    }
  }

  const tinydng::FieldData* iso_field =
    FindCustomField(image, tinydng::TAG_ISO);
  if (!iso_field) {
    iso_field = FindCustomFieldAny(images, tinydng::TAG_ISO, nullptr);
  }
  if (iso_field) {
    uint16_t value = 0;
    if (ReadCustomShort(*iso_field, &value)) {
      iso = static_cast<double>(value);
    } else {
      AppendWarn(&warn_out, "ISOSpeedRatings tag present but could not be read.");
    }
  }

  const tinydng::FieldData* fnum_field =
    FindCustomField(image, tinydng::TAG_F_NUMBER);
  if (!fnum_field) {
    fnum_field = FindCustomFieldAny(images, tinydng::TAG_F_NUMBER, nullptr);
  }
  if (fnum_field) {
    double value = 0.0;
    if (ReadCustomRational(*fnum_field, &value, false)) {
      f_number = value;
    }
  }

  int bytes_per_sample = 0;
  if (image.sample_format == tinydng::SAMPLEFORMAT_UINT) {
    if (bps <= 8) {
      bytes_per_sample = 1;
    } else if (bps <= 16) {
      bytes_per_sample = 2;
    } else {
      throw std::runtime_error("Unsupported integer bit depth.");
    }
  } else if (image.sample_format == tinydng::SAMPLEFORMAT_IEEEFP) {
    if (bps == 32) {
      bytes_per_sample = 4;
    } else {
      throw std::runtime_error("Unsupported float bit depth.");
    }
  } else {
    throw std::runtime_error("Unsupported sample format.");
  }

  std::size_t n_samples = static_cast<std::size_t>(width) *
    static_cast<std::size_t>(height) * static_cast<std::size_t>(spp);
  if (image.data.size() < n_samples * static_cast<std::size_t>(bytes_per_sample)) {
    throw std::runtime_error("DNG data buffer is smaller than expected.");
  }

  std::vector<double> values(n_samples);

  std::vector<double> black_levels(static_cast<std::size_t>(spp), 0.0);
  std::vector<double> white_levels(static_cast<std::size_t>(spp),
                                   DefaultWhiteLevel(bps));
  double default_white = DefaultWhiteLevel(bps);
  for (int c = 0; c < spp; ++c) {
    if (c < 4) {
      double black = static_cast<double>(image.black_level[c]);
      double white = static_cast<double>(image.white_level[c]);
      if (white > black) {
        black_levels[c] = black;
        white_levels[c] = white;
      } else {
        black_levels[c] = 0.0;
        white_levels[c] = default_white;
      }
    } else {
      black_levels[c] = 0.0;
      white_levels[c] = default_white;
    }
  }

  // Processing order: linearization -> black/white normalize -> gain map ->
  // baseline exposure handling -> active area crop -> exposure normalization.
  const unsigned char* data = image.data.data();
  bool linearization_warned = false;
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      for (int c = 0; c < spp; ++c) {
        std::size_t sample_idx = (static_cast<std::size_t>(y) *
          static_cast<std::size_t>(width) + static_cast<std::size_t>(x)) *
          static_cast<std::size_t>(spp) + static_cast<std::size_t>(c);
        std::size_t offset = sample_idx * static_cast<std::size_t>(bytes_per_sample);

        double raw_value = 0.0;
        uint32_t int_value = 0;
        if (image.sample_format == tinydng::SAMPLEFORMAT_UINT) {
          if (bytes_per_sample == 1) {
            int_value = static_cast<uint32_t>(data[offset]);
            if (bps < 8) {
              int_value &= ((1u << bps) - 1u);
            }
          } else {
            int_value = static_cast<uint32_t>(data[offset]) |
              (static_cast<uint32_t>(data[offset + 1]) << 8);
            if (bps < 16) {
              int_value &= ((1u << bps) - 1u);
            }
          }
          raw_value = static_cast<double>(int_value);
          if (processing && linearization_present &&
              linearization_table.size() > 1) {
            if (int_value < linearization_table.size()) {
              raw_value = static_cast<double>(linearization_table[int_value]);
              linearization_applied = true;
            } else if (!linearization_warned) {
              AppendWarn(&warn_out,
                         "Linearization table is smaller than sample range.");
              linearization_warned = true;
            }
          }
        } else {
          float value = 0.0f;
          std::memcpy(&value, data + offset, sizeof(float));
          raw_value = static_cast<double>(value);
          if (processing && linearization_present && !linearization_warned) {
            AppendWarn(&warn_out,
                       "Linearization table present but samples are float.");
            linearization_warned = true;
          }
        }

        double out_value = raw_value;
        if (processing) {
          double denom = white_levels[c] - black_levels[c];
          if (denom <= 0.0) {
            out_value = 0.0;
          } else {
            out_value = (raw_value - black_levels[c]) / denom;
          }
          out_value = Clamp01(out_value);
        }

        values[sample_idx] = out_value;
      }
    }
  }

  if (linearization_present && !linearization_applied && processing &&
      linearization_table.size() <= 1) {
    AppendWarn(&warn_out,
               "Linearization table present but not available for use.");
  }

  if (processing && opcodes_present) {
    opcodes_applied = ApplyGainMaps(image.opcodelist2_gainmap, width, height,
                                    spp, &values, &warn_out);
    if (!opcodes_applied) {
      AppendWarn(&warn_out, "OpcodeList2 gain maps present but not applied.");
    }
  } else if (opcodes_present && !processing) {
    AppendWarn(&warn_out, "OpcodeList2 gain maps present but skipped.");
  }

  if (baseline_exposure_seen && baseline_exposure != 0.0) {
    AppendWarn(&warn_out,
               "BaselineExposure present; value not applied or undone.");
  }

  if (image.has_active_area) {
    int active_area_vals[4] = {
      image.active_area[0],
      image.active_area[1],
      image.active_area[2],
      image.active_area[3]
    };
    std::vector<double> cropped;
    int crop_w = width;
    int crop_h = height;
    if (ApplyActiveAreaCrop(values, width, height, spp, active_area_vals,
                            &cropped, &crop_w, &crop_h)) {
      values.swap(cropped);
      width = crop_w;
      height = crop_h;
      active_area_applied = true;
    }
  }

  bool iso_ok = !Rcpp::NumericVector::is_na(iso) && iso > 0.0;
  bool exposure_ok = !Rcpp::NumericVector::is_na(exposure_time) &&
    exposure_time > 0.0;
  if (iso_ok && exposure_ok) {
    if (processing) {
      double scale = (100.0 / iso) * (1.0 / exposure_time);
      for (double& v : values) {
        v *= scale;
      }
      exposure_normalized_applied = true;
    } else {
      AppendWarn(&warn_out,
                 "Exposure normalization skipped because normalize is FALSE.");
    }
  } else {
    AppendWarn(&warn_out,
               "Exposure normalization skipped due to missing ISO or ExposureTime.");
  }

  std::vector<double> r_values = RowMajorToR(values, width, height, spp);
  Rcpp::NumericVector pixels(r_values.begin(), r_values.end());
  if (spp == 1) {
    Rcpp::IntegerVector dim = Rcpp::IntegerVector::create(height, width);
    pixels.attr("dim") = dim;
  } else {
    Rcpp::IntegerVector dim = Rcpp::IntegerVector::create(height, width, spp);
    pixels.attr("dim") = dim;
  }

  Rcpp::IntegerMatrix cfa_pattern(2, 2);
  cfa_pattern(0, 0) = image.cfa_pattern[0][0];
  cfa_pattern(0, 1) = image.cfa_pattern[0][1];
  cfa_pattern(1, 0) = image.cfa_pattern[1][0];
  cfa_pattern(1, 1) = image.cfa_pattern[1][1];

  Rcpp::IntegerVector cfa_plane_color(4);
  for (int i = 0; i < 4; ++i) {
    cfa_plane_color[i] = static_cast<int>(
      static_cast<unsigned char>(image.cfa_plane_color[i]));
  }

  Rcpp::IntegerVector black_level(4);
  Rcpp::IntegerVector white_level(4);
  for (int i = 0; i < 4; ++i) {
    black_level[i] = image.black_level[i];
    white_level[i] = image.white_level[i];
  }

  Rcpp::NumericVector as_shot_neutral(3, NA_REAL);
  for (int i = 0; i < 3; ++i) {
    if (image.has_as_shot_neutral) {
      as_shot_neutral[i] = image.as_shot_neutral[i];
    }
  }

  Rcpp::NumericVector analog_balance(3, NA_REAL);
  for (int i = 0; i < 3; ++i) {
    if (image.has_analog_balance) {
      analog_balance[i] = image.analog_balance[i];
    }
  }

  Rcpp::NumericVector color_matrix1(9);
  Rcpp::NumericVector color_matrix2(9);
  int idx = 0;
  for (int r = 0; r < 3; ++r) {
    for (int c = 0; c < 3; ++c) {
      color_matrix1[idx] = image.color_matrix1[r][c];
      color_matrix2[idx] = image.color_matrix2[r][c];
      idx++;
    }
  }

  Rcpp::IntegerVector active_area(4);
  for (int i = 0; i < 4; ++i) {
    active_area[i] = image.has_active_area ? image.active_area[i] : NA_INTEGER;
  }

  Rcpp::List meta = Rcpp::List::create(
    Rcpp::Named("width") = width,
    Rcpp::Named("height") = height,
    Rcpp::Named("samples_per_pixel") = image.samples_per_pixel,
    Rcpp::Named("bits_per_sample") = image.bits_per_sample,
    Rcpp::Named("bits_per_sample_original") = image.bits_per_sample_original,
    Rcpp::Named("sample_format") = static_cast<int>(image.sample_format),
    Rcpp::Named("cfa_pattern") = cfa_pattern,
    Rcpp::Named("cfa_plane_color") = cfa_plane_color,
    Rcpp::Named("cfa_layout") = image.cfa_layout,
    Rcpp::Named("black_level") = black_level,
    Rcpp::Named("white_level") = white_level,
    Rcpp::Named("as_shot_neutral") = as_shot_neutral,
    Rcpp::Named("has_as_shot_neutral") = image.has_as_shot_neutral,
    Rcpp::Named("analog_balance") = analog_balance,
    Rcpp::Named("has_analog_balance") = image.has_analog_balance,
    Rcpp::Named("color_matrix1") = color_matrix1,
    Rcpp::Named("color_matrix2") = color_matrix2,
    Rcpp::Named("orientation") = static_cast<int>(orientation),
    Rcpp::Named("active_area") = active_area,
    Rcpp::Named("has_active_area") = image.has_active_area,
    Rcpp::Named("active_area_applied") = active_area_applied,
    Rcpp::Named("linearization_present") = linearization_present,
    Rcpp::Named("linearization_applied") = linearization_applied,
    Rcpp::Named("opcodes_present") = opcodes_present,
    Rcpp::Named("opcodes_applied") = opcodes_applied,
    Rcpp::Named("baseline_exposure") = baseline_exposure,
    Rcpp::Named("baseline_exposure_seen") = baseline_exposure_seen,
    Rcpp::Named("baseline_exposure_undone") = baseline_exposure_undone,
    Rcpp::Named("iso") = iso,
    Rcpp::Named("exposure_time") = exposure_time,
    Rcpp::Named("f_number") = f_number,
    Rcpp::Named("exposure_normalized_applied") = exposure_normalized_applied
  );

  return Rcpp::List::create(
    Rcpp::Named("pixels") = pixels,
    Rcpp::Named("meta") = meta,
    Rcpp::Named("warn") = warn_out
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector dng_apply_active_area_cpp(Rcpp::NumericVector pixels,
                                              Rcpp::IntegerVector active_area) {
  Rcpp::IntegerVector dims = pixels.attr("dim");
  if (dims.size() < 2) {
    Rcpp::stop("pixels must have dim.");
  }
  int height = dims[0];
  int width = dims[1];
  int spp = (dims.size() >= 3) ? dims[2] : 1;
  if (active_area.size() < 4) {
    Rcpp::stop("active_area must have 4 values.");
  }

  int area[4] = {active_area[0], active_area[1], active_area[2], active_area[3]};
  std::vector<double> row_major = RToRowMajor(pixels, width, height, spp);
  std::vector<double> cropped;
  int crop_w = width;
  int crop_h = height;
  if (!ApplyActiveAreaCrop(row_major, width, height, spp, area, &cropped,
                           &crop_w, &crop_h)) {
    return pixels;
  }

  std::vector<double> out = RowMajorToR(cropped, crop_w, crop_h, spp);
  Rcpp::NumericVector out_vec(out.begin(), out.end());
  if (spp == 1) {
    out_vec.attr("dim") = Rcpp::IntegerVector::create(crop_h, crop_w);
  } else {
    out_vec.attr("dim") = Rcpp::IntegerVector::create(crop_h, crop_w, spp);
  }
  return out_vec;
}

// [[Rcpp::export]]
Rcpp::NumericVector dng_exposure_normalize_cpp(Rcpp::NumericVector pixels,
                                               double iso,
                                               double exposure_time) {
  if (iso <= 0.0 || exposure_time <= 0.0) {
    Rcpp::stop("iso and exposure_time must be positive.");
  }
  Rcpp::IntegerVector dims = pixels.attr("dim");
  if (dims.size() < 2) {
    Rcpp::stop("pixels must have dim.");
  }
  int height = dims[0];
  int width = dims[1];
  int spp = (dims.size() >= 3) ? dims[2] : 1;
  double scale = (100.0 / iso) * (1.0 / exposure_time);

  std::vector<double> row_major = RToRowMajor(pixels, width, height, spp);
  for (double& v : row_major) {
    v *= scale;
  }
  std::vector<double> out = RowMajorToR(row_major, width, height, spp);
  Rcpp::NumericVector out_vec(out.begin(), out.end());
  out_vec.attr("dim") = dims;
  return out_vec;
}

// [[Rcpp::export]]
Rcpp::NumericVector dng_undo_baseline_exposure_cpp(Rcpp::NumericVector pixels,
                                                   double baseline_exposure) {
  Rcpp::IntegerVector dims = pixels.attr("dim");
  if (dims.size() < 2) {
    Rcpp::stop("pixels must have dim.");
  }
  int height = dims[0];
  int width = dims[1];
  int spp = (dims.size() >= 3) ? dims[2] : 1;
  double scale = std::pow(2.0, baseline_exposure);
  if (scale == 0.0) {
    Rcpp::stop("baseline_exposure scale is invalid.");
  }

  std::vector<double> row_major = RToRowMajor(pixels, width, height, spp);
  for (double& v : row_major) {
    v /= scale;
  }
  std::vector<double> out = RowMajorToR(row_major, width, height, spp);
  Rcpp::NumericVector out_vec(out.begin(), out.end());
  out_vec.attr("dim") = dims;
  return out_vec;
}

// [[Rcpp::export]]
bool write_dng_cpp(const std::string& filename,
                   SEXP pixels,
                   Rcpp::List meta,
                   int bitdepth = 16,
                   bool compress = true) {
  if (!Rf_isNumeric(pixels)) {
    Rcpp::stop("pixels must be numeric.");
  }

  Rcpp::RObject pix_obj(pixels);
  if (bitdepth < 1 || bitdepth > 16) {
    Rcpp::stop("bitdepth must be between 1 and 16.");
  }

  Rcpp::NumericVector pix = Rcpp::as<Rcpp::NumericVector>(pix_obj);
  Rcpp::IntegerVector dims = pix.attr("dim");
  if (dims.size() == 0) {
    Rcpp::stop("pixels must be a matrix or 3D array.");
  }

  int height = dims[0];
  int width = dims[1];
  int spp = 1;
  bool is_mosaic = false;

  if (dims.size() == 2) {
    spp = 1;
    is_mosaic = true;
  } else if (dims.size() == 3 && dims[2] == 3) {
    spp = 3;
  } else {
    Rcpp::stop("pixels must have dim (h,w) or (h,w,3).");
  }

  std::size_t n_samples = static_cast<std::size_t>(width) *
    static_cast<std::size_t>(height) * static_cast<std::size_t>(spp);
  std::vector<uint16_t> buf(n_samples);
  double max_val = std::pow(2.0, static_cast<double>(bitdepth)) - 1.0;

  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      for (int c = 0; c < spp; ++c) {
        std::size_t r_idx = static_cast<std::size_t>(y) +
          static_cast<std::size_t>(height) *
          (static_cast<std::size_t>(x) + static_cast<std::size_t>(width) *
          static_cast<std::size_t>(c));

        double v = pix[r_idx];
        if (Rcpp::NumericVector::is_na(v)) {
          v = 0.0;
        }
        v = Clamp01(v);
        uint16_t q = static_cast<uint16_t>(std::lround(v * max_val));

        std::size_t sample_idx = (static_cast<std::size_t>(y) *
          static_cast<std::size_t>(width) + static_cast<std::size_t>(x)) *
          static_cast<std::size_t>(spp) + static_cast<std::size_t>(c);
        buf[sample_idx] = q;
      }
    }
  }

  tinydngwriter::DNGImage dng_image;
  dng_image.SetBigEndian(false);
  if (!dng_image.SetSubfileType(false, false, false)) {
    Rcpp::stop("Failed to set DNG SubfileType.");
  }
  if (!dng_image.SetImageWidth(static_cast<unsigned int>(width)) ||
      !dng_image.SetImageLength(static_cast<unsigned int>(height)) ||
      !dng_image.SetRowsPerStrip(static_cast<unsigned int>(height))) {
    Rcpp::stop("Failed to set DNG image dimensions.");
  }
  if (!dng_image.SetSamplesPerPixel(static_cast<unsigned short>(spp))) {
    Rcpp::stop("Failed to set DNG samples per pixel.");
  }

  std::vector<unsigned short> bps(static_cast<std::size_t>(spp),
                                  static_cast<unsigned short>(bitdepth));
  if (!dng_image.SetBitsPerSample(static_cast<unsigned int>(spp), bps.data())) {
    Rcpp::stop("Failed to set DNG bits per sample.");
  }

  if (!dng_image.SetPlanarConfig(tinydngwriter::PLANARCONFIG_CONTIG)) {
    Rcpp::stop("Failed to set DNG planar configuration.");
  }

  if (is_mosaic) {
    if (!dng_image.SetPhotometric(tinydngwriter::PHOTOMETRIC_CFA)) {
      Rcpp::stop("Failed to set DNG photometric interpretation.");
    }
    if (!dng_image.SetCFARepeatPatternDim(2, 2)) {
      Rcpp::stop("Failed to set CFA repeat pattern.");
    }

    unsigned char cfa_vals[4] = {0, 1, 1, 2};
    if (HasListElement(meta, "cfa_pattern")) {
      Rcpp::IntegerMatrix cfa = Rcpp::as<Rcpp::IntegerMatrix>(meta["cfa_pattern"]);
      if (cfa.nrow() == 2 && cfa.ncol() == 2) {
        cfa_vals[0] = static_cast<unsigned char>(cfa(0, 0));
        cfa_vals[1] = static_cast<unsigned char>(cfa(0, 1));
        cfa_vals[2] = static_cast<unsigned char>(cfa(1, 0));
        cfa_vals[3] = static_cast<unsigned char>(cfa(1, 1));
      }
    }

    if (!dng_image.SetCFAPattern(4, cfa_vals)) {
      Rcpp::stop("Failed to set CFA pattern.");
    }
  } else {
    if (!dng_image.SetPhotometric(tinydngwriter::PHOTOMETRIC_RGB)) {
      Rcpp::stop("Failed to set DNG photometric interpretation.");
    }
  }

  if (HasListElement(meta, "black_level")) {
    Rcpp::NumericVector black = GetNumericVector(meta, "black_level");
    if (black.size() > 0) {
      std::vector<unsigned short> black_vals(static_cast<std::size_t>(black.size()));
      for (R_xlen_t i = 0; i < black.size(); ++i) {
        double v = black[i];
        if (Rcpp::NumericVector::is_na(v)) v = 0.0;
        if (v < 0.0) v = 0.0;
        if (v > max_val) v = max_val;
        black_vals[static_cast<std::size_t>(i)] = static_cast<unsigned short>(v);
      }
      if (black.size() == 4) {
        dng_image.SetBlackLevelRepeatDim(2, 2);
      }
      dng_image.SetBlackLevel(static_cast<unsigned int>(black_vals.size()),
                              black_vals.data());
    }
  }

  if (HasListElement(meta, "white_level")) {
    Rcpp::NumericVector white = GetNumericVector(meta, "white_level");
    if (white.size() > 0) {
      std::vector<double> white_vals(static_cast<std::size_t>(white.size()));
      for (R_xlen_t i = 0; i < white.size(); ++i) {
        double v = white[i];
        if (Rcpp::NumericVector::is_na(v)) v = max_val;
        if (v < 0.0) v = 0.0;
        if (v > max_val) v = max_val;
        white_vals[static_cast<std::size_t>(i)] = v;
      }
      dng_image.SetWhiteLevelRational(static_cast<unsigned int>(white_vals.size()),
                                      white_vals.data());
    }
  }

  bool allow_as_shot = true;
  if (HasListElement(meta, "has_as_shot_neutral")) {
    Rcpp::LogicalVector has_as_shot = Rcpp::as<Rcpp::LogicalVector>(
      meta["has_as_shot_neutral"]);
    allow_as_shot = has_as_shot.size() > 0 && has_as_shot[0] == TRUE;
  }
  if (allow_as_shot && HasListElement(meta, "as_shot_neutral")) {
    Rcpp::NumericVector as_shot = GetNumericVector(meta, "as_shot_neutral");
    if (!HasNaNumeric(as_shot, 3)) {
      dng_image.SetAsShotNeutral(3, as_shot.begin());
    }
  }

  if (HasListElement(meta, "color_matrix1")) {
    Rcpp::NumericVector cm1 = GetNumericVector(meta, "color_matrix1");
    if (!HasNaNumeric(cm1, 9)) {
      dng_image.SetColorMatrix1(3, cm1.begin());
    }
  }

  if (HasListElement(meta, "color_matrix2")) {
    Rcpp::NumericVector cm2 = GetNumericVector(meta, "color_matrix2");
    if (!HasNaNumeric(cm2, 9)) {
      dng_image.SetColorMatrix2(3, cm2.begin());
    }
  }

  bool allow_analog_balance = true;
  if (HasListElement(meta, "has_analog_balance")) {
    Rcpp::LogicalVector has_analog = Rcpp::as<Rcpp::LogicalVector>(
      meta["has_analog_balance"]);
    allow_analog_balance = has_analog.size() > 0 && has_analog[0] == TRUE;
  }
  if (allow_analog_balance && HasListElement(meta, "analog_balance")) {
    Rcpp::NumericVector ab = GetNumericVector(meta, "analog_balance");
    if (!HasNaNumeric(ab, 3)) {
      dng_image.SetAnalogBalance(3, ab.begin());
    }
  }

  bool allow_active_area = true;
  if (HasListElement(meta, "has_active_area")) {
    Rcpp::LogicalVector has_active = Rcpp::as<Rcpp::LogicalVector>(
      meta["has_active_area"]);
    allow_active_area = has_active.size() > 0 && has_active[0] == TRUE;
  }
  if (allow_active_area && HasListElement(meta, "active_area")) {
    Rcpp::IntegerVector area = GetIntegerVector(meta, "active_area");
    if (!HasNaInteger(area, 4)) {
      unsigned int values[4] = {
        static_cast<unsigned int>(area[0]),
        static_cast<unsigned int>(area[1]),
        static_cast<unsigned int>(area[2]),
        static_cast<unsigned int>(area[3])
      };
      dng_image.SetActiveArea(values);
    }
  }

  if (HasListElement(meta, "orientation")) {
    Rcpp::IntegerVector orientation = GetIntegerVector(meta, "orientation");
    if (orientation.size() >= 1 && orientation[0] != NA_INTEGER) {
      dng_image.SetOrientation(static_cast<unsigned short>(orientation[0]));
    }
  }

  if (is_mosaic && compress) {
    if (!dng_image.SetCompression(tinydngwriter::COMPRESSION_NEW_JPEG)) {
      Rcpp::stop("Failed to set DNG compression.");
    }
    if (!dng_image.SetImageDataJpeg(buf.data(),
                                    static_cast<unsigned int>(width),
                                    static_cast<unsigned int>(height),
                                    static_cast<unsigned int>(bitdepth))) {
      Rcpp::stop("Failed to set DNG JPEG image data.");
    }
  } else {
    if (!dng_image.SetCompression(tinydngwriter::COMPRESSION_NONE)) {
      Rcpp::stop("Failed to set DNG compression.");
    }
    if (!dng_image.SetImageData(reinterpret_cast<unsigned char*>(buf.data()),
                                buf.size() * sizeof(uint16_t))) {
      Rcpp::stop("Failed to set DNG image data.");
    }
  }

  tinydngwriter::DNGWriter writer(false);
  writer.AddImage(&dng_image);
  std::string err;
  if (!writer.WriteToFile(filename.c_str(), &err)) {
    Rcpp::stop(err.empty() ? "Failed to write DNG." : err);
  }

  return true;
}
