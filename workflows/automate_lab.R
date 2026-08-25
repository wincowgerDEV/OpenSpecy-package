# File-backed particle analysis for a large H5 map.
#
# Set OPENSPECY_PARTICLE_SOURCE to an H5, ENVI .hdr, .dat, or .img source.
# Set OPENSPECY_PARTICLE_OUTPUT to a durable output directory if desired; the
# default is a temporary directory so this workflow never writes beside or over
# the authoritative source.

source_file <- Sys.getenv("OPENSPECY_PARTICLE_SOURCE", unset = "")
if (!nzchar(source_file) || !file.exists(source_file)) {
  stop("Set OPENSPECY_PARTICLE_SOURCE to a readable H5 or ENVI source.")
}

output_dir <- Sys.getenv(
  "OPENSPECY_PARTICLE_OUTPUT",
  unset = file.path(tempdir(), "openspecy-particle-analysis")
)
cache_dir <- Sys.getenv(
  "OPENSPECY_PARTICLE_CACHE",
  unset = file.path(tempdir(), "openspecy-filespec-cache")
)

# These are the current scientific decision thresholds. They are also drawn on
# the returned S/N and maximum-correlation histograms.
sn_threshold_min <- 4*10^5
sn_threshold_max <- Inf
cor_threshold <- 0.6

get_lib("medoid_derivative")
lib <- load_lib("medoid_derivative")

wd = "C:\\Users\\winco\\OneDrive\\Documents\\EWG"
source_file = "C:\\Users\\winco\\OneDrive\\Documents\\EWG\\bigconcurve.h5"
map <- open_specs(source_file, cache_dir = wd)
print(map)

# Region views share the immutable source and cache. This is useful for
# inspection, but automate_particle_analysis() discovers and processes these
# regions sequentially without a user-written loop.
region_views <- split_spec(map, by = "region")
names(region_views)

map <- read_h5("C:\\Users\\winco\\OneDrive\\Documents\\EWG\\smallconcurve.h5")

listedfiles <- lapply(unique(map$metadata$region), function(x) filter_spec(map, map$metadata$region ==x))

result2 <- automate_particle_analysis( 
    listedfiles,
    library = lib,
    output_dir = wd,
    material_col = "material_class",
    particle_id_strategy = "collapse",
    spectral_smooth = TRUE,
    sn_threshold_min = sn_threshold_min,
    sn_threshold_max = sn_threshold_max,
    cor_threshold = cor_threshold,
    area_threshold = 1,
    label_unknown = TRUE,
    remove_unknown = FALSE,
    pixel_length = 1,
    metric = "tot_sig",
    collapse_function = mean,
    outputs = c(
      "details", "summary", "processed", "particle_image",
      "particle_heatmap", "particle_heatmap_thresholded", "cor_heatmap",
      "sn_histogram", "cor_histogram", "time"
    ),
    origins = list(x = 0, y = 0)
)

library(dplyr)
result2$particle_details_all_csv %>%
  group_by(sample_id) %>%
  summarise(percent_area = sum(area_um2[material_class == "poly(ethylene)"])/sum(area_um2),
area = sum(area_um2))

plot(sample_spec(listedfiles[[4]] |> spatial_smooth(), 10))

# plot() replaces replayPlot() and accepts a region/sample name or position.
plot(result2, sample = 1, which = "particle_heatmap")
plot(result, sample = 1, which = "sn_histogram")
plot(result2, sample = 1, which = "particle_image")

result$particle_details_all_csv
result$particle_summary_all_csv
