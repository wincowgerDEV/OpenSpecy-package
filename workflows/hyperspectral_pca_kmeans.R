# Convert a hyperspectral image to a PCA- and K-means-compressed Specs object,
# then map the K-means group assigned to every image pixel.
#
# Example:
# result <- hyperspectral_pca_kmeans("path/to/hyperspectral-image.h5")
# result$specs
# result$heatmap
# saveRDS(result$specs, "hyperspectral-pca-kmeans.rds")

library(OpenSpecy)

hyperspectral_pca_kmeans <- function(x, n_components = 10L, k = 100,
                                     seed = 42L,
                                     steps = c("pca", "kmeans"),
                                     preprocess = T,
                                     smooth_spec = T, 
                                     nstart = 10L) {
  if (is.character(x)) {
    x <- read_any(x)
  }
  if (smooth_spec){
    x <- spatial_smooth(x)
  }
  if (preprocess){
    x <- process_spec(x)
  }
  if (!is_OpenSpecy(x)) {
    stop("'x' must be an OpenSpecy object or a file readable by read_any()",
         call. = FALSE)
  }

  x <- as_OpenSpecy(x)

  if (ncol(x$spectra) <= k) {
    stop("K-means with k = 100 requires more than 100 image pixels/spectra",
         call. = FALSE)
  }

  max_components <- min(nrow(x$spectra), ncol(x$spectra))
  if (length(n_components) != 1L || is.na(n_components) ||
      n_components != as.integer(n_components) || n_components < 1L ||
      n_components > max_components) {
    stop("'n_components' must be between 1 and ", max_components,
         call. = FALSE)
  }

  set.seed(seed)
  specs <- as_Specs(
    x,
    steps = steps,
    #n_components = as.integer(n_components),
    centers = k,
    nstart = as.integer(nstart)
  )

  # K-means replaces each pixel's value_id with KM1, KM2, ..., KM100.
  # Match by source_id so the plotted groups stay aligned with x$spectra.
  coord_index <- match(colnames(x$spectra), specs$coords$source_id)
  if (anyNA(coord_index)) {
    stop("Could not align Specs coordinates with the hyperspectral image",
         call. = FALSE)
  }
  k_group <- as.integer(sub("^KM", "", specs$coords$value_id[coord_index]))
  x$metadata$k_group <- k_group

  list(
    specs = specs,
    map = x,
    k_group = k_group,
    heatmap = heatmap_spec(
      x,
      z = k_group,
      colorscale = "Viridis",
      showlegend = TRUE
    )
  )
}

result <- hyperspectral_pca_kmeans("C:\\Users\\winco\\OneDrive\\Documents\\EWG\\test_1um.h5", 
                                  k = 3,
                                  steps = c("kmeans")
                                  )

result$heatmap

vi <- visual_image(result$map, require = TRUE)

plot(as.raster(vi$image))

particle_image(
  result$map,
  material_col = "k_group",
  alpha = 0.4,
  legend = TRUE,
  main = "K-means groups"
)
