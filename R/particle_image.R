#' @rdname particle_image
#' @title Plot particle maps with base graphics
#'
#' @description
#' `particle_image()` plots particle classifications from an `OpenSpecy`,
#' `Specs`, or metadata table. When a visual image is attached with
#' \code{\link{add_visual_image}()} or supplied directly, particle coordinates
#' are transformed onto the image and drawn as a transparent categorical raster.
#'
#' @param x an `OpenSpecy` object, `Specs` object, or metadata table.
#' @param material_col column containing material or class labels.
#' @param image optional image path, array, raster, raw BMP bytes, or image
#' object. If `NULL`, an attached visual image is used when available.
#' @param bottom_left,top_right optional map extent in image pixel coordinates.
#' @param pixel_length map pixel length in plotting units when no image overlay
#' is used.
#' @param origin numeric length-2 x/y origin offset when no image overlay is
#' used.
#' @param palette named character vector mapping materials to colors.
#' @param alpha transparency for particle raster cells.
#' @param labels logical; draw feature labels when feature IDs are present?
#' The default is `FALSE` because particle maps quickly become cluttered.
#' @param label_col column used for labels.
#' @param main,xlab,ylab plot labels.
#' @param legend logical; draw a base graphics legend?
#' @param pch,cex retained for compatibility; particle cells are drawn as a
#' raster.
#' @param \ldots additional arguments passed to \code{\link[graphics]{plot}()}.
#'
#' @return Invisibly returns the plotted metadata table.
#'
#' @examples
#' tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
#' tiny_map$metadata$material_class <- ifelse(tiny_map$metadata$x < 5,
#'                                            "poly(ethylene)", "mineral")
#' particle_image(tiny_map, legend = TRUE)
#'
#' @importFrom graphics box legend par plot rasterImage text
#' @importFrom grDevices adjustcolor rainbow
#' @export
particle_image <- function(x, material_col = "material_class", image = NULL,
                           bottom_left = NULL, top_right = NULL,
                           pixel_length = 1, origin = c(0, 0),
                           palette = NULL, alpha = 0.8, labels = FALSE,
                           label_col = "feature_id", main = "Particle Image",
                           xlab = "X", ylab = "Y", legend = FALSE,
                           pch = 15, cex = 1, ...) {
  dt <- .particle_plot_data(x)
  if (!all(c("x", "y") %in% names(dt))) {
    stop("'x' must provide x and y coordinate columns", call. = FALSE)
  }
  if (!material_col %in% names(dt)) {
    stop("material column '", material_col, "' was not found",
         call. = FALSE)
  }

  vi <- if (is_OpenSpecy(x) || is_Specs(x)) {
    .resolve_visual_image(x, img = image, bottom_left = bottom_left,
                          top_right = top_right)
  } else {
    list(image = image, bottom_left = bottom_left, top_right = top_right)
  }

  material <- as.character(dt[[material_col]])
  background <- .particle_background_material(material)
  material[background] <- NA_character_
  pal <- .resolve_particle_palette(material[!background], palette)

  if (!is.null(vi$image) && !is.null(vi$bottom_left) &&
      !is.null(vi$top_right)) {
    raster <- .visual_image_raster(vi$image)
    map_dim <- .visual_map_dim(vi, dt)
    xy <- .map_to_image_coords(dt$x, dt$y, map_dim, vi$bottom_left,
                               vi$top_right)
    clipped <- .clip_image_coords(
      cbind(xy$y, xy$x), dim(raster),
      tolerance = .image_edge_tolerance(map_dim, vi$bottom_left, vi$top_right)
    )
    valid <- clipped$valid
    plot(NA, NA, xlim = c(1, ncol(raster)), ylim = c(nrow(raster), 1),
         asp = 1, xlab = xlab, ylab = ylab, main = main, ...)
    graphics::rasterImage(raster, 1, nrow(raster), ncol(raster), 1)
    grid <- .particle_material_grid(dt, material, background,
                                    map_dim = map_dim)
    overlay <- .particle_grid_raster(grid, pal, alpha = alpha)
    ext <- .clip_particle_overlay_extent(vi$bottom_left, vi$top_right,
                                         dim(raster))
    graphics::rasterImage(overlay, ext$xleft, ext$ybottom,
                          ext$xright, ext$ytop, interpolate = FALSE)
    valid <- valid & !background
    label_x <- clipped$coords[valid, 2L]
    label_y <- clipped$coords[valid, 1L]
    plot_dt <- dt[valid, ]
  } else {
    map_dim <- if (is_OpenSpecy(x) || is_Specs(x)) .infer_visual_map_dim(x)
    grid <- .particle_material_grid(dt, material, background,
                                    map_dim = map_dim,
                                    pixel_length = pixel_length,
                                    origin = origin)
    .plot_particle_material_grid(grid, pal, alpha = alpha, main = main,
                                 xlab = xlab, ylab = ylab, ...)
    keep <- !background
    label_x <- suppressWarnings(as.numeric(dt$x[keep])) * pixel_length +
      origin[1L]
    label_y <- suppressWarnings(as.numeric(dt$y[keep])) * pixel_length +
      origin[2L]
    plot_dt <- dt[keep, ]
  }

  if (isTRUE(labels) && label_col %in% names(dt)) {
    label_dt <- plot_dt[!duplicated(plot_dt[[label_col]]) &
                          !is.na(plot_dt[[label_col]]) &
                          as.character(plot_dt[[label_col]]) != "-88", ]
    if (nrow(label_dt)) {
      idx <- match(label_dt[[label_col]], plot_dt[[label_col]])
      keep <- !is.na(idx)
      idx <- idx[keep]
      graphics::text(label_x[idx], label_y[idx],
                     labels = label_dt[[label_col]][keep],
                     cex = 0.6, pos = 3)
    }
  }

  if (isTRUE(legend) && length(pal)) {
    graphics::legend("topright", legend = names(pal), fill = pal,
                     cex = 0.8, bty = "n")
  }

  invisible(dt)
}

.particle_plot_data <- function(x) {
  if (is_OpenSpecy(x)) {
    return(data.table::as.data.table(as_OpenSpecy(x)$metadata))
  }
  if (is_Specs(x)) {
    x <- as_Specs(x)
    md <- data.table::as.data.table(x$coords)
    if (nrow(x$metadata) && "value_id" %in% names(x$metadata)) {
      row_id <- seq_len(nrow(md))
      md$.row_id <- row_id
      md <- merge(md, x$metadata, by = "value_id", all.x = TRUE,
                  sort = FALSE)
      data.table::setorder(md, .row_id)
      md$.row_id <- NULL
    }
    return(md)
  }
  data.table::as.data.table(x)
}

.resolve_particle_palette <- function(material, palette = NULL) {
  material <- material[!is.na(material) & nzchar(material)]
  if (!length(material)) return(character())
  if (is.null(palette)) palette <- .particle_material_palette()
  missing <- setdiff(unique(material), names(palette))
  if (length(missing)) {
    palette <- c(
      palette,
      stats::setNames(grDevices::rainbow(length(missing)), missing)
    )
  }
  palette[unique(material)]
}

.particle_background_material <- function(material) {
  material <- trimws(material)
  is.na(material) | !nzchar(material) |
    tolower(material) %in% c("background", "na")
}

.particle_material_grid <- function(dt, material, background,
                                    map_dim = NULL, pixel_length = 1,
                                    origin = c(0, 0)) {
  x <- suppressWarnings(as.numeric(dt$x))
  y <- suppressWarnings(as.numeric(dt$y))
  valid <- is.finite(x) & is.finite(y)
  if (!any(valid)) {
    return(list(x = numeric(), y = numeric(),
                z = matrix(NA_character_, 0L, 0L)))
  }

  if (!is.null(map_dim) && length(map_dim) == 2L &&
      all(is.finite(map_dim)) && all(map_dim > 0)) {
    nx <- as.integer(round(map_dim[1L]))
    ny <- as.integer(round(map_dim[2L]))
    x0 <- min(x[valid])
    y0 <- min(y[valid])
    x_vals <- x0 + seq_len(nx) - 1L
    y_vals <- y0 + seq_len(ny) - 1L
    x_idx <- as.integer(round(x - x0)) + 1L
    y_idx <- as.integer(round(y - y0)) + 1L
  } else {
    x_vals <- sort(unique(x[valid]))
    y_vals <- sort(unique(y[valid]))
    x_idx <- match(x, x_vals)
    y_idx <- match(y, y_vals)
  }

  valid <- valid & !is.na(x_idx) & !is.na(y_idx) &
    x_idx >= 1L & x_idx <= length(x_vals) &
    y_idx >= 1L & y_idx <= length(y_vals)
  show <- valid & !background & !is.na(material) & nzchar(material)

  z <- matrix(NA_character_, nrow = length(x_vals), ncol = length(y_vals))
  if (any(show)) z[cbind(x_idx[show], y_idx[show])] <- material[show]
  list(x = x_vals * pixel_length + origin[1L],
       y = y_vals * pixel_length + origin[2L],
       z = z)
}

.particle_grid_raster <- function(grid, pal, alpha = 0.8) {
  if (!length(grid$x) || !length(grid$y)) {
    return(grDevices::as.raster(matrix("#FFFFFF00", 1L, 1L)))
  }
  colors <- matrix("#FFFFFF00", nrow = nrow(grid$z), ncol = ncol(grid$z))
  if (length(pal)) {
    idx <- match(grid$z, names(pal))
    fill <- !is.na(idx)
    colors[fill] <- grDevices::adjustcolor(pal[idx[fill]], alpha.f = alpha)
  }
  grDevices::as.raster(t(colors[, rev(seq_len(ncol(colors))), drop = FALSE]))
}

.plot_particle_material_grid <- function(grid, pal, alpha, main, xlab, ylab,
                                         ...) {
  xr <- .particle_cell_range(grid$x)
  yr <- .particle_cell_range(grid$y)
  graphics::plot(NA, NA, xlim = xr, ylim = yr, asp = 1, xlab = xlab,
                 ylab = ylab, main = main, ...)
  graphics::rasterImage(.particle_grid_raster(grid, pal, alpha = alpha),
                        xr[1L], yr[1L], xr[2L], yr[2L],
                        interpolate = FALSE)
  graphics::box()
}

.particle_cell_range <- function(x) {
  if (!length(x)) return(c(0, 1))
  x <- sort(unique(x))
  step <- if (length(x) > 1L) min(diff(x)) else 1
  c(min(x) - step / 2, max(x) + step / 2)
}

.clip_particle_overlay_extent <- function(bottom_left, top_right, image_dim) {
  x <- pmin(pmax(c(bottom_left[1L], top_right[1L]), 1), image_dim[2L])
  y <- pmin(pmax(c(bottom_left[2L], top_right[2L]), 1), image_dim[1L])
  list(xleft = x[1L], xright = x[2L], ybottom = y[1L], ytop = y[2L])
}

.particle_material_palette <- function() {
  c(
    "mineral" = "#FC8EAC",
    "organic matter" = "pink",
    "cellulose derivatives (ether cellulose)" = "red",
    "polyacrylonitriles (nitriles)" = "darkblue",
    "poly(esters_ethers_diglycidylethers_terephthalates)s" = "lightblue",
    "polymethacrylates" = "forestgreen",
    "poly(acrylamide_amid)s" = "green",
    "poly(ethylene)" = "#FF6E00",
    "polysiloxanes" = "#FFBD31",
    "polyvinylalcohols" = "#E0B0FF",
    "polycarbonates" = "magenta",
    "polyurethanes (isocyanates)" = "yellow",
    "poly(propylene)" = "#15F4EE",
    "polyhaloolefins (vinylhalides)" = "maroon",
    "polyethersulfones (polysulfone)" = "#00988F",
    "polystyrenes (polyphenylethylenes, -methylstyrene)" = "#882d17",
    "polyvinylethers" = "#DDAD4B",
    "polydienes (butadienes, isoprenes)" = "cornflowerblue",
    "polyhydroxy(meth)acrylates" = "#4CBB17",
    "polysuccinates" = "black",
    "polyvinylesters" = "darkgray",
    "poly(ether)ketones (polyketones)" = "#FFFFCC",
    "NA" = "lavender"
  )
}
