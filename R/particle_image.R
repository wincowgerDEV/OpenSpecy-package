#' @rdname particle_image
#' @title Plot particle maps with base graphics
#'
#' @description
#' `particle_image()` plots particle classifications from an `OpenSpecy`,
#' `Specs`, or metadata table. When a visual image is attached with
#' \code{\link{add_visual_image}()} or supplied directly, particle coordinates
#' are transformed onto the image and drawn with transparency.
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
#' @param alpha transparency for particle points.
#' @param labels logical; draw feature labels when feature IDs are present?
#' The default is `FALSE` because particle maps quickly become cluttered.
#' @param label_col column used for labels.
#' @param main,xlab,ylab plot labels.
#' @param legend logical; draw a base graphics legend?
#' @param pch,cex plotting character and size passed to
#' \code{\link[graphics]{points}()}.
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
#' @importFrom graphics legend par plot points rasterImage text
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
  material[is.na(material) | !nzchar(material)] <- "NA"
  pal <- .resolve_particle_palette(material, palette)
  cols <- grDevices::adjustcolor(pal[material], alpha.f = alpha)

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
    graphics::points(clipped$coords[valid, 2L], clipped$coords[valid, 1L],
                     col = cols[valid], pch = pch, cex = cex)
    label_x <- clipped$coords[valid, 2L]
    label_y <- clipped$coords[valid, 1L]
    plot_dt <- dt[valid, ]
  } else {
    xx <- dt$x * pixel_length + origin[1L]
    yy <- dt$y * pixel_length + origin[2L]
    plot(xx, yy, type = "n", asp = 1, xlab = xlab, ylab = ylab,
         main = main, ...)
    graphics::points(xx, yy, col = cols, pch = pch, cex = cex)
    label_x <- xx
    label_y <- yy
    plot_dt <- dt
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

  if (isTRUE(legend)) {
    graphics::legend("topright", legend = names(pal), fill = pal,
                     cex = 0.7, bty = "n")
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
