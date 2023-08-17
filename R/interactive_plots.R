#' @rdname interactive_plots
#' @title Interactive plots for OpenSpecy objects
#'
#' @description
#' These functions generate heatmaps, spectral plots, and interactive plots for
#' OpenSpecy data.
#'
#' @param x an \code{OpenSpecy} object containing metadata and spectral data for
#' the first group.
#' @param x2 an optional second \code{OpenSpecy} object containing metadata and
#' spectral data for the second group.
#' @param z optional numeric vector specifying the intensity values for the
#' heatmap. If not provided, the function will use the intensity values from the
#' \code{OpenSpecy} object.
#' @param sn optional numeric value specifying the signal-to-noise ratio
#' threshold. If provided along with \code{min_sn}, regions with SNR below the
#' threshold will be excluded from the heatmap.
#' @param cor optional numeric value specifying the correlation threshold. If
#' provided along with \code{min_cor}, regions with correlation below the
#' threshold will be excluded from the heatmap.
#' @param min_sn optional numeric value specifying the minimum signal-to-noise
#' ratio for inclusion in the heatmap. Regions with SNR below this threshold
#' will be excluded.
#' @param min_cor optional numeric value specifying the minimum correlation for
#' inclusion in the heatmap. Regions with correlation below this threshold
#' will be excluded.
#' @param select optional index of the selected spectrum to highlight on the
#' heatmap.
#' @param select2 an optional second index of the selected spectrum to highlight
#' on the heatmap.
#' @param line list; \code{line} parameter for \code{x}; passed to
#' \code{\link[plotly]{add_trace}()}.
#' @param line2 list; \code{line} parameter for \code{x2}; passed to
#' \code{\link[plotly]{add_trace}()}.
#' @param line_select list; \code{line}; parameter for \code{select}; passed to
#' \code{\link[plotly]{add_trace}()}.
#' @param font list; passed to \code{\link[plotly]{layout}()}.
#' @param plot_bgcolor color value; passed to \code{\link[plotly]{layout}()}.
#' @param paper_bgcolor color value; passed to \code{\link[plotly]{layout}()}.
#' @param colorscale colorscale passed to \code{\link[plotly]{add_trace}()}.
#' @param \ldots further arguments passed to \code{\link[plotly]{plot_ly}()}.
#'
#' @return
#' A plotly heatmap object displaying the OpenSpecy data. A subplot
#' containing the heatmap and spectra plot. A plotly object displaying the
#' spectra from the \code{OpenSpecy} object(s).
#'
#' @examples
#' data("raman_hdpe")
#' tiny_map <- read_extdata("CA_tiny_map.zip") |> read_zip()
#' plotly_spec(raman_hdpe)
#'
#' correlation <- cor_spec(
#'   conform_spec(raman_hdpe, range = raman_hdpe$wavenumber, res = 5),
#'   conform_spec(tiny_map, tiny_map$wavenumbers, res = 5)
#' )
#' heatmap_spec(tiny_map, z = tiny_map$metadata$y)
#'
#' sample_spec(tiny_map, size = 12) |>
#'   interactive_plot(select = 2, x2 = raman_hdpe)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom plotly plot_ly add_trace add_markers subplot layout
#' @importFrom data.table melt
#'
#' @export
plotly_spec <- function(x, ...) {
  UseMethod("plotly_spec")
}

#' @rdname interactive_plots
#'
#' @export
plotly_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname interactive_plots
#'
#' @export
plotly_spec.OpenSpecy <- function(x, x2 = NULL,
                                  line = list(color = 'rgb(255, 255, 255)'),
                                  line2 = list(dash = "dash",
                                               color = 'rgb(125,249,255)'),
                                  font = list(color = '#FFFFFF'),
                                  plot_bgcolor = 'rgb(17, 0, 73)',
                                  paper_bgcolor = 'rgb(0, 0, 0)',
                                  ...) {
  dt <- cbind(wavenumber = x$wavenumber, x$spectra) |>
    melt(id.vars = "wavenumber", variable.name = "id", value.name = "intensity")

  p <- plot_ly(dt, type = "scatter", mode = "lines", ...) |>
    add_trace(x = ~wavenumber, y = ~make_rel(intensity, na.rm = T),
              color = ~id, line = line,
              name = "Your Spectra", showlegend = F) |>
    layout(xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                        autorange = "reversed"),
           yaxis = list(title = "intensity [-]"),
           plot_bgcolor = plot_bgcolor,
           paper_bgcolor = paper_bgcolor,
           legend = list(orientation = 'h', y = 1.1), font = font)

  # Add dummy trace for Your Spectra
  p <- p |>
    add_trace(x = NULL, y = NULL,
              line = line, name = "Your Spectra", showlegend = T)

  if (!is.null(x2)) {
    dt2 <- cbind(wavenumber = x2$wavenumber, x2$spectra) |>
      melt(id.vars = "wavenumber", variable.name = "id", value.name = "intensity")

    p <- p |>
      add_trace(data = dt2, x = ~wavenumber, y = ~make_rel(intensity, na.rm = T),
                color = ~id, type = "scatter", mode = "lines",
                name = "Library Spectra",
                line = line2, showlegend = F)

    # Add dummy trace for Library Spectra
    p <- p |>
      add_trace(x = NULL, y = NULL,
                line = line2,
                name = "Library Spectra", showlegend = T)
  }

  return(p)
}

#' @rdname interactive_plots
#'
#' @export
heatmap_spec <- function(x, ...) {
  UseMethod("heatmap_spec")
}

#' @rdname interactive_plots
#'
#' @export
heatmap_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname interactive_plots
#'
#' @export
heatmap_spec.OpenSpecy <- function(x,
                                   z = NULL, sn = NULL, cor = NULL,
                                   min_sn = NULL, min_cor = NULL, select = NULL,
                                   font = list(color = '#FFFFFF'),
                                   plot_bgcolor = 'rgba(17, 0, 73, 0)',
                                   paper_bgcolor = 'rgb(0, 0, 0)',
                                   colorscale = 'Viridis',
                                   ...) {
  if(!is.null(z))
    plot_z <- z # default
  else if(!is.null(cor))
    plot_z <- cor
  else if(!is.null(sn))
    plot_z <- sn
  else stop("z, cor, or sn need to be specified to plot the z axis", call. = F)

  if (!is.null(sn) && !is.null(min_sn))
    plot_z <- ifelse(sn > min_sn, plot_z, NA)

  if (!is.null(cor) && !is.null(min_cor))
    plot_z <- ifelse(cor > min_cor, plot_z, NA)

  p <- plot_ly(...) |>
    add_trace(x = x$metadata$x, y = x$metadata$y, z = plot_z,
              colorscale = colorscale, type = "heatmap", hoverinfo = 'text',
              showscale = F,
              text = ~paste("row: ", 1:nrow(x$metadata),
                            "<br>x: ", x$metadata$x,", y: ", x$metadata$y,
                            ", z: ", plot_z,
                            if(!is.null(sn)) paste("<br>snr: ", round(sn, 0)) else "",
                            if(!is.null(cor)) paste("<br>cor: ", round(cor, 1)) else ""
                            )) |>
    layout(
      xaxis = list(title = 'x', zeroline = F, showgrid = F),
      yaxis = list(title = 'y', scaleanchor = "x", scaleratio = 1,
                   zeroline = F, showgrid = F),
      plot_bgcolor = plot_bgcolor, paper_bgcolor = paper_bgcolor,
      showlegend = FALSE, font = font)

  if(!is.null(select)){
    p <-  p |> add_markers(x = x$metadata$x[select], y = x$metadata$y[select],
                           name = "Selected Spectrum")
  }

  return(p)
}

#' @rdname interactive_plots
#'
#' @export
interactive_plot <- function(x, ...) {
  UseMethod("interactive_plot")
}

#' @rdname interactive_plots
#'
#' @export
interactive_plot.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname interactive_plots
#'
#' @export
interactive_plot.OpenSpecy <- function(x, select, x2 = NULL, select2 = NULL,
                                       line = list(color = 'rgb(255, 255, 255)'),
                                       line2 = list(dash = "dash",
                                                    color = 'rgb(125,249,255)'),
                                       line_select = list(color = 'red'),
                                       font = list(color = '#FFFFFF'),
                                       plot_bgcolor = 'rgba(17, 0, 73, 0)',
                                       paper_bgcolor = 'rgb(0, 0, 0)',
                                       colorscale = 'Viridis',
                                       ...) {
  # Generate the heatmap
  heat_map <- heatmap_spec(x, z = x$metadata$y, select = select,
                           font = font, plot_bgcolor = plot_bgcolor,
                           paper_bgcolor = paper_bgcolor,
                           colorscale = colorscale)

  # Generate the spectral plot
  spectra_plot <- plotly_spec(x, x2 = x2, select = select, select2 = select2,
                              line = line, line2 = line2, font = font,
                              plot_bgcolor = plot_bgcolor,
                              paper_bgcolor = paper_bgcolor)

  # Extract intensity and wavenumber for the selected spectrum
  select_points <- x$metadata[select, ]
  select_intensity <- x$spectra[, select, with = F]
  select_wavenumber <- x$wavenumber

  # Add trace for the selected spectrum in the spectral plot
  select_trace <- list(type = "scatter", mode = "lines", x = select_wavenumber,
                       y = select_intensity, line = line_select,
                       name = "Selected Spectrum"
                       )

  # Update the spectral plot data with the selected spectrum trace
  spectra_plot$data <- c(spectra_plot$data, select_trace)

  # Add margin to heatmap for separation
  heat_map <- heat_map |> layout(autosize = TRUE, margin = list(b = 100))

  # Combine both plots using subplot
  plot_grid <- subplot(heat_map, spectra_plot, nrows = 2, heights = c(0.6, 0.4),
                       margin = 0.1)

  # Show the interactive plot
  return(plot_grid)
}
