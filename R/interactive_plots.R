#' @rdname interactive_plots
#' @title Plots for Open Specy objects
#'
#' @description
#' These functions generate heatmaps, spectral plots, and interactive plots for OpenSpecy data.
#'
#' @param x An OpenSpecy object containing metadata and spectral data for the first group.
#' @param x2 An optional second OpenSpecy object containing metadata and spectral data for the second group.
#' @param z Optional numeric vector specifying the intensity values for the heatmap. If not provided, the function will use the intensity values from the OpenSpecy object.
#' @param sn Optional numeric value specifying the signal-to-noise ratio threshold. If provided along with \code{min_sn}, regions with SNR below the threshold will be excluded from the heatmap.
#' @param cor Optional numeric value specifying the correlation threshold. If provided along with \code{min_cor}, regions with correlation below the threshold will be excluded from the heatmap.
#' @param min_sn Optional numeric value specifying the minimum signal-to-noise ratio for inclusion in the heatmap. Regions with SNR below this threshold will be excluded.
#' @param min_cor Optional numeric value specifying the minimum correlation for inclusion in the heatmap. Regions with correlation below this threshold will be excluded.
#' @param selected_spectrum Optional index of the selected spectrum to highlight on the heatmap.
#' @param selected_spectrum2 an optional second index of the selected spectrum to highlight on the heatmap.
#' @param \ldots further arguments passed to subfunctions.
#'
#' @return A plotly heatmap object displaying the OpenSpecy data. A subplot containing the heatmap and spectra plot. A plotly object displaying the spectra from the OpenSpecy object(s).
#'
#' @examples
#' data("raman_hdpe")
#' tiny_map <- read_zip(read_extdata("CA_tiny_map.zip"))
#' plotly_spec(raman_hdpe)
#'
#' correlation <- cor_spec(
#'   conform_spec(raman_hdpe, new_wavenumbers = raman_hdpe$wavenumber, res = 5),
#'   conform_spec(tiny_map, tiny_map$wavenumbers, res = 5)
#' )
#' heatmap_spec(tiny_map, z = tiny_map$metadata$y)
#'
#' sample_spec(tiny_map, size = 12) |>
#'   interactive_plot(selected_spectrum = 2, x2 = raman_hdpe)
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
  stop("'x' needs to be of class 'OpenSpecy'")
}

#' @rdname interactive_plots
#'
#' @export
plotly_spec.OpenSpecy <- function(x, x2 = NULL, ...) {
  dt <- cbind(wavenumber = x$wavenumber, x$spectra) |>
    melt(id.vars = "wavenumber", variable.name = "id", value.name = "intensity")

  p <- plot_ly(dt, type = "scatter", mode = "lines", ...) |>
    add_trace(x = ~wavenumber,
              y = ~make_rel(intensity, na.rm = T),
              color = ~id,
              name = "Your Spectra",
              line = list(color = 'rgb(255,255,255)'),
              showlegend = F) |>
    layout(xaxis = list(title = "wavenumber [cm<sup>-1</sup>]", autorange = "reversed"),
           yaxis = list(title = "absorbance intensity [-]"),
           plot_bgcolor = 'rgb(17,0,73)',
           paper_bgcolor = 'rgb(0,0,0)',
           legend = list(orientation = 'h', y = 1.1),
           font = list(color = '#FFFFFF'))

  # Add dummy trace for Your Spectra
  p <- p |>
    add_trace(x = NULL,
              y = NULL,
              line = list(color = 'rgb(255,255,255)'),
              name = "Your Spectra",
              showlegend = T)

  if (!is.null(x2)) {
    dt2 <- cbind(wavenumber = x2$wavenumber, x2$spectra) |>
      melt(id.vars = "wavenumber", variable.name = "id", value.name = "intensity")

    p <- p |>
      add_trace(
        data = dt2,
        x = ~wavenumber,
        y = ~make_rel(intensity, na.rm = T),
        color = ~id,
        type = "scatter",
        mode = "lines",
        name = "Library Spectra",
        line = list(dash = "dash", color = 'rgb(125,249,255)'),
        showlegend = F)

    # Add dummy trace for Library Spectra
    p <- p |>
      add_trace(x = NULL,
                y = NULL,
                line = list(dash = "dash", color = 'rgb(125,249,255)'),
                name = "Library Spectra",
                showlegend = T)
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
  stop("'x' needs to be of class 'OpenSpecy'")
}

#' @rdname interactive_plots
#'
#' @export
heatmap_spec.OpenSpecy <- function(x,
                                   z = NULL,
                                   sn = NULL,
                                   cor = NULL,
                                   min_sn = NULL,
                                   min_cor = NULL,
                                   selected_spectrum = NULL, ...) {
  if(!is.null(z)){
    plot_z <- z # default
  }
  else if(!is.null(cor)){
    plot_z <- cor
  }
  else if(!is.null(sn)){
    plot_z <- sn
  }
  else{
    stop("z, cor, or sn need to be specified to plot the z axis.")
  }
  if (!is.null(sn) && !is.null(min_sn)) {
    plot_z <- ifelse(sn > min_sn, plot_z, NA)
  }
  if (!is.null(cor) && !is.null(min_cor)) {
    plot_z <- ifelse(cor > min_cor, plot_z, NA)
  }

  #colorscale <- if (!is.null(cor)) {
  #    hcl.colors(n = sum(sn > min_sn & cor > min_cor), palette = "viridis")
  #} else {
  #    heat.colors(n = sum(sn > min_sn))
  #}

  p <- plot_ly(...) |>
    add_trace(
      x = x$metadata$x,
      y = x$metadata$y,
      z = plot_z,
      colorscale='Viridis',
      type = "heatmap",
      hoverinfo = 'text',
      showscale = F,
      text = ~paste(
        "row: ", 1:nrow(x$metadata),
        "<br>x: ", x$metadata$x,", y: ", x$metadata$y, ", z: ", plot_z,
        if(!is.null(sn)){paste("<br>snr: ", round(sn, 0))} else{""},
        if(!is.null(cor)){paste("<br>cor: ", round(cor, 1))} else{""})) |>
    layout(
      #title = paste0(nrow(x$metadata), " Spectra"),
      xaxis = list(title = 'x', zeroline = F, showgrid = F),
      yaxis = list(title = 'y',
                   scaleanchor = "x",
                   scaleratio = 1,
                   zeroline = F, showgrid = F),
      plot_bgcolor = 'rgba(17,0,73, 0)',
      paper_bgcolor = 'rgb(0,0,0)',
      showlegend = FALSE,
      font = list(color = '#FFFFFF'))

  if(!is.null(selected_spectrum)){
    p <-  p |> add_markers(
      name = "Selected Spectrum",
      x = x$metadata$x[selected_spectrum],
      y = x$metadata$y[selected_spectrum])
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
  stop("'x' needs to be of class 'OpenSpecy'")
}

#' @rdname interactive_plots
#'
#' @export
interactive_plot.OpenSpecy <- function(x, selected_spectrum, x2 = NULL,
                                       selected_spectrum2 = NULL, ...) {
  # Generate the heatmap
  heat_map <- heatmap_spec(x, z = x$metadata$y, selected_spectrum = selected_spectrum)

  # Generate the spectral plot
  spectra_plot <- plotly_spec(x, x2 = x2, selected_spectrum = selected_spectrum, selected_spectrum2 = selected_spectrum2)

  # Extract intensity and wavenumber for the selected spectrum
  selected_spectrum_points <- x$metadata[selected_spectrum, ]
  selected_spectrum_intensity <- x$spectra[, selected_spectrum, with = F]
  selected_spectrum_wavenumber <- x$wavenumber

  # Add trace for the selected spectrum in the spectral plot
  selected_spectrum_trace <- list(
    type = "scatter",
    mode = "lines",
    x = selected_spectrum_wavenumber,
    y = selected_spectrum_intensity,
    line = list(color = 'red'),  # Set the line color to red
    name = "Selected Spectrum"
  )

  # Update the spectral plot data with the selected spectrum trace
  spectra_plot$data <- c(spectra_plot$data, selected_spectrum_trace)

  # Add margin to heatmap for separation
  heat_map <- heat_map |> layout(autosize = TRUE, margin = list(b = 100))

  # Combine both plots using subplot
  plot_grid <- subplot(heat_map, spectra_plot, nrows = 2, heights = c(0.6, 0.4), margin = 0.1)

  # Show the interactive plot
  return(plot_grid)
}