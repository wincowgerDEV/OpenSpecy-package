#' @rdname plot_OpenSpecy
#' @title Plots for Open Specy objects
#'
#' @description
#' These functions generate heatmaps, spectral plots, and interactive plots for OpenSpecy data.
#'
#' @param object An OpenSpecy object containing metadata and spectral data.
#' @param z Optional numeric vector specifying the intensity values for the heatmap. If not provided, the function will use the intensity values from the OpenSpecy object.
#' @param sn Optional numeric value specifying the signal-to-noise ratio threshold. If provided along with \code{min_sn}, regions with SNR below the threshold will be excluded from the heatmap.
#' @param cor Optional numeric value specifying the correlation threshold. If provided along with \code{min_cors}, regions with correlation below the threshold will be excluded from the heatmap.
#' @param min_sn Optional numeric value specifying the minimum signal-to-noise ratio for inclusion in the heatmap. Regions with SNR below this threshold will be excluded.
#' @param min_cors Optional numeric value specifying the minimum correlation for inclusion in the heatmap. Regions with correlation below this threshold will be excluded.
#' @param selected_spectrum Optional index of the selected spectrum to highlight on the heatmap.
#' @param x An OpenSpecy object containing metadata and spectral data for the first group.
#' @param selected_spectrum Optional index of the selected spectrum to plot.
#' @param x2 An optional second OpenSpecy object containing metadata and spectral data for the second group.
#' @param selected_spectrum2 Optional index of the selected spectrum from the second group to plot.
#'
#' @return A plotly heatmap object displaying the OpenSpecy data. A subplot containing the heatmap and spectra plot. A plotly object displaying the spectra from the OpenSpecy object(s).
#'
#' @examples
#' data1 <- read_any(read_extdata("raman_hdpe.json"))
#' data2 <- read_zip("inst/extdata/CA_tiny_map.zip")
#' plot_OpenSpecy(data1)
#' plot_OpenSpecy(data1, selected_spectrum = 2)
#' plot_OpenSpecy(data1, x2 = data2, selected_spectrum2 = 3)
#' heatmap_OpenSpecy(data2, z = data2$metadata$y)
#' interactive_plot(data1, selected_spectrum = 2)
#' interactive_plot(data2, selected_spectrum = 2, x2 = data1, selected_spectrum2 = 1)
#' @author
#' Win Cowger, Zacharias Steinmetz#'
#' 
#' @importFrom plotly plot_ly add_trace add_markers subplot layout
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% 
#' 
#' @export
 heatmap_OpenSpecy <- function(object, 
                               z = NULL, 
                               sn = NULL, 
                               cor = NULL, 
                               min_sn = NULL, 
                               min_cors = NULL,
                               selected_spectrum = NULL) {
     
     if(is.null(object)) {
         stop("No data object provided")
     }
     
     plot_z <- z # default
     if (!is.null(sn) && !is.null(min_sn)) {
         plot_z <- ifelse(sn > min_sn, plot_z, NA)
     }
     if (!is.null(cor) && !is.null(min_cors)) {
         plot_z <- ifelse(cor > min_cors, plot_z, NA)
     }
     
     p <- plot_ly() %>%
         add_trace(
             x = object$metadata$x,
             y = object$metadata$y,
             z = plot_z,
             type = "heatmap",
             hoverinfo = 'text',
             showscale = F,
             colors = "viridis",
             text = ~paste(
                 "row: ", 1:nrow(object$metadata),
                 "<br>x: ", object$metadata$x,", y: ", object$metadata$y, ", z: ", plot_z,
                 if(!is.null(sn)){paste("<br>snr: ", round(sn, 0))} else{""},
                 if(!is.null(cor)){paste("<br>cor: ", round(cor, 1))} else{""})) %>%
         layout(
             xaxis = list(title = 'x', zeroline = F, showgrid = F),
             yaxis = list(title = 'y', zeroline = F, showgrid = F),
             showlegend = FALSE)
     if(!is.null(selected_spectrum)){
        p <-  p %>% add_markers(
         x = object$metadata$x[selected_spectrum],
         y = object$metadata$y[selected_spectrum])
     }
     return(p)
 }
 
 #' @rdname plot_OpenSpecy
 #'
 #' @export
 plot_OpenSpecy <- function(x, selected_spectrum = 1, x2 = NULL, selected_spectrum2 = 1) {
     dt <- cbind(wavenumber = x$wavenumber, intensity = x$spectra[, ..selected_spectrum]) |>
         pivot_longer(cols = -wavenumber, names_to = "id", values_to = "intensity")
     
     p <- plot_ly(dt, x = ~wavenumber, y = ~make_rel(intensity), color = ~id, type = "scatter", mode = "lines") %>%
         layout(xaxis = list(title = "Wavenumber", autorange = "reversed"), 
                yaxis = list(title = "Intensity"),
                showlegend = FALSE)
     
     if (!is.null(x2)) {
         dt2 <- cbind(wavenumber = x2$wavenumber, intensity = x2$spectra[, ..selected_spectrum2]) |>
             pivot_longer(cols = -wavenumber, names_to = "id", values_to = "intensity")
         
         p <- p %>% 
             add_trace(
                 data = dt2, 
                 x = ~wavenumber, 
                 y = ~make_rel(intensity), 
                 color = ~id, 
                 type = "scatter", 
                 mode = "lines",
                 line = list(dash = "dash")
             )
     }
     
     return(p)
 }
 
 
 #' @rdname plot_OpenSpecy
 #'
 #' @export
 interactive_plot <- function(x, selected_spectrum, x2, selected_spectrum2){
     heat_map <- heatmap_OpenSpecy(x, z = x$metadata$y, selected_spectrum = selected_spectrum)
     spectra_plot <- plot_OpenSpecy(x, selected_spectrum, x2, selected_spectrum2)
     
     # Add margin to heatmap for separation
     heat_map <- heat_map %>% layout(autosize = T, margin = list(b = 100))
     
     subplot(heat_map, spectra_plot, nrows = 2, heights = c(0.6, 0.4), margin = 0.1)
 }
