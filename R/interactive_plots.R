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
#' @param model optional model library returned by \code{\link{train_spec_model}()}.
#' Supplying this with \code{model_class} adds a logistic-regression coefficient
#' background to \code{plotly_spec()}.
#' @param model_class exact trained class label whose signed logistic
#' coefficients should be displayed. Positive weights are green, values near
#' zero yellow, and negative weights red. These are model weights, not causal
#' peak attributions.
#' @param model_colorscale continuous Plotly colorscale for signed model
#' weights, centered at zero.
#' @param model_opacity opacity of the model-weight background from zero to one.
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
#' @param line list; \code{line} parameter for \code{x}; passed to
#' \code{\link[plotly]{add_trace}()}.
#' @param line2 list; \code{line} parameter for \code{x2}; passed to
#' @param font list; passed to \code{\link[plotly]{layout}()}.
#' @param plot_bgcolor color value; passed to \code{\link[plotly]{layout}()}.
#' @param paper_bgcolor color value; passed to \code{\link[plotly]{layout}()}.
#' @param colorscale colorscale passed to \code{\link[plotly]{add_trace}()} can
#' be an array or one of `"Blackbody"`, `"Bluered"`, `"Blues"`, `"Cividis"`,
#' `"Earth"`, `"Electric"`, `"Greens"`, `"Greys"`, `"Hot"`, `"Jet"`, `"Picnic"`,
#' `"Portland"`, `"Rainbow"`, `"RdBu"`, `"Reds"`, `"Viridis"`, `"YlGnBu"`,
#' `"YlOrRd"`.
#' @param showlegend whether to show the legend passed to
#' @param make_rel logical, whether to make the spectra relative or use the raw values
#' @param type specification for plot type either interactive or static
#' \code{\link[plotly]{plot_ly}()}.
#' @param \ldots further arguments passed to \code{\link[plotly]{plot_ly}()}.
#'
#' @return
#' A plotly heatmap object displaying the OpenSpecy data. A subplot
#' containing the heatmap and spectra plot. A plotly object displaying the
#' spectra from the \code{OpenSpecy} object(s).
#'
#' @examples
#' \dontshow{data.table::setDTthreads(2)}
#' data("raman_hdpe")
#' tiny_map <- read_extdata("CA_tiny_map.zip") |> read_zip()
#' plotly_spec(raman_hdpe)
#'
#' heatmap_spec(tiny_map, z = tiny_map$metadata$y, showlegend = TRUE)
#'
#' sample_spec(tiny_map, size = 12) |>
#'   interactive_plot(select = 2, x2 = raman_hdpe)
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @importFrom plotly plot_ly add_trace add_markers subplot layout
#' @importFrom data.table melt
#' @importFrom graphics image
#' @importFrom grDevices heat.colors
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
plotly_spec.OpenSpecy <- function(x,
                                  x2 = NULL,
                                  model = NULL,
                                  model_class = NULL,
                                  model_colorscale = list(
                                    c(0, "#d73027"),
                                    c(0.5, "#fee08b"),
                                    c(1, "#1a9850")
                                  ),
                                  model_opacity = 0.28,
                                  line = list(color = 'rgb(255, 255, 255)'),
                                  line2 = list(dash = "dot",
                                               color = "rgb(255,0,0)"),
                                  font = list(color = '#FFFFFF'),
                                  plot_bgcolor = 'rgba(17, 0, 73, 0)',
                                  paper_bgcolor = 'rgb(0, 0, 0)',
                                  showlegend = FALSE,
                                  make_rel = TRUE,
                                  ...) {
  x <- as_OpenSpecy(x)
  if(make_rel)   x <- make_rel(x, na.rm = T)
  dt <- as.data.table(cbind(wavenumber = x$wavenumber, x$spectra)) |>
    melt(
      id.vars = "wavenumber",
      variable.name = "id",
      value.name = "intensity"
    )

  p <- plot_ly(dt, ...)
  if (!is.null(model) || !is.null(model_class)) {
    if (is.null(model) || is.null(model_class)) {
      stop("'model' and 'model_class' must be supplied together", call. = FALSE)
    }
    if (!is.numeric(model_opacity) || length(model_opacity) != 1L ||
        is.na(model_opacity) || model_opacity < 0 || model_opacity > 1) {
      stop("'model_opacity' must be one number from zero to one", call. = FALSE)
    }
    weights <- model_class_weights(model, model_class)
    limit <- max(abs(weights$weight), na.rm = TRUE)
    if (!is.finite(limit) || limit == 0) limit <- 1
    p <- p |> add_trace(
      x = weights$wavenumber,
      y = c(0, 1),
      z = rbind(weights$weight, weights$weight),
      type = "heatmap",
      yaxis = "y2",
      colorscale = model_colorscale,
      zmin = -limit,
      zmax = limit,
      zmid = 0,
      opacity = model_opacity,
      showscale = TRUE,
      colorbar = list(title = "Logistic<br>weight"),
      hovertemplate = paste0(
        "%{x:.1f} cm<sup>-1</sup><br>weight %{z:.4g}<extra>",
        model_class, "</extra>"
      ),
      inherit = FALSE
    )
  }
  p <- p |>
    add_trace(
      data = dt,
      x = ~ wavenumber,
      y = ~ intensity,
      split = ~ id,
      line = line,
      name = "x1",
      showlegend = showlegend
    ) |>
    layout(
      xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                   autorange = "reversed"),
      yaxis = list(title = "intensity [-]"),
      yaxis2 = list(overlaying = "y", range = c(0, 1), visible = FALSE,
                    fixedrange = TRUE),
      plot_bgcolor = plot_bgcolor,
      paper_bgcolor = paper_bgcolor,
      legend = list(orientation = 'h', y = 1.1),
      font = font
    )

  if(!is.null(x2)) {
    x2 <- as_OpenSpecy(x2)
    if(make_rel) x2 <- make_rel(x2, na.rm = T)
    dt2 <- as.data.table(cbind(wavenumber = x2$wavenumber, x2$spectra)) |>
      melt(
        id.vars = "wavenumber",
        variable.name = "id",
        value.name = "intensity"
      )

    p <- p |>
      add_trace(
        data = dt2,
        x = ~ wavenumber,
        y = ~ intensity,
        split = ~ id,
        type = "scatter",
        mode = "lines",
        name = "x2",
        line = line2,
        showlegend = showlegend
      )
  }

  return(p)
}

#' @rdname interactive_plots
#'
#' @export
model_class_weights <- function(model, model_class) {
  if (!is.list(model) || is.null(model$coefficients)) {
    stop("'model' must be a trained logistic-regression model library",
         call. = FALSE)
  }
  model_type <- if (is.null(model$model_type)) {
    "logistic_regression"
  } else {
    model$model_type
  }
  if (!identical(model_type, "logistic_regression")) {
    stop("Model-weight overlays currently support logistic regression only",
         call. = FALSE)
  }
  if (!is.character(model_class) || length(model_class) != 1L ||
      is.na(model_class) || !nzchar(model_class)) {
    stop("'model_class' must be one nonempty trained class label", call. = FALSE)
  }
  coefficients <- data.table::as.data.table(model$coefficients)
  required <- c("name", "names", "dimension_units")
  missing <- setdiff(required, names(coefficients))
  if (length(missing)) {
    stop("Model coefficients are missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  available <- unique(c(
    as.character(coefficients$name),
    as.character(model$class_names),
    if (!is.null(model$dimension_conversion$name)) {
      as.character(model$dimension_conversion$name)
    } else {
      character()
    }
  ))
  available <- available[!is.na(available) & nzchar(available)]
  if (!model_class %in% available) {
    stop(
      "Class '", model_class, "' is not present in this model. Available ",
      "classes include: ", paste(head(available, 10L), collapse = ", "),
      call. = FALSE
    )
  }
  out <- coefficients[
    as.character(name) == model_class,
    .(
      wavenumber = suppressWarnings(as.numeric(names)),
      weight = as.numeric(dimension_units)
    )
  ]
  out <- out[is.finite(wavenumber) & wavenumber != 0 & is.finite(weight)]
  out <- out[, .(weight = sum(weight)), by = wavenumber]
  axis <- suppressWarnings(as.numeric(model$all_variables))
  axis <- sort(unique(axis[is.finite(axis) & axis != 0]))
  if (length(axis)) {
    out <- merge(
      data.table::data.table(wavenumber = axis), out,
      by = "wavenumber", all.x = TRUE, sort = TRUE
    )
    out[is.na(weight), weight := 0]
  }
  if (!nrow(out)) {
    stop("The model does not contain a finite wavenumber axis", call. = FALSE)
  }
  data.table::setorder(out, wavenumber)
  out[]
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
                                   z = NULL,
                                   sn = NULL,
                                   cor = NULL,
                                   min_sn = NULL,
                                   min_cor = NULL,
                                   select = NULL,
                                   font = list(color = '#FFFFFF'),
                                   plot_bgcolor = 'rgba(17, 0, 73, 0)',
                                   paper_bgcolor = 'rgb(0, 0, 0)',
                                   colorscale = 'Viridis',
                                   showlegend = FALSE,
                                   type = "interactive",
                                   ...) {
  x <- as_OpenSpecy(x)

  if(!is.null(z))
    plot_z <- z # default
  else if(!is.null(cor))
    plot_z <- cor
  else if(!is.null(sn))
    plot_z <- sn
  else
    stop("z, cor, or sn need to be specified to plot the z axis", call. = F)

  if(!is.null(sn) && !is.null(min_sn))
    plot_z <- ifelse(sn > min_sn, plot_z, NA)

  if(!is.null(cor) && !is.null(min_cor))
    plot_z <- ifelse(cor > min_cor, plot_z, NA)

  if(all(is.na(plot_z)))
    plot_z = rep(-88, length.out = length(plot_z))

  if(type == "interactive"){
      p <- plot_ly(...) |>
          add_trace(
              x = x$metadata$x,
              y = x$metadata$y,
              z = if(!is.numeric(plot_z)) {
                  as.numeric(as.factor(plot_z))
              } else {
                  plot_z
              },
              colorscale = colorscale,
              type = "heatmap",
              hoverinfo = 'text',
              showscale = showlegend,
              text = ~ paste0(
                  if(!"file_name" %in% names(x$metadata)) 
                        paste0("row: ", 1:nrow(x$metadata))
                  else 
                        paste0("file name: ", x$metadata$file_name),
                  "<br>x: ",
                  x$metadata$x,
                  ", y: ",
                  x$metadata$y,
                  ", z: ",
                  plot_z,
                  if(!is.null(sn))
                      paste0("<br>snr: ", signif(sn, 2))
                  else
                      "",
                  if(!is.null(cor))
                      paste0("<br>cor: ", signif(cor, 2))
                  else
                      ""
              )
          ) |>
          layout(
              xaxis = list(
                  title = 'x',
                  zeroline = F,
                  showgrid = F
              ),
              yaxis = list(
                  title = 'y',
                  scaleanchor = "x",
                  scaleratio = 1,
                  zeroline = F,
                  showgrid = F
              ),
              plot_bgcolor = plot_bgcolor,
              paper_bgcolor = paper_bgcolor,
              showlegend = showlegend,
              font = font
          )
      
      if(!is.null(select)) {
          p <-
              p |> add_markers(
                  x = x$metadata$x[select],
                  y = x$metadata$y[select],
                  name = "Selected Spectrum"
              )
      }
  }
  if(type == "static"){
      mat <- matrix(plot_z, nrow = length(unique(x$metadata$x)), ncol = length(unique(x$metadata$y)))
      return(image(unique(x$metadata$x), unique(x$metadata$y), mat, col = heat.colors(100), main = "Heatmap", xlab = "X-axis", ylab = "Y-axis", asp = 1))
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
interactive_plot.OpenSpecy <- function(x,
                                       x2 = NULL,
                                       select = NULL,
                                       line = list(color = 'rgb(255, 255, 255)'),
                                       line2 = list(dash = "dot",
                                                    color = "rgb(255,0,0)"),
                                       font = list(color = '#FFFFFF'),
                                       plot_bgcolor = 'rgba(17, 0, 73, 0)',
                                       paper_bgcolor = 'rgb(0, 0, 0)',
                                       colorscale = 'Viridis',
                                       ...) {
  x <- as_OpenSpecy(x)

  # Generate the heatmap
  heat_map <- heatmap_spec(
    x,
    z = x$metadata$y,
    select = select,
    font = font,
    plot_bgcolor = plot_bgcolor,
    paper_bgcolor = paper_bgcolor,
    colorscale = colorscale
  )

  x3 <- filter_spec(x, logic = select)

  # Generate the spectral plot
  spectra_plot <- plotly_spec(
    x3,
    x2 = x2,
    line = line,
    line2 = line2,
    font = font,
    plot_bgcolor = plot_bgcolor,
    paper_bgcolor = paper_bgcolor
  )

  # Add margin to heatmap for separation
  heat_map <-
    heat_map |> layout(autosize = TRUE, margin = list(b = 100))

  # Combine both plots using subplot
  plot_grid <-
    subplot(
      heat_map,
      spectra_plot,
      nrows = 2,
      heights = c(0.6, 0.4),
      margin = 0.1
    )

  # Show the interactive plot
  return(plot_grid)
}
