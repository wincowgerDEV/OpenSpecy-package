#' @title Generic Open Specy Methods
#' @rdname gen_OpenSpecy
#'
#' @description
#' Methods to visualize and convert \code{OpenSpecy} objects.
#'
#' @details
#' \code{head()} shows the first few lines of an \code{OpenSpecy} object.
#' \code{print()} prints the contents of an \code{OpenSpecy} object.
#' \code{plot()} produces a \code{\link[graphics]{plot}()} of an OpenSpecy
#'
#' @param x an \code{OpenSpecy} object.
#' @param object an \code{OpenSpecy} object.
#' @param offset        Numeric value for vertical offset of each successive spectrum.
#'                      Defaults to 1. If 0, all spectra share the same baseline.
#' @param legend_var    Character string naming a metadata column in \code{x$metadata}
#'                      that labels/colors each spectrum. If NULL, spectra won't be labeled.
#' @param pallet        The base R graphics color pallet function to use. If NULL will default to all black.
#' @param main          Plot text for title. 
#' @param xlab          Plot x axis text
#' @param ylab          Plot y axis text
#' @param \ldots further arguments passed to the respective default method.
#'
#' @return
#' \code{head()}, \code{print()}, and \code{summary()} return a textual
#' representation of an \code{OpenSpecy} object.
#' \code{plot()} and \code{lines()} return a plot.
#' \code{as.data.frame()} and \code{as.data.table()} convert \code{OpenSpecy}
#' objects into tabular data.
#'
#' @examples
#' data("raman_hdpe")
#'
#' # Printing the OpenSpecy object
#' print(raman_hdpe)
#'
#' # Displaying the first few lines of the OpenSpecy object
#' head(raman_hdpe)
#'
#' # Plotting the spectra
#' plot(raman_hdpe)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link[utils]{head}()}, \code{\link[base]{print}()},
#' \code{\link[base]{summary}()}, \code{\link[graphics]{matplot}()}, and
#' \code{\link[graphics]{matlines}()},
#' \code{\link[base]{as.data.frame}()},
#' \code{\link[data.table]{as.data.table}()}
#'
#' @importFrom utils head
#' @importFrom graphics lines par text
#' @importFrom grDevices rainbow 
#' @export
head.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) |> head(...)
}

#' @rdname gen_OpenSpecy
#'
#' @method print OpenSpecy
#' @export
print.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) |> print(...)
  cat("\n$metadata\n")
  print(x$metadata)
}

#' @rdname gen_OpenSpecy
#'
#' @method plot OpenSpecy
#' @export
plot.OpenSpecy <- function(x,
                           offset = 0,
                           legend_var = NULL,
                           pallet = rainbow,
                           main = "Spectra Plot",
                           xlab = "Wavenumber (1/cm)",
                           ylab = "Intensity (a.u.)",
                           ...) {
    #-------------------------------------------------------------------
    # Basic input checks
    #-------------------------------------------------------------------
    num_spectra <- length(x$spectra)
    if (num_spectra == 0) {
        stop("No spectra found in the OpenSpecy object.")
    }
    
    # Ensure wavenumbers exist
    wave_nums <- x$wavenumber
    if (is.null(wave_nums) || length(wave_nums) == 0) {
        stop("No wavenumber data found in the OpenSpecy object.")
    }
    
    #-------------------------------------------------------------------
    # Assign groups/colors based on legend_var if provided
    #-------------------------------------------------------------------
    if (!is.null(legend_var) && legend_var %in% names(x$metadata)) {
        # Factor each spectrum’s group label
        group_factor <- factor(x$metadata[[legend_var]])
        # Create a color palette for all factor levels
        if(!is.null(pallet)){
            color_palette <- pallet(length(levels(group_factor)))
        }
        else{
            color_palette <- rep("black", num_spectra)
        }
        # Map each spectrum's group to a color
        spectrum_colors <- color_palette[as.numeric(group_factor)]
        # Labels come from the specified metadata column
        label_list <- as.character(x$metadata[[legend_var]])
    } else {
        # No valid legend_var; treat each spectrum as a separate label
        if(!is.null(pallet)){
            color_palette <- pallet(num_spectra)
        }
        else{
            color_palette <- rep("black", num_spectra)
        }
        group_factor <- factor(seq_len(num_spectra))
        spectrum_colors <- color_palette[as.numeric(group_factor)]
    }
    
    #-------------------------------------------------------------------
    # Establish global plot limits (x- and y-ranges)
    #   Incorporate vertical offset to accommodate stacked spectra
    #-------------------------------------------------------------------
    all_intensities <- unlist(x$spectra)
    y_min <- min(all_intensities, na.rm = TRUE)
    y_max <- max(all_intensities, na.rm = TRUE) + offset * (num_spectra - 1)
    
    # Increase left margin so labels do not get cut off
    op <- par(mar = c(4, 4, 2, 0) + 0.1)
    on.exit(par(op), add = TRUE)  # ensure margins are restored
    
    # Draw an empty plot with reversed x-axis
    plot(
        NA, NA,
        xlim = rev(range(wave_nums, na.rm = TRUE)),  # largest wavenumber on left
        ylim = c(y_min, y_max),
        main = main,
        xlab = xlab,
        ylab = ylab,
        ...
    )
    
    #-------------------------------------------------------------------
    # Plot each spectrum in sequence, offsetting vertically,
    # then place a text label on the left-hand side (largest wavenumber).
    #-------------------------------------------------------------------
    # If wave_nums is sorted ascending, wave_nums[length(wave_nums)] is the largest
    x_left_side <- wave_nums[length(wave_nums)]
    
    for (i in seq_len(num_spectra)) {
        # Vertical offset
        offset_val <- offset * (i - 1)
        # Add line
        lines(
            wave_nums,
            x$spectra[[i]] + offset_val,
            col = spectrum_colors[i],
            ...
        )
        if(!is.null(legend_var) && legend_var %in% names(x$metadata)){
            # Coordinates at the "left" end (largest wavenumber) for labeling
            y_left_side <- x$spectra[[i]][length(wave_nums)] + offset_val - 0.1
            
            # Place text slightly to the right of the data point
            text(
                x_left_side,
                y_left_side,
                labels = label_list[i],
                col = spectrum_colors[i],
                pos = 4,      # position 4 = right of the coordinate
                offset = 0.3, # small horizontal offset
                cex = 0.75     # slightly smaller text, for neatness
            )
        }
    }
    invisible(NULL)
}

#' @rdname gen_OpenSpecy
#'
#' @method summary OpenSpecy
#' @export
summary.OpenSpecy <- function(object, ...) {
  cat("$wavenumber\n")
  wl <- length(object$wavenumber)
  wr <- range(object$wavenumber)
  res <- spec_res(object)
  array(c(wl, wr, res), c(1,4), list("", c("Length", "Min.", "Max.",
                                           "Res."))) |>
    print()

  cat("\n$spectra\n")
  sl <- length(object$spectra)
  sr <- range(object$spectra, na.rm = T)
  array(c(sl, sr), c(1,3), list("", c("Number", "Min. Intensity",
                                      "Max. Intensity"))) |>
    print()

  cat("\n$metadata\n")
  xr <- range(object$metadata$x)
  yr <- range(object$metadata$y)
  t(array(c(xr, yr), c(2,2), list(c("Min.", "Max."), c("x", "y")))) |> print()
  names(object$metadata) |> print()
}

#' @rdname gen_OpenSpecy
#'
#' @method as.data.frame OpenSpecy
#' @export
as.data.frame.OpenSpecy <- function(x, ...) {
  data.frame(wavenumber = x$wavenumber, x$spectra)
}

#' @rdname gen_OpenSpecy
#'
#' @method as.data.table OpenSpecy
#'
#' @importFrom data.table data.table
#' @export
as.data.table.OpenSpecy <- function(x, ...) {
  data.table(wavenumber = x$wavenumber, x$spectra)
}
