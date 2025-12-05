# Process Spectra

`process_spec()` is a monolithic wrapper function for all spectral
processing steps.

## Usage

``` r
process_spec(x, ...)

# Default S3 method
process_spec(x, ...)

# S3 method for class 'OpenSpecy'
process_spec(
  x,
  active = TRUE,
  adj_intens = FALSE,
  adj_intens_args = list(type = "none"),
  conform_spec = TRUE,
  conform_spec_args = list(range = NULL, res = 5, type = "interp"),
  restrict_range = FALSE,
  restrict_range_args = list(min = 0, max = 6000),
  flatten_range = FALSE,
  flatten_range_args = list(min = 2200, max = 2420),
  subtr_baseline = FALSE,
  subtr_baseline_args = list(type = "polynomial", degree = 8, raw = FALSE, baseline =
    NULL),
  smooth_intens = TRUE,
  smooth_intens_args = list(polynomial = 3, window = 11, derivative = 1, abs = TRUE),
  make_rel = TRUE,
  make_rel_args = list(na.rm = TRUE),
  ...
)
```

## Arguments

- x:

  an `OpenSpecy` object.

- active:

  logical; indicating whether to perform processing. If `TRUE`, the
  processing steps will be applied. If `FALSE`, the original data will
  be returned.

- adj_intens:

  logical; describing whether to adjust the intensity units.

- adj_intens_args:

  named list of arguments passed to
  [`adj_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_intens.md).

- conform_spec:

  logical; whether to conform the spectra to a new wavenumber range and
  resolution.

- conform_spec_args:

  named list of arguments passed to
  [`conform_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/conform_spec.md).

- restrict_range:

  logical; indicating whether to restrict the wavenumber range of the
  spectra.

- restrict_range_args:

  named list of arguments passed to
  [`restrict_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md).

- flatten_range:

  logical; indicating whether to flatten the range around the carbon
  dioxide region.

- flatten_range_args:

  named list of arguments passed to
  [`flatten_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md).

- subtr_baseline:

  logical; indicating whether to subtract the baseline from the spectra.

- subtr_baseline_args:

  named list of arguments passed to
  [`subtr_baseline()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/subtr_baseline.md).

- smooth_intens:

  logical; indicating whether to apply a smoothing filter to the
  spectra.

- smooth_intens_args:

  named list of arguments passed to
  [`smooth_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/smooth_intens.md).

- make_rel:

  logical; if `TRUE` spectra are automatically normalized with
  [`make_rel()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/make_rel.md).

- make_rel_args:

  named list of arguments passed to
  [`make_rel()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/make_rel.md).

- na.rm:

  Whether to allow NA or set all NA values to

- ...:

  further arguments passed to subfunctions.

## Value

`process_spec()` returns an `OpenSpecy` object with processed spectra
based on the specified parameters.

## Examples

``` r
data("raman_hdpe")
plot(raman_hdpe)


# Process spectra with range restriction and baseline subtraction
process_spec(raman_hdpe,
             restrict_range = TRUE,
             restrict_range_args = list(min = 500, max = 3000),
             subtr_baseline = TRUE,
             subtr_baseline_args = list(type = "polynomial",
                                        polynomial = 8)) |>
  plot()


# Process spectra with smoothing and derivative
process_spec(raman_hdpe,
             smooth_intens = TRUE,
             smooth_intens_args = list(
               polynomial = 3,
               window = 11,
               derivative = 1
               )
             ) |>
  plot()

```
