# Calculate signal and noise metrics for OpenSpecy objects

This function calculates common signal and noise metrics for `OpenSpecy`
objects.

## Usage

``` r
sig_noise(x, ...)

# Default S3 method
sig_noise(x, ...)

# S3 method for class 'OpenSpecy'
sig_noise(
  x,
  metric = "run_sig_over_noise",
  na.rm = TRUE,
  prob = 0.5,
  step = 20,
  breaks = seq(min(unlist(x$spectra)), max(unlist(x$spectra)), length =
    ((nrow(x$spectra)^(1/3)) * (max(unlist(x$spectra)) - min(unlist(x$spectra))))/(2 *
    IQR(unlist(x$spectra)))),
  sig_min = NULL,
  sig_max = NULL,
  noise_min = NULL,
  noise_max = NULL,
  abs = T,
  spatial_smooth = F,
  sigma = c(1, 1),
  threshold = NULL,
  ...
)
```

## Arguments

- x:

  an `OpenSpecy` object.

- metric:

  character; specifying the desired metric to calculate. Options include
  `"sig"` (mean intensity), `"noise"` (standard deviation of intensity),
  `"sig_times_noise"` (absolute value of signal times noise),
  `"sig_over_noise"` (absolute value of signal / noise),
  `"run_sig_over_noise"` (absolute value of signal / noise where signal
  is estimated as the max intensity and noise is estimated as the height
  of a low intensity region.), `"log_tot_sig"` (sum of the inverse log
  intensities, useful for spectra in log units), `"tot_sig"` (sum of
  intensities), or `"entropy"` (Shannon entropy of intensities)..

- na.rm:

  logical; indicating whether missing values should be removed when
  calculating signal and noise. Default is `TRUE`.

- prob:

  numeric single value; the probability to retrieve for the quantile
  where the noise will be interpreted with the run_sig_over_noise
  option.

- step:

  numeric; the step size of the region to look for the
  run_sig_over_noise option.

- breaks:

  numeric; the number or positions of the breaks for entropy
  calculation. Defaults to infer a decent value from the data.

- sig_min:

  numeric; the minimum wavenumber value for the signal region.

- sig_max:

  numeric; the maximum wavenumber value for the signal region.

- noise_min:

  numeric; the minimum wavenumber value for the noise region.

- noise_max:

  numeric; the maximum wavenumber value for the noise region.

- abs:

  logical; whether to return the absolute value of the result

- spatial_smooth:

  logical; whether to spatially smooth the sig/noise using the xy
  coordinates and a gaussian smoother.

- sigma:

  numeric; two value vector describing standard deviation for smoother
  in each dimension, y is specified first followed by x, should be the
  same for each in most cases.

- threshold:

  numeric; if NULL, no threshold is set, otherwise use a numeric value
  to set the target threshold which true signal or noise should be
  above. The function will return a logical value instead of numeric if
  a threshold is set.

- ...:

  further arguments passed to subfunctions; currently not used.

## Value

A numeric vector containing the calculated metric for each spectrum in
the `OpenSpecy` object or logical value if threshold is set describing
if the numbers where above or equal to (TRUE) the threshold.

## See also

[`restrict_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md)

## Examples

``` r
data("raman_hdpe")

sig_noise(raman_hdpe, metric = "sig")
#> intensity 
#>  101.1691 
sig_noise(raman_hdpe, metric = "noise")
#> intensity 
#>  61.00777 
sig_noise(raman_hdpe, metric = "sig_times_noise")
#> intensity 
#>    6172.1 
```
