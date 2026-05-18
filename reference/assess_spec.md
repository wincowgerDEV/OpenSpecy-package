# Assess common spectral quality issues

`assess_spec()` scans spectra for common quality-control issues and
returns one row for each issue found.

## Usage

``` r
assess_spec(x, ...)

# Default S3 method
assess_spec(x, ...)

# S3 method for class 'OpenSpecy'
assess_spec(
  x,
  checks = c("high_tail", "silent_region", "co2_region", "missing_values",
    "flat_spectrum", "negative_intensity", "low_snr"),
  high_prob = 0.9,
  tail_n = 5L,
  silent_region = c(1800, 2000),
  co2_region = c(2200, 2420),
  snr_threshold = 4,
  flat_tol = sqrt(.Machine$double.eps),
  negative_tol = 0,
  na.rm = TRUE,
  ...
)
```

## Arguments

- x:

  an `OpenSpecy` object.

- checks:

  character; checks to run. Options include `"high_tail"`,
  `"silent_region"`, `"co2_region"`, `"missing_values"`,
  `"flat_spectrum"`, `"negative_intensity"`, and `"low_snr"`.

- high_prob:

  numeric; spectrum-wide quantile used as the high intensity threshold
  for tail and region checks.

- tail_n:

  integer; number of points to check at each end of the spectrum.

- silent_region:

  numeric length two; wavenumber range expected to be mostly silent.

- co2_region:

  numeric length two; carbon dioxide wavenumber range.

- snr_threshold:

  numeric; spectra with run signal-to-noise below this value are
  flagged.

- flat_tol:

  numeric; maximum finite intensity range considered flat.

- negative_tol:

  numeric; minimum allowed intensity before a spectrum is flagged as
  negative.

- na.rm:

  logical; indicating whether missing values should be removed when
  calculating thresholds and metrics.

- ...:

  further arguments passed to
  [`sig_noise()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/sig_noise.md)
  for the `"low_snr"` check.

## Value

A
[`data.table-class()`](https://rdrr.io/pkg/data.table/man/data.table-class.html)
with one row per issue found and columns describing the spectrum, check,
issue, likely cause, potential fix, metric value, threshold, and region.
If no issues are found, an empty `data.table` with the same columns is
returned.

## Author

Win Cowger

## Examples

``` r
data("raman_hdpe")
assess_spec(raman_hdpe)
#>    spectrum_index spectrum_id      check
#>             <int>      <char>     <char>
#> 1:              1   intensity co2_region
#>                                              issue
#>                                             <char>
#> 1: High intensity in CO2 region (infrared spectra)
#>                                                                     description
#>                                                                          <char>
#> 1: The maximum intensity in 2200-2420 is above the spectrum-wide high quantile.
#>                                                                                     likely_cause
#>                                                                                           <char>
#> 1: Carbon dioxide present in signal, basline correction issues, or background collection issues.
#>                                                                                                                             potential_fix
#>                                                                                                                                    <char>
#> 1: Flatten or remove the CO2 region, add instrument's atmouspheric correction, purge the instrument, or rerun the background or spectrum.
#>                  metric value threshold region_min region_max
#>                  <char> <num>     <num>      <num>      <num>
#> 1: max_region_intensity   142     128.7       2200       2420
```
