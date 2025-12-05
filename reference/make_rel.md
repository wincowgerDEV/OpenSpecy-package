# Make spectral intensities relative

`make_rel()` converts intensities `x` into relative values between 0 and
1 using the standard normalization equation. If `na.rm` is `TRUE`,
missing values are removed before the computation proceeds.

## Usage

``` r
make_rel(x, ...)

# Default S3 method
make_rel(x, na.rm = FALSE, ...)

# S3 method for class 'OpenSpecy'
make_rel(x, na.rm = FALSE, ...)
```

## Arguments

- x:

  a numeric vector or an R OpenSpecy object

- na.rm:

  logical. Should missing values be removed?

- ...:

  further arguments passed to `make_rel()`.

## Value

`make_rel()` return numeric vectors (if vector provided) or an
`OpenSpecy` object with the normalized intensity data.

## Details

`make_rel()` is used to retain the relative height proportions between
spectra while avoiding the large numbers that can result from some
spectral instruments.

## See also

[`min()`](https://rdrr.io/r/base/Extremes.html) and
[`round()`](https://rdrr.io/r/base/Round.html);
[`adj_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_intens.md)
for log transformation functions;
[`conform_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/conform_spec.md)
for conforming wavenumbers of an `OpenSpecy` object to be matched with a
reference library

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
make_rel(c(-1000, -1, 0, 1, 10))
#> [1] 0.0000000 0.9891089 0.9900990 0.9910891 1.0000000
```
