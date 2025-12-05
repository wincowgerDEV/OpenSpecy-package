# Spectral resolution

Helper function for calculating the spectral resolution from
`wavenumber` data.

## Usage

``` r
spec_res(x, ...)

# Default S3 method
spec_res(x, ...)

# S3 method for class 'OpenSpecy'
spec_res(x, ...)
```

## Arguments

- x:

  a numeric vector with `wavenumber` data or an `OpenSpecy` object.

- ...:

  further arguments passed to subfunctions; currently not used.

## Value

`spec_res()` returns a single numeric value.

## Details

The spectral resolution is the the minimum wavenumber, wavelength, or
frequency difference between two lines in a spectrum that can still be
distinguished.

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
data("raman_hdpe")

spec_res(raman_hdpe)
#> [1] 3.00527
```
