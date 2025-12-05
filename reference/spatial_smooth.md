# Spatial Smoothing of OpenSpecy Objects

Applies spatial smoothing to an `OpenSpecy` object using a Gaussian
filter.

## Usage

``` r
spatial_smooth(x, sigma = c(1, 1, 1), ...)
```

## Arguments

- x:

  an `OpenSpecy` object.

- sigma:

  a numeric vector specifying the standard deviations for the Gaussian
  kernel in the x and y dimensions, respectively.

- ...:

  further arguments passed to or from other methods.

## Value

An `OpenSpecy` object with smoothed spectra.

## Details

This function performs spatial smoothing on the spectral data in an
`OpenSpecy` object. It assumes that the spatial coordinates are provided
in the `metadata` element of the object, specifically in the `x` and `y`
columns, and that there is a `col_id` column in `metadata` that matches
the column names in the `spectra` data.table.

## See also

[`as_OpenSpecy()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md),
[`gaussianSmooth()`](https://rdrr.io/pkg/mmand/man/gaussianSmooth.html)

## Author

Win Cowger
