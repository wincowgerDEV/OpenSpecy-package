# Measure the area under band of spectra

Area under the band calculations are useful for quantifying spectral
features. Specialized fields in spectroscopy have different area under
the band regions of interest and their ratios that help with
understanding differences in materials. Additional processing is
typically required prior to calculating these values for accuracy and
reproducibility.

## Usage

``` r
area_under_band(x, ...)

# Default S3 method
area_under_band(x, ...)

# S3 method for class 'OpenSpecy'
area_under_band(x, min, max, na.rm = F, ...)
```

## Arguments

- x:

  an `OpenSpecy` object.

- min:

  a numeric value of the smallest wavenumber to begin calculation.

- max:

  a numeric value of the largest wavenumber to end calculation.

- na.rm:

  a logical value for whether to ignore NA values.

- ...:

  additional arguments passed to vapply.

## Value

Numeric vector of area under the band calculations for each spectrum in
the Open Specy object.

## Author

Win Cowger

## Examples

``` r
data("raman_hdpe")
#Single area calculation
area_under_band(raman_hdpe, min = 1000,max = 2000)
#> intensity 
#>     30213 
#Ratio of two areas. 
area_under_band(raman_hdpe, min = 1000,max = 2000)/area_under_band(raman_hdpe, min = 500,max = 700)
#> intensity 
#>  9.116777 
```
