# Range restriction and flattening for spectra

`restrict_range()` restricts wavenumber ranges to user specified values.
Multiple ranges can be specified by inputting a series of max and min
values in order. `flatten_range()` will flatten ranges of the spectra
that should have no peaks. Multiple ranges can be specified by inputting
the series of max and min values in order.

## Usage

``` r
restrict_range(x, ...)

# Default S3 method
restrict_range(x, ...)

# S3 method for class 'OpenSpecy'
restrict_range(x, min, max, make_rel = TRUE, ...)

flatten_range(x, ...)

# Default S3 method
flatten_range(x, ...)

# S3 method for class 'OpenSpecy'
flatten_range(x, min = 2200, max = 2400, make_rel = TRUE, ...)
```

## Arguments

- x:

  an `OpenSpecy` object.

- min:

  a vector of minimum values for the range to be flattened.

- max:

  a vector of maximum values for the range to be flattened.

- make_rel:

  logical; should the output intensities be normalized to the range \[0,
  1\] using
  [`make_rel()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/make_rel.md)
  function?

- ...:

  additional arguments passed to subfunctions; currently not in use.

## Value

An `OpenSpecy` object with the spectral intensities within specified
ranges restricted or flattened.

## See also

[`conform_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/conform_spec.md)
for conforming wavenumbers to be matched with a reference library;
[`adj_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_intens.md)
for log transformation functions;
[`min()`](https://rdrr.io/r/base/Extremes.html) and
[`round()`](https://rdrr.io/r/base/Round.html)

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
test_noise <- as_OpenSpecy(x = seq(400,4000, by = 10),
                           spectra = data.frame(intensity = rnorm(361)))
plot(test_noise)


restrict_range(test_noise, min = 1000, max = 2000)
#>      wavenumber intensity
#>           <num>     <num>
#>   1:       1000 0.7862717
#>   2:       1010 0.4993344
#>   3:       1020 0.3027464
#>   4:       1030 0.3453091
#>   5:       1040 0.7759545
#>  ---                     
#>  97:       1960 0.7164056
#>  98:       1970 0.7209565
#>  99:       1980 0.1502945
#> 100:       1990 0.7486770
#> 101:       2000 0.4860082
#> 
#> $metadata
#>        x     y  license    col_id                          file_id
#>    <int> <int>   <char>    <char>                           <char>
#> 1:     1     1 CC BY-NC intensity f58abfcd86e997e44e1b627dfbbb4b0e

flattened_intensities <- flatten_range(test_noise, min = c(1000, 2000),
                                       max = c(1500, 2500))
plot(flattened_intensities)

```
