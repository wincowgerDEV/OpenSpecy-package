# Conform spectra to a standard wavenumber series

Spectra can be conformed to a standard suite of wavenumbers to be
compared with a reference library or to be merged to other spectra.

## Usage

``` r
conform_spec(x, ...)

# Default S3 method
conform_spec(x, ...)

# S3 method for class 'OpenSpecy'
conform_spec(x, range = NULL, res = 5, allow_na = F, type = "interp", ...)
```

## Arguments

- x:

  a list object of class `OpenSpecy`.

- range:

  a vector of new wavenumber values, can be just supplied as a min and
  max value.

- res:

  spectral resolution adjusted to or `NULL` if the raw range should be
  used.

- allow_na:

  logical; should NA values in places beyond the wavenumbers of the
  dataset be allowed?

- type:

  the type of wavenumber adjustment to make. `"interp"` results in
  linear interpolation while `"roll"` conducts a nearest rolling join of
  the wavenumbers. `"mean_up"` only works when Spectra are being
  aggregated, we take the mean of the intensities within the wavenumber
  specified. This can maintain smaller peaks and make spectra more
  similar to it's less resolved relatives. mean_up option is still
  experimental.

- ...:

  further arguments passed to
  [`approx()`](https://rdrr.io/r/stats/approxfun.html)

## Value

[`adj_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_intens.md)
returns a data frame containing two columns named `"wavenumber"` and
`"intensity"`

## See also

[`restrict_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md)
and
[`flatten_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md)
for adjusting wavenumber ranges;
[`subtr_baseline()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/subtr_baseline.md)
for spectral background correction

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
data("raman_hdpe")
conform_spec(raman_hdpe, c(1000, 2000))
#>      wavenumber intensity
#>           <num>     <num>
#>   1:       1000  61.69439
#>   2:       1005  69.33837
#>   3:       1010  66.49849
#>   4:       1015  65.00000
#>   5:       1020  70.80060
#>  ---                     
#> 197:       1980 100.32542
#> 198:       1985 114.12203
#> 199:       1990 122.80339
#> 200:       1995 106.29492
#> 201:       2000 115.16610
#> 
#> $metadata
#>        x     y  user_name spectrum_type spectrum_identity      organization
#>    <int> <int>     <char>        <char>            <char>            <char>
#> 1:     1     1 Win Cowger         Raman              HDPE Horiba Scientific
#>     license                                                        session_id
#>      <char>                                                            <char>
#> 1: CC BY-NC 5728ddde4f649fd71f6f487fc5ad8d80/dc85257201307a131e71d9ec24aaccbf
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b
```
