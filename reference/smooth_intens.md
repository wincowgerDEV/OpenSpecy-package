# Smooth spectral intensities

This smoother can enhance the signal to noise ratio of the data useing a
Savitzky-Golay or Whittaker-Henderson filter.

## Usage

``` r
smooth_intens(x, ...)

# Default S3 method
smooth_intens(x, ...)

# S3 method for class 'OpenSpecy'
smooth_intens(
  x,
  polynomial = 3,
  window = 11,
  derivative = 1,
  abs = TRUE,
  lambda = 1600,
  d = 2,
  type = "sg",
  lag = 2,
  make_rel = TRUE,
  ...
)

calc_window_points(x, ...)

# Default S3 method
calc_window_points(x, wavenum_width = 70, ...)

# S3 method for class 'OpenSpecy'
calc_window_points(x, wavenum_width = 70, ...)
```

## Arguments

- x:

  an object of class `OpenSpecy` or vector for `calc_window_points()`.

- polynomial:

  polynomial order for the filter

- window:

  number of data points in the window, filter length (must be odd).

- derivative:

  the derivative order if you want to calculate the derivative. Zero
  (default) is no derivative.

- abs:

  logical; whether you want to calculate the absolute value of the
  resulting output.

- lambda:

  smoothing parameter for Whittaker-Henderson smoothing, 50 results in
  rough smoothing and 10^4 results in a high level of smoothing.

- d:

  order of differences to use for Whittaker-Henderson smoothing,
  typically set to 2.

- type:

  the type of smoothing to use "wh" for Whittaker-Henerson or "sg" for
  Savitzky-Golay.

- lag:

  the lag to use for the numeric derivative calculation if using
  Whittaker-Henderson. Greater values lead to smoother derivatives, 1 or
  2 is common.

- make_rel:

  logical; if `TRUE` spectra are automatically normalized with
  [`make_rel()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/make_rel.md).

- wavenum_width:

  the width of the window you want in wavenumbers.

- ...:

  further arguments passed to
  [`sgolay()`](https://rdrr.io/pkg/signal/man/sgolay.html).

## Value

`smooth_intens()` returns an `OpenSpecy` object.

`calc_window_points()` returns a single numberic vector object of the
number of points needed to fill the window and can be passed to
`smooth_intens()`. For many applications, this is more reusable than
specifying a static number of points.

## Details

For Savitzky-Golay this is a wrapper around the filter function in the
signal package to improve integration with other Open Specy functions. A
typical good smooth can be achieved with 11 data point window and a 3rd
or 4th order polynomial. For Whittaker-Henderson, the code is largely
based off of the whittaker() function in the pracma package. In general
Whittaker-Henderson is expected to be slower but more robust than
Savitzky-Golay.

## References

Savitzky A, Golay MJ (1964). “Smoothing and Differentiation of Data by
Simplified Least Squares Procedures.” *Analytical Chemistry*, **36**(8),
1627–1639.

## See also

[`sgolay()`](https://rdrr.io/pkg/signal/man/sgolay.html)

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
data("raman_hdpe")

smooth_intens(raman_hdpe)
#>      wavenumber   intensity
#>           <num>       <num>
#>   1:    301.040 0.102500885
#>   2:    304.632 0.063171177
#>   3:    308.221 0.031695428
#>   4:    311.810 0.008073638
#>   5:    315.398 0.007614310
#>  ---                       
#> 960:   3187.990 0.002890315
#> 961:   3190.520 0.005482884
#> 962:   3193.060 0.009382631
#> 963:   3195.590 0.014589555
#> 964:   3198.120 0.021103657
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

smooth_intens(raman_hdpe, window = calc_window_points(x = raman_hdpe, wavenum_width = 70))
#>      wavenumber  intensity
#>           <num>      <num>
#>   1:    301.040 0.06819092
#>   2:    304.632 0.05691608
#>   3:    308.221 0.04668103
#>   4:    311.810 0.03748578
#>   5:    315.398 0.02933032
#>  ---                      
#> 960:   3187.990 0.01418184
#> 961:   3190.520 0.01677777
#> 962:   3193.060 0.01949510
#> 963:   3195.590 0.02233386
#> 964:   3198.120 0.02529402
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

smooth_intens(raman_hdpe, lambda = 1600, d = 2, lag = 2, type = "wh")
#>      wavenumber  intensity
#>           <num>      <num>
#>   1:    301.040 0.09445155
#>   2:    304.632 0.08547362
#>   3:    308.221 0.07684707
#>   4:    311.810 0.06877658
#>   5:    315.398 0.06138149
#>  ---                      
#> 960:   3187.990 0.07254546
#> 961:   3190.520 0.08301247
#> 962:   3193.060 0.09396644
#> 963:   3195.590 0.10515688
#> 964:   3198.120 0.11642380
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
