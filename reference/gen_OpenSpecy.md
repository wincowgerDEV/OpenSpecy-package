# Generic Open Specy Methods

Methods to visualize and convert `OpenSpecy` objects.

## Usage

``` r
# S3 method for class 'OpenSpecy'
head(x, ...)

# S3 method for class 'OpenSpecy'
print(x, ...)

# S3 method for class 'OpenSpecy'
plot(
  x,
  offset = 0,
  legend_var = NULL,
  pallet = rainbow,
  main = "Spectra Plot",
  xlab = "Wavenumber (1/cm)",
  ylab = "Intensity (a.u.)",
  ...
)

# S3 method for class 'OpenSpecy'
summary(object, ...)

# S3 method for class 'OpenSpecy'
as.data.frame(x, ...)

# S3 method for class 'OpenSpecy'
as.data.table(x, ...)
```

## Arguments

- x:

  an `OpenSpecy` object.

- offset:

  Numeric value for vertical offset of each successive spectrum.
  Defaults to 1. If 0, all spectra share the same baseline.

- legend_var:

  Character string naming a metadata column in `x$metadata` that
  labels/colors each spectrum. If NULL, spectra won't be labeled.

- pallet:

  The base R graphics color pallet function to use. If NULL will default
  to all black.

- main:

  Plot text for title.

- xlab:

  Plot x axis text

- ylab:

  Plot y axis text

- object:

  an `OpenSpecy` object.

- ...:

  further arguments passed to the respective default method.

## Value

[`head()`](https://rdrr.io/r/utils/head.html),
[`print()`](https://r-hyperspec.github.io/hyperSpec/reference/show.html),
and
[`summary()`](https://r-hyperspec.github.io/hyperSpec/reference/show.html)
return a textual representation of an `OpenSpecy` object.
[`plot()`](https://rdrr.io/r/base/plot.html) and
[`lines()`](https://rdrr.io/r/graphics/lines.html) return a plot.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
[`as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html)
convert `OpenSpecy` objects into tabular data.

## Details

[`head()`](https://rdrr.io/r/utils/head.html) shows the first few lines
of an `OpenSpecy` object.
[`print()`](https://r-hyperspec.github.io/hyperSpec/reference/show.html)
prints the contents of an `OpenSpecy` object.
[`plot()`](https://rdrr.io/r/base/plot.html) produces a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) of an OpenSpecy

## See also

[`head()`](https://rdrr.io/r/utils/head.html),
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`matplot()`](https://rdrr.io/r/graphics/matplot.html), and
[`matlines()`](https://rdrr.io/r/graphics/matplot.html),
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
[`as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html)

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
data("raman_hdpe")

# Printing the OpenSpecy object
print(raman_hdpe)
#>      wavenumber intensity
#>           <num>     <int>
#>   1:    301.040        26
#>   2:    304.632        50
#>   3:    308.221        48
#>   4:    311.810        45
#>   5:    315.398        46
#>  ---                     
#> 960:   3187.990        71
#> 961:   3190.520        71
#> 962:   3193.060        75
#> 963:   3195.590        75
#> 964:   3198.120        67
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

# Displaying the first few lines of the OpenSpecy object
head(raman_hdpe)
#>    wavenumber intensity
#>         <num>     <int>
#> 1:    301.040        26
#> 2:    304.632        50
#> 3:    308.221        48
#> 4:    311.810        45
#> 5:    315.398        46
#> 6:    318.983        42

# Plotting the spectra
plot(raman_hdpe)

```
