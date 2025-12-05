# Ignore or remove NA intensities

Sometimes you want to keep or remove NA values in intensities to allow
for spectra with varying shapes to be analyzed together or maintained in
a single Open Specy object.

## Usage

``` r
manage_na(x, ...)

# Default S3 method
manage_na(x, lead_tail_only = TRUE, ig = c(NA), ...)

# S3 method for class 'OpenSpecy'
manage_na(x, lead_tail_only = TRUE, ig = c(NA), fun, type = "ignore", ...)
```

## Arguments

- x:

  a numeric vector or an R OpenSpecy object.

- lead_tail_only:

  logical whether to only look at leading adn tailing values.

- ig:

  character vector, values to ignore.

- fun:

  the name of the function you want run, this is only used if the
  "ignore" type is chosen.

- type:

  character of either "ignore" or "remove".

- ...:

  further arguments passed to `fun`.

## Value

`manage_na()` return logical vectors of NA locations (if vector
provided) or an `OpenSpecy` object with ignored or removed NA values.

## See also

`OpenSpecy` object to be matched with a reference library
[`fill_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md)
can be used to fill NA values in Open Specy objects.
[`restrict_range()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_range.md)
can be used to restrict spectral ranges in other ways than removing NAs.

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
manage_na(c(NA, -1, NA, 1, 10))
#> [1]  TRUE FALSE FALSE FALSE FALSE
manage_na(c(NA, -1, NA, 1, 10), lead_tail_only = FALSE)
#> [1]  TRUE FALSE  TRUE FALSE FALSE
manage_na(c(NA, 0, NA, 1, 10), lead_tail_only = FALSE, ig = c(NA,0))
#> [1]  TRUE  TRUE  TRUE FALSE FALSE
data(raman_hdpe)
raman_hdpe$spectra[[1]][1:10] <- NA

#would normally return all NA without na.rm = TRUE but doesn't here.
manage_na(raman_hdpe, fun = make_rel)
#>      wavenumber  intensity
#>           <num>      <num>
#>   1:    301.040         NA
#>   2:    304.632         NA
#>   3:    308.221         NA
#>   4:    311.810         NA
#>   5:    315.398         NA
#>  ---                      
#> 960:   3187.990 0.03870968
#> 961:   3190.520 0.03870968
#> 962:   3193.060 0.04387097
#> 963:   3195.590 0.04387097
#> 964:   3198.120 0.03354839
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

#will remove the first 10 values we set to NA
manage_na(raman_hdpe, type = "remove")
#>      wavenumber intensity
#>           <num>     <int>
#>   1:    336.889        48
#>   2:    340.467        44
#>   3:    344.042        49
#>   4:    347.618        52
#>   5:    351.190        48
#>  ---                     
#> 950:   3187.990        71
#> 951:   3190.520        71
#> 952:   3193.060        75
#> 953:   3195.590        75
#> 954:   3198.120        67
#> 
#> $metadata
#>        x     y  user_name spectrum_type spectrum_identity      organization
#>    <int> <int>     <char>        <char>            <char>            <char>
#> 1:     1     1 Win Cowger         Raman              HDPE Horiba Scientific
#>     license                                                        session_id
#>      <char>                                                            <char>
#> 1: CC BY-NC 5728ddde4f649fd71f6f487fc5ad8d80/dc85257201307a131e71d9ec24aaccbf
#>                             file_id    col_id
#>                              <char>    <char>
#> 1: cb06ce2846b119d932fb6696479a445b intensity
```
