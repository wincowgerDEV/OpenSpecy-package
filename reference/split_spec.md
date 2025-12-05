# Split Open Specy objects

Convert a list of Open Specy objects with any number of spectra into a
list of Open Specy objects with one spectrum each.

## Usage

``` r
split_spec(x)
```

## Arguments

- x:

  a list of OpenSpecy objects

## Value

A list of Open Specy objects each with 1 spectrum.

## Details

Function will accept a list of Open Specy objects of any length and will
split them to their individual components. For example a list of two
objects, an Open Specy with only one spectrum and an Open Specy with 50
spectra will return a list of length 51 each with Open Specy objects
that only have one spectrum.

## See also

[`c_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_spec.md)
for combining `OpenSpecy` objects.
[`collapse_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/def_features.md)
for summarizing `OpenSpecy` objects.

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
data("test_lib")
data("raman_hdpe")
listed <- list(test_lib, raman_hdpe)
test <- split_spec(listed)
test2 <- split_spec(list(test_lib))
```
