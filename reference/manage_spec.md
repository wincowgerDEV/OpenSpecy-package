# Manage spectral objects

`c_spec()` concatenates `OpenSpecy` objects. `sample_spec()` samples
spectra from an `OpenSpecy` object. `merge_map()` merge two `OpenSpecy`
objects from spectral maps.

## Usage

``` r
c_spec(x, ...)

# Default S3 method
c_spec(x, ...)

# S3 method for class 'OpenSpecy'
c_spec(x, ...)

# S3 method for class 'list'
c_spec(x, range = NULL, res = 5, ...)

sample_spec(x, ...)

# Default S3 method
sample_spec(x, ...)

# S3 method for class 'OpenSpecy'
sample_spec(x, size = 1, prob = NULL, ...)

merge_map(x, ...)

# Default S3 method
merge_map(x, ...)

# S3 method for class 'OpenSpecy'
merge_map(x, ...)

# S3 method for class 'list'
merge_map(x, origins = NULL, ...)
```

## Arguments

- x:

  a list of `OpenSpecy` objects or of file paths.

- range:

  a numeric providing your own wavenumber ranges or character argument
  called `"common"` to let `c_spec()` find the common wavenumber range
  of the supplied spectra. `NULL` will interpret the spectra having all
  the same wavenumber range.

- res:

  defaults to `NULL`, the resolution you want the output wavenumbers to
  be.

- size:

  the number of spectra to sample.

- prob:

  probabilities to use for the sampling.

- origins:

  a list with 2 value vectors of x y coordinates for the offsets of each
  image.

- ...:

  further arguments passed to submethods.

## Value

`c_spec()` and `sample_spec()` return `OpenSpecy` objects.

## See also

[`conform_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/conform_spec.md)
for conforming wavenumbers

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
# Concatenating spectra
spectra <- lapply(c(read_extdata("raman_hdpe.csv"),
                    read_extdata("ftir_ldpe_soil.asp")), read_any)
common <- c_spec(spectra, range = "common", res = 5)
range <- c_spec(spectra, range = c(1000, 2000), res = 5)

# Sampling spectra
tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
sampled <- sample_spec(tiny_map, size = 3)
```
