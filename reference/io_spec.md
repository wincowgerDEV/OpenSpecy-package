# Read and write spectral data

Functions for reading and writing spectral data to and from OpenSpecy
format. `OpenSpecy` objects are lists with components `wavenumber`,
`spectra`, and `metadata`. Currently supported formats are .y(a)ml,
.json, .csv, or .rds.

## Usage

``` r
write_spec(x, ...)

# Default S3 method
write_spec(x, ...)

# S3 method for class 'OpenSpecy'
write_spec(x, file, method = NULL, digits = getOption("digits"), ...)

read_spec(file, method = NULL, ...)

as_hyperSpec(x)
```

## Arguments

- x:

  an object of class
  [`OpenSpecy`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md).

- file:

  file path to be read from or written to.

- method:

  optional; function to be used as a custom reader or writer. Defaults
  to the appropriate function based on the file extension.

- digits:

  number of significant digits to use when formatting numeric values;
  defaults to
  [`getOption`](https://rdrr.io/r/base/options.html)`("digits")`.

- ...:

  further arguments passed to the submethods.

## Value

`read_spec()` reads data formatted as an `OpenSpecy` object and returns
a list object of class
[`OpenSpecy`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
containing spectral data. `write_spec()` writes a file for an object of
class
[`OpenSpecy`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
containing spectral data. `as_hyperspec()` converts an `OpenSpecy`
object to a
[`hyperSpec-class`](https://r-hyperspec.github.io/hyperSpec/reference/hyperSpec-class.html)
object.

## Details

Due to floating point number errors there may be some differences in the
precision of the numbers returned if using multiple devices for .json
and .yaml files but the numbers should be nearly identical.
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) should return the
exact same object every time.

## See also

[`OpenSpecy()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md);
[`read_text()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
[`read_asp()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
[`read_spa()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
[`read_spc()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
and
[`read_jdx()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md)
for text files, .asp, .spa, .spa, .spc, and .jdx formats, respectively;
[`read_zip()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
and
[`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
for wrapper functions;
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html);
[`readRDS()`](https://rdrr.io/r/base/readRDS.html);
[`write_yaml()`](https://rdrr.io/pkg/yaml/man/write_yaml.html);
[`read_yaml()`](https://rdrr.io/pkg/yaml/man/read_yaml.html);
[`write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html);
[`read_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html);

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
read_extdata("raman_hdpe.yml") |> read_spec()
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
#>                             file_id      file_name    col_id
#>                              <char>         <char>    <char>
#> 1: cb06ce2846b119d932fb6696479a445b raman_hdpe.yml intensity
read_extdata("raman_hdpe.json") |> read_spec()
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
#>                             file_id       file_name    col_id
#>                              <char>          <char>    <char>
#> 1: cb06ce2846b119d932fb6696479a445b raman_hdpe.json intensity
read_extdata("raman_hdpe.rds") |> read_spec()
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
#>                             file_id      file_name
#>                              <char>         <char>
#> 1: cb06ce2846b119d932fb6696479a445b raman_hdpe.rds
read_extdata("raman_hdpe.csv") |> read_spec()
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
#>        x     y      file_name  license    col_id
#>    <int> <int>         <char>   <char>    <char>
#> 1:     1     1 raman_hdpe.csv CC BY-NC intensity
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/e0ae8a6c03aabd77378f9d0c367d565c
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b

if (FALSE) { # \dontrun{
data(raman_hdpe)
write_spec(raman_hdpe, "raman_hdpe.yml")
write_spec(raman_hdpe, "raman_hdpe.json")
write_spec(raman_hdpe, "raman_hdpe.rds")
write_spec(raman_hdpe, "raman_hdpe.csv")

# Convert an OpenSpecy object to a hyperSpec object
hyper <- as_hyperSpec(raman_hdpe)
} # }
```
