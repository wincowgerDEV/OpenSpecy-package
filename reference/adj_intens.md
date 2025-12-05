# Adjust spectral intensities to standard absorbance units.

Converts reflectance or transmittance intensity units to absorbance
units and adjust log or exp transformed units.

## Usage

``` r
adj_intens(x, ...)

# Default S3 method
adj_intens(x, type = "none", make_rel = TRUE, log_exp = "none", ...)

# S3 method for class 'OpenSpecy'
adj_intens(x, type = "none", make_rel = TRUE, log_exp = "none", ...)
```

## Arguments

- x:

  a list object of class `OpenSpecy`.

- type:

  a character string specifying whether the input spectrum is in
  absorbance units (`"none"`, default) or needs additional conversion
  from `"reflectance"` or `"transmittance"` data.

- make_rel:

  logical; if `TRUE` spectra are automatically normalized with
  [`make_rel()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/make_rel.md).

- log_exp:

  a character string specifying whether the input needs to be log
  transformed `"log"`, exp transformed `"exp"`, or not (`"none"`,
  default).

- ...:

  further arguments passed to submethods; this is to
  [`adj_neg()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/data_norm.md)
  for `adj_intens()` and to
  [`conform_res()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/data_norm.md)
  for `conform_intens()`.

## Value

`adj_intens()` returns a data frame containing two columns named
`"wavenumber"` and `"intensity"`.

## Details

Many of the Open Specy functions will assume that the spectrum is in
absorbance units. For example, see
[`subtr_baseline()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/subtr_baseline.md).
To run those functions properly, you will need to first convert any
spectra from transmittance or reflectance to absorbance using this
function. The transmittance adjustment uses the \\log(1 / T)\\
calculation which does not correct for system and particle
characteristics. The reflectance adjustment uses the Kubelka-Munk
equation \\(1 - R)^2 / 2R\\. We assume that the reflectance intensity is
a percent from 1-100 and first correct the intensity by dividing by 100
so that it fits the form expected by the equation.

## See also

[`subtr_baseline()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/subtr_baseline.md)
for spectral background correction.

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
data("raman_hdpe")

adj_intens(raman_hdpe)
#>      wavenumber  intensity
#>           <num>      <num>
#>   1:    301.040 0.00000000
#>   2:    304.632 0.03037975
#>   3:    308.221 0.02784810
#>   4:    311.810 0.02405063
#>   5:    315.398 0.02531646
#>  ---                      
#> 960:   3187.990 0.05696203
#> 961:   3190.520 0.05696203
#> 962:   3193.060 0.06202532
#> 963:   3195.590 0.06202532
#> 964:   3198.120 0.05189873
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
