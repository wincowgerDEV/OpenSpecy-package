# Sample Raman spectrum

Raman spectrum of high-density polyethylene (HDPE) provided by Horiba
Scientific.

## Format

An threepart list of class
[`OpenSpecy`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
containing:

|               |                                                                                                                                                                                |
|---------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `wavenumber`: | spectral wavenumbers \[1/cm\] (vector of 964 rows)                                                                                                                             |
| `spectra`:    | absorbance values [-](https://rdrr.io/r/base/Arithmetic.html) (a [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html) with 964 rows and 1 column) |
| `metadata`:   | spectral metadata                                                                                                                                                              |

## References

Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD,
Hemabessiere L, Lee E, Mill L, et al. (2020). “Critical Review of
Processing and Classification Techniques for Images and Spectra in
Microplastic Research.” *Applied Spectroscopy*, **74**(9), 989–1010.
[doi:10.1177/0003702820929064](https://doi.org/10.1177/0003702820929064)
.

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
data(raman_hdpe)
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
```
