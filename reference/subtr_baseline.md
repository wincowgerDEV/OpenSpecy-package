# Automated background subtraction for spectral data

This baseline correction routine iteratively finds the baseline of a
spectrum using polynomial fitting methods or accepts a manual baseline.

## Usage

``` r
subtr_baseline(x, ...)

# Default S3 method
subtr_baseline(
  x,
  y,
  type = "polynomial",
  degree = 8,
  raw = FALSE,
  full = T,
  remove_peaks = T,
  refit_at_end = F,
  crop_boundaries = F,
  iterations = 10,
  peak_width_mult = 3,
  termination_diff = 0.05,
  degree_part = 2,
  bl_x = NULL,
  bl_y = NULL,
  make_rel = TRUE,
  ...
)

# S3 method for class 'OpenSpecy'
subtr_baseline(
  x,
  type = "polynomial",
  degree = 8,
  raw = FALSE,
  full = T,
  remove_peaks = T,
  refit_at_end = F,
  crop_boundaries = F,
  iterations = 10,
  peak_width_mult = 3,
  termination_diff = 0.05,
  degree_part = 2,
  baseline = list(wavenumber = NULL, spectra = NULL),
  make_rel = TRUE,
  ...
)
```

## Arguments

- x:

  a list object of class `OpenSpecy` or a vector of wavenumbers.

- y:

  a vector of spectral intensities.

- type:

  one of `"polynomial"` or `"manual"` depending on the desired baseline
  correction method.

- degree:

  the degree of the full spectrum polynomial. Must be less than the
  number of unique points when `raw` is `FALSE`. Typically, a good fit
  can be found with an 8th order polynomial.

- raw:

  if `TRUE`, use raw and not orthogonal polynomials.

- full:

  logical, whether to use the full spectrum as in `"imodpoly"` or to
  partition as in `"smodpoly"`.

- remove_peaks:

  logical, whether to remove peak regions during first iteration.

- refit_at_end:

  logical, whether to refit a polynomial to the end result (TRUE) or to
  use linear approximation.

- crop_boundaries:

  logical, whether to smartly crop the boundaries to match the spectra
  based on peak proximity.

- iterations:

  the number of iterations for automated baseline correction.

- peak_width_mult:

  scaling factor for the width of peak detection regions.

- termination_diff:

  scaling factor for the ratio of difference in residual standard
  deviation to terminate iterative fitting with.

- degree_part:

  the degree of the polynomial for `"smodpoly"`. Must be less than the
  number of unique points.

- bl_x:

  a vector of wavenumbers for the baseline.

- bl_y:

  a vector of spectral intensities for the baseline.

- make_rel:

  logical; if `TRUE`, spectra are automatically normalized with
  [`make_rel()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/make_rel.md).

- baseline:

  an `OpenSpecy` object containing the baseline data to be subtracted
  (only for `"manual"`).

- ...:

  further arguments passed to
  [`poly()`](https://rdrr.io/r/stats/poly.html) or `"smodpoly"`
  parameters.

## Value

`subtr_baseline()` returns a data frame containing two columns named
`"wavenumber"` and `"intensity"`.

## Details

This function supports two types of `"polynomial"` automated baseline
correction with options. Default settings are closest to `"imodpoly"`
for iterative polynomial fitting based on Zhao et al. (2007).
Additionally options recommended by `"smodpoly"` for segmented iterative
polynomial fitting with enhanced peak detection from the S-Modpoly
algorithm (<https://github.com/jackma123-rgb/S-Modpoly>), and `"manual"`
for applying a user-provided baseline.

## References

Chen MS (2020). Michaelstchen/ModPolyFit. *MATLAB*. Retrieved from
<https://github.com/michaelstchen/modPolyFit> (Original work published
July 28, 2015)

Zhao J, Lui H, McLean DI, Zeng H (2007). “Automated Autofluorescence
Background Subtraction Algorithm for Biomedical Raman Spectroscopy.”
*Applied Spectroscopy*, **61**(11), 1225–1232.
[doi:10.1366/000370207782597003](https://doi.org/10.1366/000370207782597003)
.

Jackma123 (2023). S-Modpoly: Segmented modified polynomial fitting for
spectral baseline correction. *GitHub Repository*. Retrieved from
<https://github.com/jackma123-rgb/S-Modpoly>.

## See also

[`poly()`](https://rdrr.io/r/stats/poly.html);
[`smooth_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/smooth_intens.md)

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
data("raman_hdpe")

# Use polynomial
subtr_baseline(raman_hdpe, type = "polynomial", degree = 8)
#>      wavenumber   intensity
#>           <num>       <num>
#>   1:    301.040 0.000000000
#>   2:    304.632 0.001328324
#>   3:    308.221 0.000000000
#>   4:    311.810 0.000000000
#>   5:    315.398 0.000000000
#>  ---                       
#> 960:   3187.990 0.016928702
#> 961:   3190.520 0.016928702
#> 962:   3193.060 0.022547154
#> 963:   3195.590 0.022547154
#> 964:   3198.120 0.011310249
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

subtr_baseline(raman_hdpe, type = "polynomial", iterations = 5)
#>      wavenumber   intensity
#>           <num>       <num>
#>   1:    301.040 0.000000000
#>   2:    304.632 0.001328324
#>   3:    308.221 0.000000000
#>   4:    311.810 0.000000000
#>   5:    315.398 0.000000000
#>  ---                       
#> 960:   3187.990 0.016928702
#> 961:   3190.520 0.016928702
#> 962:   3193.060 0.022547154
#> 963:   3195.590 0.022547154
#> 964:   3198.120 0.011310249
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

# Use manual
bl <- raman_hdpe
bl$spectra$intensity <- bl$spectra$intensity / 2
subtr_baseline(raman_hdpe, type = "manual", baseline = bl)
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
