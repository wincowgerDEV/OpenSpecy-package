# Read spectral data

Functions for reading spectral data from external file types. Currently
supported reading formats are .csv and other text files, .asp, .spa,
.spc, .xyz, and .jdx. Additionally, .0 (OPUS) and .dat (ENVI) files are
supported via
[`read_opus()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_opus.md)
and
[`read_envi()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_envi.md),
respectively.
[`read_zip()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
takes any of the files listed above. Note that proprietary file formats
like .0, .asp, and .spa are poorly supported but will likely still work
in most cases.

## Usage

``` r
read_text(
  file,
  colnames = NULL,
  method = "fread",
  comma_decimal = TRUE,
  metadata = list(file_name = basename(file), user_name = NULL, contact_info = NULL,
    organization = NULL, citation = NULL, spectrum_type = NULL, spectrum_identity = NULL,
    material_form = NULL, material_phase = NULL, material_producer = NULL,
    material_purity = NULL, material_quality = NULL, material_color = NULL,
    material_other = NULL, cas_number = NULL, instrument_used = NULL,
    instrument_accessories = NULL, instrument_mode = NULL, spectral_resolution = NULL,
    laser_light_used = NULL, number_of_accumulations = NULL, 
    
    total_acquisition_time_s = NULL, data_processing_procedure = NULL,
    level_of_confidence_in_identification = NULL, other_info = NULL, license =
    "CC BY-NC"),
  ...
)

read_asp(
  file,
  metadata = list(file_name = basename(file), user_name = NULL, contact_info = NULL,
    organization = NULL, citation = NULL, spectrum_type = NULL, spectrum_identity = NULL,
    material_form = NULL, material_phase = NULL, material_producer = NULL,
    material_purity = NULL, material_quality = NULL, material_color = NULL,
    material_other = NULL, cas_number = NULL, instrument_used = NULL,
    instrument_accessories = NULL, instrument_mode = NULL, spectral_resolution = NULL,
    laser_light_used = NULL, number_of_accumulations = NULL, 
    
    total_acquisition_time_s = NULL, data_processing_procedure = NULL,
    level_of_confidence_in_identification = NULL, other_info = NULL, license =
    "CC BY-NC"),
  ...
)

read_spa(
  file,
  metadata = list(file_name = basename(file), user_name = NULL, contact_info = NULL,
    organization = NULL, citation = NULL, spectrum_type = NULL, spectrum_identity = NULL,
    material_form = NULL, material_phase = NULL, material_producer = NULL,
    material_purity = NULL, material_quality = NULL, material_color = NULL,
    material_other = NULL, cas_number = NULL, instrument_used = NULL,
    instrument_accessories = NULL, instrument_mode = NULL, spectral_resolution = NULL,
    laser_light_used = NULL, number_of_accumulations = NULL, 
    
    total_acquisition_time_s = NULL, data_processing_procedure = NULL,
    level_of_confidence_in_identification = NULL, other_info = NULL, license =
    "CC BY-NC"),
  ...
)

read_spc(
  file,
  metadata = list(file_name = basename(file), user_name = NULL, contact_info = NULL,
    organization = NULL, citation = NULL, spectrum_type = NULL, spectrum_identity = NULL,
    material_form = NULL, material_phase = NULL, material_producer = NULL,
    material_purity = NULL, material_quality = NULL, material_color = NULL,
    material_other = NULL, cas_number = NULL, instrument_used = NULL,
    instrument_accessories = NULL, instrument_mode = NULL, spectral_resolution = NULL,
    laser_light_used = NULL, number_of_accumulations = NULL, 
    
    total_acquisition_time_s = NULL, data_processing_procedure = NULL,
    level_of_confidence_in_identification = NULL, other_info = NULL, license =
    "CC BY-NC"),
  ...
)

read_jdx(
  file,
  metadata = list(file_name = basename(file), user_name = NULL, contact_info = NULL,
    organization = NULL, citation = NULL, spectrum_type = NULL, spectrum_identity = NULL,
    material_form = NULL, material_phase = NULL, material_producer = NULL,
    material_purity = NULL, material_quality = NULL, material_color = NULL,
    material_other = NULL, cas_number = NULL, instrument_used = NULL,
    instrument_accessories = NULL, instrument_mode = NULL, spectral_resolution = NULL,
    laser_light_used = NULL, number_of_accumulations = NULL, 
    
    total_acquisition_time_s = NULL, data_processing_procedure = NULL,
    level_of_confidence_in_identification = NULL, other_info = NULL, license =
    "CC BY-NC"),
  ...
)

read_extdata(file = NULL)

read_h5(file, collapse = T, ...)
```

## Arguments

- file:

  file to be read from or written to.

- colnames:

  character vector of `length = 2` indicating the column names for the
  wavenumber and intensity; if `NULL` columns are guessed.

- method:

  submethod to be used for reading text files; defaults to
  [`fread()`](https://rdatatable.gitlab.io/data.table/reference/fread.html)
  but [`read.csv()`](https://rdrr.io/r/utils/read.table.html) works as
  well.

- comma_decimal:

  logical(1) whether commas may represent decimals.

- metadata:

  a named list of the metadata; see

- collapse:

  whether or not to use
  [`collapse_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/def_features.md)
  by particle_id.
  [`as_OpenSpecy()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
  for details.

- ...:

  further arguments passed to the submethods.

## Value

All `read_*()` functions return data frames containing two columns named
`"wavenumber"` and `"intensity"`.

## Details

`read_spc()` and `read_jdx()` are wrappers around the functions provided
by the
[hyperSpec](https://r-hyperspec.github.io/hyperSpec/reference/hyperSpec-package.html).
Other functions have been adapted various online sources. Metadata is
harvested if possible. There are many unique iterations of spectral file
formats so there may be bugs in the file conversion. Please contact us
if you identify any.

## See also

[`read_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/io_spec.md)
for reading .y(a)ml, .json, or .rds (OpenSpecy) files;
[`read_opus()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_opus.md)
for reading .0 (OPUS) files;
[`read_envi()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_envi.md)
for reading .dat (ENVI) files;
[`read_zip()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
and
[`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
for wrapper functions;
[`read.jdx()`](https://r-hyperspec.github.io/hyperSpec/reference/read.jdx.html);
[`read.spc()`](https://r-hyperspec.github.io/hyperSpec/reference/read-spc.html)

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
read_extdata("raman_hdpe.csv") |> read_text()
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
read_extdata("raman_atacamit.spc") |> read_spc()
#>      wavenumber intensity
#>           <num>     <num>
#>   1:   117.7800  218.7200
#>   2:   119.4506  236.4000
#>   3:   121.1213  242.0400
#>   4:   122.7919  233.0500
#>   5:   124.4626  219.4200
#>  ---                     
#> 555:  1043.3174   13.0066
#> 556:  1044.9881   12.4402
#> 557:  1046.6587   11.9187
#> 558:  1048.3294   11.3986
#> 559:  1050.0000   10.8148
#> 
#> $metadata
#>        x     y          file_name  license    col_id
#>    <int> <int>             <char>   <char>    <char>
#> 1:     1     1 raman_atacamit.spc CC BY-NC intensity
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/e0ae8a6c03aabd77378f9d0c367d565c
#>                             file_id
#>                              <char>
#> 1: 2693c0b0295ce36c16b10e1e660241f2
read_extdata("ftir_ldpe_soil.asp") |> read_asp()
#>       wavenumber   intensity
#>            <num>       <num>
#>    1:   650.4205 0.027056019
#>    2:   652.2841 0.029133933
#>    3:   654.1478 0.032566057
#>    4:   656.0115 0.032249778
#>    5:   657.8751 0.028957002
#>   ---                       
#> 1794:  3991.9788 0.005803670
#> 1795:  3993.8425 0.005495489
#> 1796:  3995.7062 0.005133253
#> 1797:  3997.5698 0.005595756
#> 1798:  3999.4335 0.005694630
#> 
#> $metadata
#>        x     y          file_name  license    col_id
#>    <int> <int>             <char>   <char>    <char>
#> 1:     1     1 ftir_ldpe_soil.asp CC BY-NC intensity
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/e0ae8a6c03aabd77378f9d0c367d565c
#>                             file_id
#>                              <char>
#> 1: a33700bad0376ddab156980f473bac92
read_extdata("testdata_zipped.zip") |> read_zip()
#> [[1]]
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
#>        x     y                file_name  license    col_id
#>    <int> <int>                   <char>   <char>    <char>
#> 1:     1     1 testdata2 - Copy (2).csv CC BY-NC intensity
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/e0ae8a6c03aabd77378f9d0c367d565c
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b
#> 
#> [[2]]
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
#>        x     y            file_name  license    col_id
#>    <int> <int>               <char>   <char>    <char>
#> 1:     1     1 testdata2 - Copy.csv CC BY-NC intensity
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/e0ae8a6c03aabd77378f9d0c367d565c
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b
#> 
#> [[3]]
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
#>        x     y     file_name  license    col_id
#>    <int> <int>        <char>   <char>    <char>
#> 1:     1     1 testdata2.csv CC BY-NC intensity
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/e0ae8a6c03aabd77378f9d0c367d565c
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b
#> 
```
