# Read spectral data from Bruker OPUS binary files

Read file(s) acquired with a Bruker Vertex FTIR Instrument. This
function is basically a fork of `opus_read()` from
<https://github.com/pierreroudier/opusreader>.

## Usage

``` r
read_opus(
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
  type = "spec",
  digits = 1L,
  atm_comp_minus4offset = FALSE
)
```

## Arguments

- file:

  character vector with path to file(s).

- metadata:

  a named list of the metadata; see
  [`as_OpenSpecy()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
  for details.

- type:

  character vector of spectra types to extract from OPUS binary file.
  Default is `"spec"`, which will extract the final spectra, e.g.
  expressed in absorbance (named `AB` in Bruker OPUS programs). Possible
  additional values for the character vector supplied to `type` are
  `"spec_no_atm_comp"` (spectrum of the sample without compensation for
  atmospheric gases, water vapor and/or carbon dioxide), `"sc_sample"`
  (single channel spectrum of the sample measurement), `"sc_ref"`
  (single channel spectrum of the reference measurement), `"ig_sample"`
  (interferogram of the sample measurement) and `"ig_ref"`
  (interferogram of the reference measurement).

- digits:

  Integer that specifies the number of decimal places used to round the
  wavenumbers (values of x-variables).

- atm_comp_minus4offset:

  Logical whether spectra after atmospheric compensation are read with
  an offset of -4 bytes from Bruker OPUS files; default is `FALSE`.

## Value

An `OpenSpecy` object.

## Details

The type of spectra returned by the function when using `type = "spec"`
depends on the setting of the Bruker instrument: typically, it can be
either absorbance or reflectance.

The type of spectra to extract from the file can also use Bruker's OPUS
software naming conventions, as follows:

- `ScSm` corresponds to `sc_sample`

- `ScRf` corresponds to `sc_ref`

- `IgSm` corresponds to `ig_sample`

- `IgRf` corresponds to `ig_ref`

## See also

[`read_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/io_spec.md)
for reading .y(a)ml, .json, or .rds (OpenSpecy) files;
[`read_text()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
[`read_asp()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
[`read_spa()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
[`read_spc()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md),
and
[`read_jdx()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md)
for text files, .asp, .spa, .spa, .spc, and .jdx formats, respectively;
[`read_text()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md)
for reading .dat (ENVI) files;
[`read_zip()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
and
[`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
for wrapper functions;
[`read_opus_raw()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_opus_raw.md);

## Author

Philipp Baumann, Zacharias Steinmetz, Win Cowger

## Examples

``` r
read_extdata("ftir_ps.0") |> read_opus()
#>       wavenumber         V1
#>            <num>      <num>
#>    1:   399.2239 0.01660202
#>    2:   401.1525 0.01790014
#>    3:   403.0811 0.01831616
#>    4:   405.0097 0.01894823
#>    5:   406.9384 0.02070733
#>   ---                      
#> 2122:  4489.8223 0.02349887
#> 2123:  4491.7509 0.02352615
#> 2124:  4493.6796 0.02357077
#> 2125:  4495.6082 0.02362563
#> 2126:  4497.5368 0.02361715
#> 
#> $metadata
#>        x     y file_name  license                         unique_id
#>    <int> <int>    <char>   <char>                            <char>
#> 1:     1     1 ftir_ps.0 CC BY-NC sample1_PSnew_2020-10-28 14:33:02
#>        sample_id        date_time_sm        date_time_rf   sample_name
#>           <char>              <POSc>              <POSc>        <char>
#> 1: sample1_PSnew 2020-10-28 14:33:02 2020-10-28 14:23:20 sample1_PSnew
#>    instr_name_range resolution_wn result_spc beamspl laser_wn
#>              <char>         <int>     <char>  <char>    <num>
#> 1:    VERTEX 70-MIR             4         AB     KBr 15799.24
#>                               spc_in_file zero_filling temp_scanner_sm
#>                                    <char>        <int>           <num>
#> 1: ig_sample;ig_ref;sc_sample;sc_ref;spec            2            27.9
#>    temp_scanner_rf hum_rel_sm hum_rel_rf hum_abs_sm hum_abs_rf col_id
#>              <num>      <int>     <lgcl>     <lgcl>     <lgcl> <char>
#> 1:            27.9          2         NA         NA         NA     V1
#>                                                           session_id
#>                                                               <char>
#> 1: d862a750602b537df0489a1dd2b06b60/c4a50edae27608b4ccf1a690dc7df0b1
#>                             file_id
#>                              <char>
#> 1: 0e9318d04fdb3beb4cd61145b080ccb6
```
