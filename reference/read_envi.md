# Read ENVI data

This function allows ENVI data import.

## Usage

``` r
read_envi(
  file,
  header = NULL,
  spectral_smooth = F,
  sigma = c(1, 1, 1),
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
```

## Arguments

- file:

  name of the binary file.

- header:

  name of the ASCII header file. If `NULL`, the name of the header file
  is guessed by looking for a second file with the same basename as
  `file` but with .hdr extension.

- spectral_smooth:

  logical value determines whether spectral smoothing will be performed.

- sigma:

  if `spectral_smooth` then this option applies the 3d standard
  deviations for the `gaussianSmooth` function from the `mmand` package
  to describe how spectral smoothing occurs on each dimension. The first
  two dimensions are x and y, the third is the wavenumbers.

- metadata:

  a named list of the metadata; see
  [`as_OpenSpecy()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
  for details.

- ...:

  further arguments passed to the submethods.

## Value

An `OpenSpecy` object.

## Details

ENVI data usually consists of two files, an ASCII header and a binary
data file. The header contains all information necessary for correctly
reading the binary file via
[`read.ENVI()`](https://rdrr.io/pkg/caTools/man/ENVI.html).

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
[`read_opus()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_opus.md)
for reading .0 (OPUS) files;
[`read_zip()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
and
[`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md)
for wrapper functions;
[`read.ENVI()`](https://rdrr.io/pkg/caTools/man/ENVI.html)
[`gaussianSmooth()`](https://rdrr.io/pkg/mmand/man/gaussianSmooth.html)

## Author

Zacharias Steinmetz, Claudia Beleites
