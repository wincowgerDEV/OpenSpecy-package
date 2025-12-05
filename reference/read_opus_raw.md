# Read a Bruker OPUS spectrum binary raw string

Read single binary acquired with an Bruker Vertex FTIR Instrument

## Usage

``` r
read_opus_raw(rw, type = "spec", atm_comp_minus4offset = FALSE)
```

## Arguments

- rw:

  a raw vector

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

- atm_comp_minus4offset:

  logical; whether spectra after atmospheric compensation are read with
  an offset of -4 bytes from Bruker OPUS files. Default is `FALSE`.

## Value

A list of 10 elements:

- `metadata`:

  a `data.frame` containing metadata from the OPUS file.

- `spec`:

  if `"spec"` was requested in the `type` option, a matrix of the
  spectrum of the sample (otherwise set to `NULL`).

- `spec_no_atm_comp`:

  if `"spec_no_atm_comp"` was requested in the `type` option, a matrix
  of the spectrum of the sample without atmospheric compensation
  (otherwise set to `NULL`).

- `sc_sample`:

  if `"sc_sample"` was requested in the `type` option, a matrix of the
  single channel spectrum of the sample (otherwise set to `NULL`).

- `sc_ref`:

  if `"sc_ref"` was requested in the `type` option, a matrix of the
  single channel spectrum of the reference (otherwise set to `NULL`).

- `ig_sample`:

  if `"ig_sample"` was requested in the `type` option, a matrix of the
  interferogram of the sample (otherwise set to `NULL`).

- `ig_ref`:

  if `"ig_ref"` was requested in the `type` option, a matrix of the
  interferogram of the reference (otherwise set to `NULL`).

- `wavenumbers`:

  if `"spec"` or `"spec_no_atm_comp"` was requested in the `type`
  option, a numeric vector of the wavenumbers of the spectrum of the
  sample (otherwise set to `NULL`).

- `wavenumbers_sc_sample`:

  if `"sc_sample"` was requested in the `type` option, a numeric vector
  of the wavenumbers of the single channel spectrum of the sample
  (otherwise set to `NULL`).

- `wavenumbers_sc_ref`:

  if `"sc_ref"` was requested in the `type` option, a numeric vector of
  the wavenumbers of the single channel spectrum of the reference
  (otherwise set to `NULL`).

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

[`read_opus()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_opus.md)

## Author

Philipp Baumann and Pierre Roudier
