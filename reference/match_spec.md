# Identify and filter spectra

`match_spec()` joins two `OpenSpecy` objects and their metadata based on
similarity. `cor_spec()` correlates two `OpenSpecy` objects, typically
one with knowns and one with unknowns. `ident_spec()` retrieves the top
match values from a correlation matrix and formats them with metadata.
`get_metadata()` retrieves metadata from OpenSpecy objects.
`max_cor_named()` formats the top correlation values from a correlation
matrix as a named vector. `filter_spec()` filters an Open Specy object.
`fill_spec()` adds filler values to an `OpenSpecy` object where it
doesn't have intensities. `os_similarity()` EXPERIMENTAL, returns a
single similarity metric between two OpenSpecy objects based on the
method used.

## Usage

``` r
cor_spec(x, ...)

# Default S3 method
cor_spec(x, ...)

# S3 method for class 'OpenSpecy'
cor_spec(
  x,
  library,
  na.rm = T,
  conform = F,
  type = "roll",
  compute = "optimized",
  ...
)

match_spec(x, ...)

# Default S3 method
match_spec(x, ...)

# S3 method for class 'OpenSpecy'
match_spec(
  x,
  library,
  na.rm = T,
  conform = F,
  type = "roll",
  top_n = NULL,
  order = NULL,
  add_library_metadata = NULL,
  add_object_metadata = NULL,
  compute = "optimized",
  fill = NULL,
  ...
)

ident_spec(
  cor_matrix,
  x,
  library,
  top_n = NULL,
  add_library_metadata = NULL,
  add_object_metadata = NULL,
  ...
)

get_metadata(x, ...)

# Default S3 method
get_metadata(x, ...)

# S3 method for class 'OpenSpecy'
get_metadata(x, logic, rm_empty = TRUE, ...)

max_cor_named(cor_matrix, na.rm = T)

filter_spec(x, ...)

# Default S3 method
filter_spec(x, ...)

# S3 method for class 'OpenSpecy'
filter_spec(x, logic, ...)

ai_classify(x, ...)

# Default S3 method
ai_classify(x, ...)

# S3 method for class 'OpenSpecy'
ai_classify(x, library, fill = NULL, ...)

fill_spec(x, ...)

# Default S3 method
fill_spec(x, ...)

# S3 method for class 'OpenSpecy'
fill_spec(x, fill, ...)

os_similarity(x, ...)

# Default S3 method
os_similarity(x, ...)

# S3 method for class 'OpenSpecy'
os_similarity(x, y, method = "hamming", na.rm = T, ...)
```

## Arguments

- x:

  an `OpenSpecy` object, typically with unknowns.

- library:

  an `OpenSpecy` or `glmnet` object representing the reference library
  of spectra or model to use in identification.

- na.rm:

  logical; indicating whether missing values should be removed when
  calculating correlations. Default is `TRUE`.

- conform:

  Whether to conform the spectra to the library wavenumbers or not.

- type:

  the type of conformation to make returned by
  [`conform_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/conform_spec.md)

- compute:

  the compute strategy used for correlation, "optimized" by default will
  use the current most optimized strategy for Pearson correlation,
  "base" will use base R's [`cor()`](https://rdrr.io/r/stats/cor.html)

- top_n:

  integer; specifying the number of top matches to return. If `NULL`
  (default), all matches will be returned.

- order:

  an `OpenSpecy` used for sorting, ideally the unprocessed one; `NULL`
  skips sorting.

- add_library_metadata:

  name of a column in the library metadata to be joined; `NULL` if you
  don't want to join.

- add_object_metadata:

  name of a column in the object metadata to be joined; `NULL` if you
  don't want to join.

- fill:

  an `OpenSpecy` object with a single spectrum to be used to fill
  missing values for alignment with the AI classification.

- cor_matrix:

  a correlation matrix for object and library, can be returned by
  `cor_spec()`

- logic:

  a logical or numeric vector describing which spectra to keep.

- rm_empty:

  logical; whether to remove empty columns in the metadata.

- y:

  an `OpenSpecy` object to perform similarity search against x.

- method:

  the type of similarity metric to return.

- ...:

  additional arguments passed
  [`cor()`](https://rdrr.io/r/stats/cor.html).

## Value

`match_spec()` and `ident_spec()` will return a
[`data.table-class()`](https://rdatatable.gitlab.io/data.table/reference/data.table-class.html)
containing correlations between spectra and the library. The table has
three columns: `object_id`, `library_id`, and `match_val`. Each row
represents a unique pairwise correlation between a spectrum in the
object and a spectrum in the library. If `top_n` is specified, only the
top `top_n` matches for each object spectrum will be returned. If
`add_library_metadata` is `is.character`, the library metadata will be
added to the output. If `add_object_metadata` is `is.character`, the
object metadata will be added to the output. `filter_spec()` returns an
`OpenSpecy` object. `fill_spec()` returns an `OpenSpecy` object.
`cor_spec()` returns a correlation matrix. `get_metadata()` returns a
[`data.table-class()`](https://rdatatable.gitlab.io/data.table/reference/data.table-class.html)
with the metadata for columns which have information. `os_similarity()`
returns a single numeric value representing the type of similarity
metric requested. 'wavenumber' similarity is based on the proportion of
wavenumber values that overlap between the two objects, 'metadata' is
the proportion of metadata column names, 'hamming' is something similar
to the hamming distance where we discretize all spectra in the OpenSpecy
object by wavenumber intensity values and then relate the wavenumber
intensity value distributions by mean difference in min-max normalized
space. 'pca' tests the distance between the OpenSpecy objects in PCA
space using the first 4 component values and calculating the max-range
normalized distance between the mean components. The first two metrics
are pretty straightforward and definitely ready to go, the 'hamming' and
'pca' metrics are pretty experimental but appear to be working under our
current test cases.

## See also

[`adj_intens()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/adj_intens.md)
converts spectra;
[`get_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_lib.md)
retrieves the Open Specy reference library;
[`load_lib()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_lib.md)
loads the Open Specy reference library into an R object of choice

## Author

Win Cowger, Zacharias Steinmetz

## Examples

``` r
data("test_lib")

unknown <- read_extdata("ftir_ldpe_soil.asp") |>
  read_any() |>
  conform_spec(range = test_lib$wavenumber,
               res = spec_res(test_lib)) |>
  process_spec()
cor_spec(unknown, test_lib)
#>                                      intensity
#> 00002e4e3fac430aa1fdfea6e26f85e4  0.5716650527
#> 00061dd49cbb71549cf3d530b894ba92 -0.0524107035
#> 0008a60c1af45a76ffb91b9cbe1a32e4  0.2120038819
#> 000902ae526db452960c5782843081b4 -0.0574671312
#> 00133c1f1531821f815431f72871eae9  0.3424853396
#> 0016677a99407c18717bf5d3eea9a0a2  0.1169383083
#> 00185609a3205a19225e1530ca98084e -0.0059672577
#> 001d3eb19976c785442a3e6fa8094bff -0.0063433127
#> 0026eee747d277edf308194c81179cb0 -0.0563867464
#> 0031bb13faea1e04b52ffbeca009e8ab  0.6675896750
#> 003b2e57d47a4225b8ea041c946cfc0a -0.0204737255
#> 0046ff947759e247f85dc63a2a25e097  0.0286048318
#> 0054792a45a11ccf7158fdf2c8873125  0.0889031245
#> 005c6a81975d747fe493032cac9dfaa2  0.2070905137
#> 0062d71f901713f2ac7a89c72fb186d4  0.2847788255
#> 00904e33ccbaa20fb68c1943d8303d82  0.0103191160
#> 00931a5ccadb549d463293381c974d79 -0.0182594153
#> 00b9f87c7d7c82675cb945fe4da37917 -0.0874221226
#> 00f4b09b9823187caf65b781b8be87b9 -0.0943789974
#> 010bfd4be6c24ced909f6de717c7b04e  0.0756006101
#> 015e8a21344a60a020226cfef344c584  0.0279306259
#> 016961c0d7def12f68239756d23a79a6 -0.0739530912
#> 019b8f3e4839f8fda1441f7d712ff12d -0.0088904495
#> 01b2fea6718855567e2e468a792b457f  0.0519834480
#> 02b8194bd714469666151a7c6b90b36c  0.0687712268
#> 03f1fa06cd6bc468ae5e406ca1d58dbb -0.0560522947
#> 041771facd71667d6bd6c9269d3701be  0.0502373982
#> 0481958e37c72c71f88d80e4dd69ac59  0.0405530434
#> 058a9af32053d14e876703a60a37498f -0.0172377431
#> 05c3cde45098e01cc44da3f72ada913e -0.0586996007
#> 06a2ed935a9b37b653717ff4a0ea62f7  0.0486159272
#> 06f7950194c7a42714cec488e378a820  0.1062184336
#> 087b8dc9fe95b7cab9d037304f0d39c8 -0.0071308393
#> 09b69fcfcf3c695299dd0f88743cf14a  0.0693075642
#> 0b10e6cc01c8e53f2221647a9c828164  0.5492755145
#> 0b8b5f10d7a9b2bd7d702cd4fdfd8a29  0.0198909283
#> 0e815860f0115ac34e2b46ac91e0c096  0.0219596516
#> 101b6ae86864958ddd95445d0ab01fe4  0.4308809802
#> 14ee1d85d0b1ee84fab0773329c45bd2 -0.0552288672
#> 1a31cfc8e25332f4771d03e8a862e217  0.0257980970
#> 23069d13c090ee5235874bd632467f9c  0.0239585112
#> 252b6f9c5fb83ec82246bc041cabd221 -0.0160335235
#> 263479214867d17e135a0d599b1f8354  0.5000294168
#> 2fa630bec93db824509be188494d79ad  0.1018184192
#> 35ce7ae774d57362dacb92cacd53b01c  0.0057562573
#> 3d35fd9a2766afaf4c088b615dffc350 -0.0009768496
#> 4762edfa07e6b28a4b90db83594f2b3d -0.0004834032
#> 4f1d701f4080b4c74acdaa04dea52dd1  0.0682211684
#> 5961a72282786fe43d8704780782f459  0.0629534493
#> 65792c9e80934e4692540a3e0fbfb552  0.0457114187
#> 6e013532253522149a0deb04373b79a2  0.1339615512
#> 7167a50e76f4a34dbb1d354babd674b0  0.0387929426
#> 76b3af06ab610907cea4dbe3aa4ec67e  0.0237972406
#> 889adce6238a677669b6523d74635d03  0.1267896304
#> 9f91f6441608ecca35f69d298e5351bd  0.0018181040
#> a692fd0b8b3b80b640ce4bbea8105570  0.0423592870
#> ec69350b1b7de35fd6b8cf65048e5fc0  0.1076174781

match_spec(unknown, test_lib, add_library_metadata = "sample_name",
           top_n = 1)
#> Key: <library_id>
#>                          library_id object_id match_val     x     y
#>                              <char>    <char>     <num> <int> <int>
#> 1: 0031bb13faea1e04b52ffbeca009e8ab intensity 0.6675897    37     1
#>                         SpectrumID   Organization SpectrumType SpectrumIdentity
#>                             <char>         <char>       <char>           <char>
#> 1: HDPE_Sarah_#18_1s_20ac_10x_25mW J. Lynch, NIST        Raman             HDPE
#>    Polymer.Category LibraryType  OWNER SpectralCollectionMode Preprocessing
#>              <char>      <char> <char>                 <char>        <char>
#> 1:      polyolefins    Polymers   <NA>                   <NA>          <NA>
#>    InstrumentUsed RRUFFID IDEAL CHEMISTRY LOCALITY SOURCE STATUS    URL
#>            <char>  <char>          <char>   <char> <char> <char> <char>
#> 1:           <NA>    <NA>            <NA>     <NA>   <NA>   <NA>   <NA>
#>    MEASURED CHEMISTRY OtherInformation JCAMP-DX YFACTOR YUNITS InstrumentMode
#>                <char>           <char>   <char>  <char> <char>         <char>
#> 1:               <NA>             <NA>     <NA>    <NA>   <NA>           <NA>
#>    DELTAX Sponsor  Color   Cell     ID Comment cell_plate_id
#>    <char>  <char> <char> <char> <char>  <char>        <char>
#> 1:   <NA>    <NA>   <NA>   <NA>   <NA>    <NA>          <NA>
#>    Form..film..foam.pliable..hard.  Brand   Item Location  Notes
#>                             <char> <char> <char>   <char> <char>
#> 1:                            <NA>   <NA>   <NA>     <NA>   <NA>
#>    Longest.dimension  Width  Depth Particle.mass..mg. Tg..oC. Tm..oC. Onset....
#>               <char> <char> <char>             <char>  <char>  <char>    <char>
#> 1:              <NA>   <NA>   <NA>               <NA>    <NA>    <NA>      <NA>
#>    Enthalpy..J.g. Substance   Form Colour Filename SourceDatabase Method...49
#>            <char>    <char> <char> <char>   <char>         <char>      <char>
#> 1:           <NA>      <NA>   <NA>   <NA>     <NA>           <NA>        <NA>
#>      File  QA/QC Natural /Synthetic Plastic/other framework Abbreviation
#>    <char> <char>             <char>        <char>    <char>       <char>
#> 1:   <NA>   <NA>               <NA>          <NA>      <NA>         <NA>
#>    Source ID Method...57 morphology  color final_polymer_assignment Citation
#>       <char>      <char>     <char> <char>                   <char>   <char>
#> 1:      <NA>        <NA>       <NA>   <NA>                     <NA>     <NA>
#>    Plate Itteration SpectralResolution InstrumentAccessories Wavenumber_Range
#>    <int>      <int>             <char>                <char>           <char>
#> 1:    NA         NA               <NA>                  <NA>             <NA>
#>    PIN_ID ORIENTATION ID merged database
#>    <char>      <char>              <int>
#> 1:   <NA>        <NA>                 NA
#>    Database ID WWTP Paper/Kirstie/FLOPPE...70
#>                                         <int>
#> 1:                                         NA
#>    Database ID WWTP Paper/Kirstie/FLOPPE...71 ID (ESM1) Longest_dimension width
#>                                         <int>     <int>             <int> <int>
#> 1:                                         NA        NA                NA    NA
#>    Cluster2018/WWTP PRESSURE TEMPERATURE MaterialForm MaterialProducer
#>               <int>    <int>       <num>       <char>           <char>
#> 1:               NA       NA          NA         <NA>             <NA>
#>    NumberofAccumulations   TIME Ratio against background Truncated
#>                    <int> <char>                   <char>    <char>
#> 1:                    NA   <NA>                     <NA>      <NA>
#>    ElementMultiply or ElementDivide Collection length Apodization
#>                              <char>            <char>      <char>
#> 1:                             <NA>              <NA>        <NA>
#>    Bench serial number Subtraction or Addition Baseline Correction Smooth
#>                 <char>                  <char>              <char> <char>
#> 1:                <NA>                    <NA>                <NA>   <NA>
#>    %Transmittance->Absorbance Sample gain Number of background scans
#>                        <char>       <num>                      <int>
#> 1:                       <NA>          NA                         NA
#>    Background gain Sample spacing Number of scan points Resolution points
#>              <num>          <num>                 <int>             <int>
#> 1:              NA             NA                    NA                NA
#>    Number of FFT points   ZPD Library ID Shortform Grating Hole size (nm)
#>                   <int> <int>               <char>   <int>          <int>
#> 1:                   NA    NA                 <NA>      NA             NA
#>    Slit (um) Filter (%) Delay (s)   DATE MOLFORM $NIST SOURCE $NIST DOC FILE
#>        <int>      <int>     <int> <char>  <char>       <char>         <char>
#> 1:        NA         NA        NA   <NA>    <NA>         <NA>           <NA>
#>    $NIST PSD FILE APERTURE BEAMSPLITTER DETECTOR (DIA. DET. PORT IN SPHERE)
#>            <char>   <char>       <char>                              <char>
#> 1:           <NA>     <NA>         <NA>                                <NA>
#>    SPHERE DIAMETER ACQUISITION MODE SCANNER SPEED PHASE CORRECTION ZEROFILLING
#>             <char>           <char>        <char>           <char>      <char>
#> 1:            <NA>             <NA>          <NA>             <NA>        <NA>
#>    WAVENUMBER ACCURACY LOW PASS FILTER SWITCH GAIN ON $NIST ID COADDED SCANS
#>                 <char>          <char>         <char>   <char>         <int>
#> 1:                <NA>            <NA>           <NA>     <NA>            NA
#>    PHASE RESOLUTION    MW ContactInfo CASNumber MaterialQuality LaserLightUsed
#>               <num> <num>      <char>    <char>          <char>         <char>
#> 1:               NA    NA        <NA>      <NA>            <NA>           <NA>
#>    TotalAcquisitionTime_s DataProcessingProcedure
#>                    <char>                  <char>
#> 1:                   <NA>                    <NA>
#>    LevelofConfidenceinIdentification smoother baseline DENSITY new_label
#>                               <char>    <int>    <int>   <num>    <char>
#> 1:                              <NA>       NA       NA      NA      <NA>
#>                polymer_class plastic_or_not
#>                       <char>         <char>
#> 1: Polyolefins (POLYALKENES)        plastic
#>                                                   url_polymer_class
#>                                                              <char>
#> 1: https://www.polymerdatabase.com/polymer%20index/polyalkenes.html
#>           polymer                                                url_polymer
#>            <char>                                                     <char>
#> 1: POLY(ETHYLENE) https://www.polymerdatabase.com/polymers/polyethylene.html
#>                                                               url_more_info
#>                                                                      <char>
#> 1: https://www.polymerdatabase.com/polymer%20classes/Polyolefin%20type.html
#>    snr_deriv                          file_id
#>        <num>                           <char>
#> 1:  1339.684 77f837c1640910f2879184d61ef1df48
```
