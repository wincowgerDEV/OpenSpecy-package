# Manage spectral libraries

These functions will import the spectral libraries from Open Specy if
they were not already downloaded. The CRAN does not allow for deployment
of large datasets so this was a workaround that we are using to make
sure everyone can easily get Open Specy functionality running on their
desktop. Please see the references when using these libraries. These
libraries are the accumulation of a massive amount of effort from
independant groups and each should be attributed when you are using
their data.

## Usage

``` r
check_lib(
  type = c("derivative", "nobaseline", "raw", "medoid_derivative", "medoid_nobaseline",
    "model_derivative", "model_nobaseline"),
  path = "system",
  condition = "warning"
)

get_lib(
  type = c("derivative", "nobaseline", "raw", "medoid_derivative", "medoid_nobaseline",
    "model_derivative", "model_nobaseline"),
  path = "system",
  mode = "wb",
  revision = NULL,
  aws = FALSE,
  ...
)

load_lib(type, path = "system")

rm_lib(
  type = c("derivative", "nobaseline", "raw", "medoid_derivative", "medoid_nobaseline",
    "model_derivative", "model_nobaseline"),
  path = "system"
)
```

## Arguments

- type:

  library type to check/retrieve; defaults to
  `c("derivative", "nobaseline", "raw", "medoid_derivative", "medoid_nobaseline", "model_derivative", "model_nobaseline")`
  which reads everything.

- path:

  where to save or look for local library files; defaults to `"system"`
  pointing to `system.file("extdata", package = "OpenSpecy")`.

- condition:

  determines if `check_lib()` should warn (`"warning"`, the default) or
  throw and error (`"error"`).

- mode:

  see [`?download.file`](https://rdrr.io/r/utils/download.file.html) for
  details on mode.

- revision:

  revision number to use for libraries, revision numbers can be found at
  the osf repo (https://osf.io/x7dpz/) by clicking the library then
  history, if NULL defaults to most recent. This allows exact version
  control.

- aws:

  whether to source the files from AWS or OSF, default of FALSE is OSF.

- ...:

  further arguments passed to `osf_download()`.

## Value

`check_lib()` and `get_lib()` return messages only; `load_lib()` returns
an `OpenSpecy` object containing the respective spectral reference
library.

## Details

`check_lib()` checks to see if the Open Specy reference library already
exists on the users computer. `get_lib()` downloads the Open Specy
library from OSF
([doi:10.17605/OSF.IO/X7DPZ](https://doi.org/10.17605/OSF.IO/X7DPZ) ).
`load_lib()` will load the library into the global environment for use
with the Open Specy functions. `rm_lib()` removes the libraries from
your computer.

## References

Bell IB, Clark RJH, Gibbs PJ (2010). “Raman Spectroscopic Library.”
*Christopher Ingold Laboratories, University College London, UK*.
<https://www.chem.ucl.ac.uk/resources/raman/>.

Berzinš K, Sales RE, Barnsley JE, Walker G, Fraser-Miller SJ, Gordon KC
(2020). “Low-Wavenumber Raman Spectral Database of Pharmaceutical
Excipients.” *Vibrational Spectroscopy* **107**, 103021.
[doi:10.5281/zenodo.3614035](https://doi.org/10.5281/zenodo.3614035) .

Cabernard L, Roscher L, Lorenz C, Gerdts G, Primpke S (2018).
“Comparison of Raman and Fourier Transform Infrared Spectroscopy for the
Quantification of Microplastics in the Aquatic Environment.”
*Environmental Science & Technology* **52**(22), 13279–13288.
[doi:10.1021/acs.est.8b03438](https://doi.org/10.1021/acs.est.8b03438) .

Caggiani MC, Cosentino A, Mangone A (2016). “Pigments Checker version
3.0, a handy set for conservation scientists: A free online Raman
spectra database.” *Microchemical Journal* **129**, 123–132.
[doi:10.1016/j.microc.2016.06.020](https://doi.org/10.1016/j.microc.2016.06.020)
.

Chabuka BK, Kalivas JH (2020). “Application of a Hybrid Fusion
Classification Process for Identification of Microplastics Based on
Fourier Transform Infrared Spectroscopy. Applied Spectroscopy.” *Applied
Spectroscopy* **74**(9), 1167–1183.
[doi:10.1177/0003702820923993](https://doi.org/10.1177/0003702820923993)
.

Cowger, W (2023). “Library data.” *OSF*.
[doi:10.17605/OSF.IO/X7DPZ](https://doi.org/10.17605/OSF.IO/X7DPZ) .

Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD,
Hemabessiere L, Lee E, Mill L, et al. (2020). “Critical Review of
Processing and Classification Techniques for Images and Spectra in
Microplastic Research.” *Applied Spectroscopy*, **74**(9), 989–1010.
[doi:10.1177/0003702820929064](https://doi.org/10.1177/0003702820929064)
.

Cowger W, Roscher L, Chamas A, Maurer B, Gehrke L, Jebens H, Gerdts G,
Primpke S (2023). “High Throughput FTIR Analysis of Macro and
Microplastics with Plate Readers.” *ChemRxiv Preprint*.
[doi:10.26434/chemrxiv-2023-x88ss](https://doi.org/10.26434/chemrxiv-2023-x88ss)
.

De Frond H, Rubinovitz R, Rochman CM (2021). “µATR-FTIR Spectral
Libraries of Plastic Particles (FLOPP and FLOPP-e) for the Analysis of
Microplastics.” *Analytical Chemistry* **93**(48), 15878–15885.
[doi:10.1021/acs.analchem.1c02549](https://doi.org/10.1021/acs.analchem.1c02549)
.

El Mendili Y, Vaitkus A, Merkys A, Gražulis S, Chateigner D, Mathevet F,
Gascoin S, Petit S, Bardeau JF, Zanatta M, Secchi M, Mariotto G, Kumar
A, Cassetta M, Lutterotti L, Borovin E, Orberger B, Simon P, Hehlen B,
Le Guen M (2019). “Raman Open Database: first interconnected Raman–X-ray
diffraction open-access resource for material identification.” *Journal
of Applied Crystallography*, **52**(3), 618–625.
[doi:10.1107/s1600576719004229](https://doi.org/10.1107/s1600576719004229)
.

Johnson TJ, Blake TA, Brauer CS, Su YF, Bernacki BE, Myers TL, Tonkyn
RG, Kunkel BM, Ertel AB (2015). “Reflectance Spectroscopy for Sample
Identification: Considerations for Quantitative Library Results at
Infrared Wavelengths.” *International Conference on Advanced Vibrational
Spectroscopy (ICAVS 8)*. <https://www.osti.gov/biblio/1452877>.

Lafuente R, Downs RT, Yang H, Stone N (2016). “The power of databases:
The RRUFF project.” *Highlights in Mineralogical Crystallography*.
[doi:10.1515/9783110417104-003](https://doi.org/10.1515/9783110417104-003)
.

Munno K, De Frond H, O’Donnell B, Rochman CM (2020). “Increasing the
Accessibility for Characterizing Microplastics: Introducing New
Application-Based and Spectral Libraries of Plastic Particles (SLoPP and
SLoPP-E).” *Analytical Chemistry* **92**(3), 2443–2451.
[doi:10.1021/acs.analchem.9b03626](https://doi.org/10.1021/acs.analchem.9b03626)
.

Myers TL, Brauer CS, Su YF, Blake TA, Johnson TJ, Richardson RL (2014).
“The influence of particle size on infrared reflectance spectra.”
*Proceedings Volume 9088, Algorithms and Technologies for Multispectral,
Hyperspectral, and Ultraspectral Imagery XX*, 908809.
[doi:10.1117/12.2053350](https://doi.org/10.1117/12.2053350) .

Myers TL, Brauer CS, Su YF, Blake TA, Tonkyn RG, Ertel AB, Johnson TJ,
Richardson RL (2015). “Quantitative reflectance spectra of solid powders
as a function of particle size.” *Applied Optics* **54**(15), 4863–4875.
[doi:10.1364/ao.54.004863](https://doi.org/10.1364/ao.54.004863) .

Primpke S, Wirth M, Lorenz C, Gerdts G (2018). “Reference database
design for the automated analysis of microplastic samples based on
Fourier transform infrared (FTIR) spectroscopy.” *Analytical and
Bioanalytical Chemistry* **410**, 5131–-5141.
[doi:10.1007/s00216-018-1156-x](https://doi.org/10.1007/s00216-018-1156-x)
.

Roscher L, Fehres A, Reisel L, Halbach M, Scholz-Böttcher B, Gerriets M,
Badewien TH, Shiravani G, Wurpts A, Primpke S, Gerdts G (2021).
“Abundances of large microplastics (L-MP, 500-5000 µm) in surface waters
of the Weser estuary and the German North Sea.” *PANGAEA*.
[doi:10.1594/PANGAEA.938143](https://doi.org/10.1594/PANGAEA.938143) .

“Handbook of Raman Spectra for geology” (2023).

“Scientific Workgroup for the Analysis of Seized Drugs.” (2023).
https://swgdrug.org/ir.htm.

**Further contribution of spectra:** Suja Sukumaran (Thermo Fisher
Scientific), Aline Carvalho, Jennifer Lynch (NIST), Claudia Cella and
Dora Mehn (JRC), Horiba Scientific, USDA Soil Characterization Data
(<https://ncsslabdatamart.sc.egov.usda.gov>), Archaeometrielabor, and
S.B. Engelsen (Royal Vet. and Agricultural University, Denmark). Kimmel
Center data was collected and provided by Prof. Steven Weiner (Kimmel
Center for Archaeological Science, Weizmann Institute of Science,
Israel).

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
if (FALSE) { # \dontrun{
#check to see if you have the library already
check_lib("derivative")

#get the library stored in your system from online repo
get_lib("derivative")

#load the library into the working environment
spec_lib <- load_lib("derivative")

#for models you should choose either both, ftir, or raman as a list item before use
get_lib("model_derivative")
mod_lib <- load_lib("model_derivative")[["ftir"]]

} # }
```
