# OpenSpecy: Analyze, Process, Identify, and Share Raman and (FT)IR Spectra

Raman and (FT)IR spectral analysis tool for plastic particles and other
environmental samples (Cowger et al. 2025,
[doi:10.1021/acs.analchem.5c00962](https://doi.org/10.1021/acs.analchem.5c00962)
). With read_any(), Open Specy provides a single function for reading
individual, batch, or map spectral data files like .asp, .csv, .jdx,
.spc, .spa, .0, and .zip. process_spec() simplifies processing spectra,
including smoothing, baseline correction, range restriction and
flattening, intensity conversions, wavenumber alignment, and min-max
normalization. Spectra can be identified in batch using an onboard
reference library using match_spec(). A Shiny app is available via
run_app() or online at <https://www.openanalysis.org/openspecy/>.

## References

Chabuka BK, Kalivas JH (2020). “Application of a Hybrid Fusion
Classification Process for Identification of Microplastics Based on
Fourier Transform Infrared Spectroscopy.” *Applied Spectroscopy*,
**74**(9), 1167–1183.
[doi:10.1177/0003702820923993](https://doi.org/10.1177/0003702820923993)
.

Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD,
Hemabessiere L, Lee E, Mill L, et al. (2020). “Critical Review of
Processing and Classification Techniques for Images and Spectra in
Microplastic Research.” *Applied Spectroscopy*, **74**(9), 989–1010.
[doi:10.1177/0003702820929064](https://doi.org/10.1177/0003702820929064)
.

Cowger, W (2023). “Library data.” *OSF*.
[doi:10.17605/OSF.IO/X7DPZ](https://doi.org/10.17605/OSF.IO/X7DPZ) .

Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De
Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral
Classification Needs an Open Source Community: Open Specy to the
Rescue!” *Analytical Chemistry*, **93**(21), 7543–7548.
[doi:10.1021/acs.analchem.1c00123](https://doi.org/10.1021/acs.analchem.1c00123)
.

Primpke S, Wirth M, Lorenz C, Gerdts G (2018). “Reference Database
Design for the Automated Analysis of Microplastic Samples Based on
Fourier Transform Infrared (FTIR) Spectroscopy.” *Analytical and
Bioanalytical Chemistry*, **410**(21), 5131–5141.
[doi:10.1007/s00216-018-1156-x](https://doi.org/10.1007/s00216-018-1156-x)
.

Savitzky A, Golay MJ (1964). “Smoothing and Differentiation of Data by
Simplified Least Squares Procedures.” *Analytical Chemistry*, **36**(8),
1627–1639.

Zhao J, Lui H, McLean DI, Zeng H (2007). “Automated Autofluorescence
Background Subtraction Algorithm for Biomedical Raman Spectroscopy.”
*Applied Spectroscopy*, **61**(11), 1225–1232.
[doi:10.1366/000370207782597003](https://doi.org/10.1366/000370207782597003)
.

## See also

Useful links:

- <https://github.com/wincowgerDEV/OpenSpecy-package/>

- <https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html>

- Report bugs at
  <https://github.com/wincowgerDEV/OpenSpecy-package/issues/>

## Author

**Maintainer**: Win Cowger <wincowger@gmail.com>
([ORCID](https://orcid.org/0000-0001-9226-3104)) \[data contributor\]

Authors:

- Zacharias Steinmetz <z.steinmetz@rptu.de>
  ([ORCID](https://orcid.org/0000-0001-6675-5033))

- Hazel Vaquero <hvaquero98@gmail.com>
  ([ORCID](https://orcid.org/0009-0001-5468-2049))

- Nick Leong ([ORCID](https://orcid.org/0009-0008-3313-4132))

- Andrea Faltynkova ([ORCID](https://orcid.org/0000-0003-2523-3137))
  \[data contributor\]

- Hannah Sherrod ([ORCID](https://orcid.org/0009-0001-0497-8693))

Other contributors:

- Andrew B Gray ([ORCID](https://orcid.org/0000-0003-2252-7367))
  \[contributor\]

- Hannah Hapich ([ORCID](https://orcid.org/0000-0003-0000-6632))
  \[contributor\]

- Jennifer Lynch ([ORCID](https://orcid.org/0000-0003-3572-8782))
  \[contributor, data contributor\]

- Hannah De Frond ([ORCID](https://orcid.org/0000-0003-1199-0727))
  \[contributor, data contributor\]

- Garth Covernton ([ORCID](https://orcid.org/0000-0003-3814-4918))
  \[contributor, data contributor\]

- Keenan Munno ([ORCID](https://orcid.org/0000-0003-2916-5944))
  \[contributor, data contributor\]

- Chelsea Rochman ([ORCID](https://orcid.org/0000-0002-7624-711X))
  \[contributor, data contributor\]

- Sebastian Primpke ([ORCID](https://orcid.org/0000-0001-7633-8524))
  \[contributor, data contributor\]

- Orestis Herodotou \[contributor\]

- Mary C Norris \[contributor\]

- Christine M Knauss ([ORCID](https://orcid.org/0000-0003-4404-8922))
  \[contributor\]

- Aleksandra Karapetrova
  ([ORCID](https://orcid.org/0000-0002-9856-1644)) \[contributor, data
  contributor, reviewer\]

- Vesna Teofilovic ([ORCID](https://orcid.org/0000-0002-3557-1482))
  \[contributor\]

- Laura A. T. Markley ([ORCID](https://orcid.org/0000-0003-0620-8366))
  \[contributor\]

- Shreyas Patankar \[contributor, data contributor\]

- Rachel Kozloski ([ORCID](https://orcid.org/0000-0003-1211-9351))
  \[contributor, data contributor\]

- Samiksha Singh \[contributor\]

- Katherine Lasdin \[contributor\]

- Cristiane Vidal ([ORCID](https://orcid.org/0000-0001-6363-9475))
  \[contributor\]

- Clare Murphy-Hagan ([ORCID](https://orcid.org/0009-0009-9629-2856))
  \[contributor\]

- Philipp Baumann <info@spectral-cockpit.space>
  ([ORCID](https://orcid.org/0000-0002-3194-8975)) \[contributor\]

- Pierre Roudier \[contributor\]

- National Renewable Energy Laboratory \[funder\]

- Possibility Lab \[funder\]
