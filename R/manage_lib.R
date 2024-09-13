#' @rdname manage_lib
#' @title Manage spectral libraries
#'
#' @description
#' These functions will import the spectral libraries from Open Specy if they
#' were not already downloaded. The CRAN does not allow for deployment of large
#' datasets so this was a workaround that we are using to make sure everyone can
#' easily get Open Specy functionality running on their desktop. Please see the
#' references when using these libraries. These libraries are the accumulation
#' of a massive amount of effort from independant groups and each should be
#' attributed when you are using their data.
#'
#' @details
#' \code{check_lib()} checks to see if the Open Specy reference library
#' already exists on the users computer.
#' \code{get_lib()} downloads the Open Specy library from OSF
#' (\doi{10.17605/OSF.IO/X7DPZ}).
#' \code{load_lib()} will load the library into the global environment for use
#' with the Open Specy functions.
#' \code{rm_lib()} removes the libraries from your computer.
#'
#' @param type library type to check/retrieve; defaults to
#' \code{c("derivative", "nobaseline", "raw", "mediod", "model")} which reads
#' everything.
#' @param node the OSF node to be retrieved; should be \code{"x7dpz"} unless you
#' maintain your own OSF node with spectral libraries.
#' @param path where to save or look for local library files; defaults to
#' \code{"system"} pointing to
#' \code{system.file("extdata", package = "OpenSpecy")}.
#' @param conflicts determines what happens when a file with the same name
#' exists at the specified destination. Can be one of the following (see
#' \code{\link[osfr]{osf_download}()} for details):
#' \describe{
#'   \item{"error"}{throw an error and abort the file transfer operation.}
#'   \item{"skip"}{skip the conflicting file(s) and continue transferring the
#'   remaining files.}
#'   \item{"overwrite" (default)}{ replace the existing file with the
#'   transferred copy.}
#' }
#' @param condition determines if \code{check_lib()} should warn
#' (\code{"warning"}, the default) or throw and error (\code{"error"}).
#' @param \ldots further arguments passed to \code{\link[osfr]{osf_download}()}.
#'
#' @return
#' \code{check_lib()} and \code{get_lib()} return messages only;
#' \code{load_lib()} returns an \code{OpenSpecy} object containing the
#' respective spectral reference library.
#'
#' @examples
#' \dontrun{
#' check_lib("derivative")
#' get_lib("derivative")
#'
#' spec_lib <- load_lib("derivative")
#' }
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @references
#' Bell IB, Clark RJH, Gibbs PJ (2010). “Raman Spectroscopic Library.”
#' *Christopher Ingold Laboratories, University College London, UK*.
#' \url{https://www.chem.ucl.ac.uk/resources/raman/}.
#'
#' Berzinš K, Sales RE, Barnsley JE, Walker G, Fraser-Miller SJ, Gordon KC
#' (2020). “Low-Wavenumber Raman Spectral Database of Pharmaceutical
#' Excipients.” *Vibrational Spectroscopy* **107**, 103021.
#' \doi{10.5281/zenodo.3614035}.
#'
#' Cabernard L, Roscher L, Lorenz C, Gerdts G, Primpke S (2018). “Comparison of
#' Raman and Fourier Transform Infrared Spectroscopy for the Quantification of
#' Microplastics in the Aquatic Environment.” *Environmental Science &
#' Technology* **52**(22), 13279--13288. \doi{10.1021/acs.est.8b03438}.
#'
#' Caggiani MC, Cosentino A, Mangone A (2016). “Pigments Checker version 3.0, a
#' handy set for conservation scientists: A free online Raman spectra database.”
#' *Microchemical Journal* **129**, 123--132.
#' \doi{10.1016/j.microc.2016.06.020}.
#'
#' Chabuka BK, Kalivas JH (2020). “Application of a Hybrid Fusion Classification
#' Process for Identification of Microplastics Based on Fourier Transform
#' Infrared Spectroscopy. Applied Spectroscopy.” *Applied Spectroscopy*
#' **74**(9), 1167--1183. \doi{10.1177/0003702820923993}.
#'
#' Cowger, W (2023). “Library data.” \emph{OSF}. \doi{10.17605/OSF.IO/X7DPZ}.
#'
#' Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD, Hemabessiere L,
#' Lee E, Mill L, et al. (2020). “Critical Review of Processing and
#' Classification Techniques for Images and Spectra in Microplastic Research.”
#' \emph{Applied Spectroscopy}, \strong{74}(9), 989--1010.
#' \doi{10.1177/0003702820929064}.
#'
#' Cowger W, Roscher L, Chamas A, Maurer B, Gehrke L, Jebens H, Gerdts G,
#' Primpke S (2023). “High Throughput FTIR Analysis of Macro and Microplastics
#' with Plate Readers.” *ChemRxiv Preprint*. \doi{10.26434/chemrxiv-2023-x88ss}.
#'
#' De Frond H, Rubinovitz R, Rochman CM (2021). “µATR-FTIR Spectral Libraries of
#' Plastic Particles (FLOPP and FLOPP-e) for the Analysis of Microplastics.”
#' *Analytical Chemistry* **93**(48), 15878--15885.
#' \doi{10.1021/acs.analchem.1c02549}.
#'
#' El Mendili Y, Vaitkus A, Merkys A, Gražulis S, Chateigner D, Mathevet F,
#' Gascoin S, Petit S, Bardeau JF, Zanatta M, Secchi M, Mariotto G, Kumar A,
#' Cassetta M, Lutterotti L, Borovin E, Orberger B, Simon P, Hehlen B,
#' Le Guen M (2019). “Raman Open Database: first interconnected Raman–X-ray
#' diffraction open-access resource for material identification.” *Journal of
#' Applied Crystallography*, **52**(3), 618--625.
#' \doi{10.1107/s1600576719004229}.
#'
#' Johnson TJ, Blake TA, Brauer CS, Su YF, Bernacki BE, Myers TL, Tonkyn RG,
#' Kunkel BM, Ertel AB (2015). “Reflectance Spectroscopy for Sample
#' Identification: Considerations for Quantitative Library
#' Results at Infrared Wavelengths.” *International Conference on Advanced
#' Vibrational Spectroscopy (ICAVS 8)*.
#' \url{https://www.osti.gov/biblio/1452877}.
#'
#' Lafuente R, Downs RT, Yang H, Stone N (2016). “The power of databases: The
#' RRUFF project.” *Highlights in Mineralogical Crystallography*.
#' \doi{10.1515/9783110417104-003}.
#'
#' Munno K, De Frond H, O’Donnell B, Rochman CM (2020). “Increasing the
#' Accessibility for Characterizing Microplastics: Introducing New
#' Application-Based and Spectral Libraries of Plastic Particles (SLoPP and
#' SLoPP-E).” *Analytical Chemistry* **92**(3), 2443--2451.
#' \doi{10.1021/acs.analchem.9b03626}.
#'
#' Myers TL, Brauer CS, Su YF, Blake TA, Johnson TJ, Richardson RL (2014).
#' “The influence of particle size on infrared reflectance spectra.”
#' *Proceedings Volume 9088, Algorithms and Technologies for Multispectral,
#' Hyperspectral, and Ultraspectral Imagery XX*, 908809.
#' \doi{10.1117/12.2053350}.
#'
#' Myers TL, Brauer CS, Su YF, Blake TA, Tonkyn RG, Ertel AB, Johnson TJ,
#' Richardson RL (2015). “Quantitative reflectance spectra of solid powders
#' as a function of particle size.” *Applied Optics* **54**(15), 4863--4875.
#' \doi{10.1364/ao.54.004863}.
#'
#' Primpke S, Wirth M, Lorenz C, Gerdts G (2018). “Reference database design
#' for the automated analysis of microplastic samples based on Fourier
#' transform infrared (FTIR) spectroscopy.”
#' *Analytical and Bioanalytical Chemistry* **410**, 5131–-5141.
#' \doi{10.1007/s00216-018-1156-x}.
#'
#' Roscher L, Fehres A, Reisel L, Halbach M, Scholz-Böttcher B, Gerriets M,
#' Badewien TH, Shiravani G, Wurpts A, Primpke S, Gerdts G (2021). “Abundances
#' of large microplastics (L-MP, 500-5000 µm) in surface waters of the Weser
#' estuary and the German North Sea.” *PANGAEA*. \doi{10.1594/PANGAEA.938143}.
#'
#' “Handbook of Raman Spectra for geology” (2023).
#' \url{http://www.geologie-lyon.fr/Raman/}.
#'
#' “Scientific Workgroup for the Analysis of Seized Drugs.” (2023).
#' https://swgdrug.org/ir.htm.
#'
#' **Further contribution of spectra:**
#' Suja Sukumaran (Thermo Fisher Scientific),
#' Aline Carvalho,
#' Jennifer Lynch (NIST),
#' Claudia Cella and Dora Mehn (JRC),
#' Horiba Scientific,
#' USDA Soil Characterization Data
#' (\url{https://ncsslabdatamart.sc.egov.usda.gov}),
#' Archaeometrielabor,
#' and S.B. Engelsen (Royal Vet. and Agricultural University, Denmark).
#' Kimmel Center data was collected and provided by Prof. Steven Weiner
#' (Kimmel Center for Archaeological Science, Weizmann Institute of Science,
#' Israel).
#'
#' @export
check_lib <- function(type = c("derivative", "nobaseline", "raw", "mediod",
                               "model"),
                      path = "system", condition = "warning") {

  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  .chkf(type, path = lp, condition = condition)

  invisible()
}

#' @rdname manage_lib
#'
#' @export
load_lib <- function(type, path = "system") {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  chk <- .chkf(type, path = lp, condition = "stop")

  fp <- file.path(lp, paste0(type, ".rds"))
  rds <- readRDS(fp)

  return(rds)
}

#' @rdname manage_lib
#'
#' @export
rm_lib <- function(type = c("derivative", "nobaseline", "raw", "mediod",
                            "model"),
                   path = "system") {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  fp <- file.path(lp, paste0(type, ".rds"))
  file.remove(fp)

  invisible()
}

# Auxiliary function for library checks
.chkf <- function(type, path = "system", condition = "warning") {
  fn <- paste0(type, ".rds")

  lp <- ifelse(path == "system", system.file("extdata", package = "OpenSpecy"),
               path)

  chk <- file.path(lp, fn) |> file.exists()

  names(chk) <- type

  out <- paste(type[!chk], collapse = ", ")

  if (!all(chk))
    do.call(condition, list("Library missing or incomplete: ", out, "; ",
                            "use 'get_lib()' to download a current version",
                            call. =  ifelse(condition %in%
                                              c("message",
                                                "packageStartupMessage"),
                                            "", FALSE)))
  chk
}
