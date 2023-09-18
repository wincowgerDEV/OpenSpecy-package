#' @rdname manage_lib
#' @title Manage spectral libraries
#'
#' @description
#' These functions will import the spectral libraries from Open Specy if they
#' were not already downloaded.
#' The CRAN does not allow for deployment of large datasets so this was a
#' workaround that we are using to make sure everyone can easily get Open Specy
#' functionality running on their desktop. Please see the references when using these libraries. 
#' These libraries are the accumulation of a massive amount of effort from independant groups and
#' each should be attributed when you are using their data.
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
#' \itemize{
#'   \item{"error"}{ throw an error and abort the file transfer operation.}
#'   \item{"skip"}{ skip the conflicting file(s) and continue transferring the
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
#' 
#' Ian M. Bell, Robin J.H. Clark and Peter J. 
#' Gibbs Christopher Ingold Laboratories, University College London, UK. 
#' Downloaded from http://www.chem.ucl.ac.uk/resources/raman/
#' 
#' Handbook of Raman Spectra data retrieved from: 
#' http://www.geologie-lyon.fr/Raman/
#' 
#' De Frond H, Rubinovitz R, Rochman CM. μATR-FTIR Spectral Libraries of 
#' Plastic Particles (FLOPP and FLOPP-e) for the Analysis of Microplastics. 
#' Anal Chem. 2021 Dec 7;93(48):15878-15885. doi: 10.1021/acs.analchem.1c02549. 
#' Epub 2021 Nov 23. PMID: 34813292.
#' 
#' S. Primpke, M. Wirth, C. Lorenz, G. Gerdts: "Reference database design 
#' for the automated analysis of microplastic samples based on Fourier 
#' transform infrared (FTIR) spectroscopy", Analytical and Bioanalytical 
#' Chemistry 410, 5131–5141 (2018), retrieved from 
#' https://link.springer.com/article/10.1007/s00216-018-1156-x
#' 
#' Karlis Berzinš, Ruth E. Sales, Jonathan E. Barnsley, Greg Walker, 
#' Sara J.Fraser-Miller, Keith C.Gordon: "Low-Wavenumber Raman 
#' Spectral Database of Pharmaceutical Excipients"; Vibrational 
#' Spectroscopy 107, 103021 (2020)
#' Retrieved from https://zenodo.org/record/3614035
#' 
#' Keenan Munno, Hannah De Frond, Bridget O’Donnell, and Chelsea M. Rochman
#' Analytical Chemistry 2020 92 (3), 2443-2451
#' DOI: 10.1021/acs.analchem.9b03626
#' 
#' Myers, Tanya L.; Brauer, Carolyn S.; Su, Yin-Fong; Blake, Thomas A.; 
#' Johnson, Timothy J.; Richardson, Robert L., 
#' The influence of particle size on infrared reflectance spectra, 
#' Proc. SPIE 9088, Algorithms and Technologies for Multispectral, 
#' Hyperspectral, and Ultraspectral Imagery XX, 908809, June 13, 2014, 
#' https://doi.org/10.1117/12.2053350.
#' 
#' Myers, Tanya L.; Brauer, Carolyn S.; Su, Yin-Fong; Blake, Thomas A.; 
#' Tonkyn, Russell G.; Ertel, Alyssa B.; Johnson, Timothy J.; 
#' Richardson, Robert L., Quantitative reflectance spectra of solid powders 
#' as a function of particle size, Appl. Opt., 2015, 54, 4863-4875, 
#' https://doi.org/10.1364/ao.54.004863.
#' 
#' Johnson, Timothy J.; Blake, Thomas A.; Brauer, Carolyn S.; 
#' Su, Yin-Fong; Bernacki, Bruce E.; Myers, Tanya L.; Tonkyn, Russell G.; 
#' Kunkel, Brenda M.; and Ertel, Alyssa B., Reflectance Spectroscopy 
#' for Sample Identification: Considerations for Quantitative Library 
#' Results at Infrared Wavelengths, International Conference on Advanced 
#' Vibrational Spectroscopy (ICAVS 8), July 12-17, 2015, 
#' https://www.osti.gov/biblio/1452877.
#' 
#' Livia Cabernard, Lisa Roscher, Claudia Lorenz, Gunnar Gerdts, 
#' and Sebastian Primpke. Environmental Science & Technology 2018 52 (22), 
#' 13279-13288, DOI: 10.1021/acs.est.8b03438
#' 
#' Chabuka BK, Kalivas JH. Application of a Hybrid Fusion Classification 
#' Process for Identification of Microplastics Based on Fourier Transform 
#' Infrared Spectroscopy. Applied Spectroscopy. 2020;74(9):1167-1183. 
#' doi:10.1177/0003702820923993
#' 
#' "Scientific Workgroup for the Analysis of Seized Drugs". 
#' Retrieved from https://swgdrug.org/ir.htm
#'  
#' M.C. Caggiani, A. Cosentino, A. Mangone,
#' Pigments Checker version 3.0, a handy set for conservation scientists: 
#' A free online Raman spectra database, Microchemical Journal, Volume 129,
#' 2016, Pages 123-132, ISSN 0026-265X, 
#' https://doi.org/10.1016/j.microc.2016.06.020.
#' 
#' El Mendili, Y., Vaitkus, A., Merkys, A., Gražulis, S., 
#' Chateigner, D., Mathevet, F., Gascoin, S., Petit, S., 
#' Bardeau, J.-F., Zanatta, M., Secchi, M., Mariotto, G., 
#' Kumar, A., Cassetta, M., Lutterotti, L., Borovin, E., 
#' Orberger, B., Simon, P., Hehlen, B., & Le Guen, M. (2019). 
#' Raman Open Database: first interconnected Raman–X-ray diffraction 
#' open-access resource for material identification. Journal of 
#' Applied Crystallography, 52(3), 618-625. doi: 10.1107/s1600576719004229
#' 
#' Dr. Suja Sukumaran from Thermo Fisher Scientific contributed FTIR spectra
#' to Open Specy
#' 
#' Dr. Aline Carvalho contributed FTIR Spectra to Open Specy. 
#' 
#' Dr. Jennifer Lynch from NIST contributed Raman and FTIR spectra to Open Specy.
#' 
#' Dr. Claudia Cella and Dr. Dora Mehn from JRC provided Raman spectra to Open Specy.
#' 
#' Horiba Scientific contributed Raman spectra to Open Specy.
#' 
#' USDA Soil Characterization Data. https://ncsslabdatamart.sc.egov.usda.gov/
#' 
#' Archaeometrielabor was from http://www.archaeometrielabor.com, now defunct.
#' 
#' Carbohydrates data was compiled by S.B.Engelsen from Royal Vet. 
#' and Agricultural University, Denmark
#' 
#' Kimmel Center data was collected and provided by Prof. Steven Weiner 
#' from Kimmel Center for Archaeological Science, 
#' Weizmann Institute of Science, Israel
#' 
#' Roscher, Lisa; Fehres, Annika; Reisel, Lorenz; Halbach, Maurits; 
#' Scholz-Böttcher, Barbara; Gerriets, Michaela; Badewien, Thomas H;
#' Shiravani, Gholamreza; Wurpts, Andreas; Primpke, Sebastian;
#' Gerdts, Gunnar (2021): Abundances of large microplastics 
#' (L-MP, 500-5000 µm) in surface waters of the Weser estuary and the
#' German North Sea (April 2018). 
#' \emph{PANGAEA}, \doi{https://doi.org/10.1594/PANGAEA.938143}
#' 
#' Lafuente R, Downs RT, Yang H, Stone N. 
#' (2016). \emph{Highlights in Mineralogical Crystallography}. 
#' \doi{https://doi.org/10.1515/9783110417104-003}.
#' 
#' Cowger W, Roscher L, Chamas A, Maurer B, Gehrke L, Jebens H, Gerdts G, Primpke S. 
#' (2023). \emph{Preprint}. \doi{https://doi.org/10.26434/chemrxiv-2023-x88ss}.
#' 
#' Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD, Hemabessiere L,
#' Lee E, Mill L, et al. (2020). “Critical Review of Processing and
#' Classification Techniques for Images and Spectra in Microplastic Research.”
#' \emph{Applied Spectroscopy}, \strong{74}(9), 989–1010.
#' \doi{10.1177/0003702820929064}.
#'
#' Cowger, W (2021). “Library data.” \emph{OSF}. \doi{10.17605/OSF.IO/X7DPZ}.
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
#' @importFrom utils read.csv
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#'
#' @export
get_lib <- function(type = c("derivative", "nobaseline", "raw", "mediod",
                             "model"),
                    path = "system", node = "x7dpz", conflicts = "overwrite",
                    ...) {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  osf <- osf_retrieve_node(node) |>
    osf_ls_files(pattern = ".rds", n_max = Inf)

  message("Fetching Open Specy reference libraries from OSF ...")
    osf |> subset(grepl(
      paste0("(", paste(type, collapse = "|"), ").rds"),
      osf$name)) |>
      osf_download(path = lp, conflicts = conflicts, progress = TRUE, ...)

  message("Use 'load_lib()' to load the library")
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
