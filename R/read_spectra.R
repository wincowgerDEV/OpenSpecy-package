#' @rdname read_spectra
#'
#' @title Read spectral data
#'
#' @description
#' Read Raman or FTIR spectrum files as asp, jdx, spc, or spa. A csv file is
#' preferred. If a csv, the file must contain one column labeled Wavelength in
#' units of (1/cm) and another column labeled Absorbance in absorbance units.
#' If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with
#' wavelength in (1/cm). These files will not always work perfectly because they
#' are tricky to read so double check them in another software.
#'
#' @param file file.
#' @param \ldots ...
#'
#' @seealso
#' seealso
#'
#' @examples
#' c()
#'
#' @importFrom magrittr %>%
#' @export
read_asp <- function(file = ".", ...) {
  if (!grepl("\\.asp$", file)) stop("file type should be 'asp'")

  tr <- file.path(file) %>% file(...)
  lns <- tr %>% readLines() %>% as.numeric()
  close(tr)

  y <- lns[-c(1:6)]
  x <- seq(lns[2], lns[3], length.out = lns[1])

  data.frame(wavenumber = x, absorbance = y)
}

#' @rdname read_spectra
#'
#' @importFrom utils read.table
#' @export
read_spa <- function(file = ".", ...) {
  if (!grepl("\\.spa$", file)) stop("file type should be 'spa'")

  trr <- file.path(file) %>% file(...)
  lns <- trr %>% readLines(n = 10, warn = FALSE)
  close(trr)

  mgn <- lns[grepl("Resolution", lns, useBytes = T)]

  start <- strsplit(mgn, " ")[[1]][5] %>% as.numeric()
  end <- strsplit(mgn, " ")[[1]][7] %>% as.numeric()

  trb <- file.path(file) %>% file(open = "rb", ...)

  # Read the start offset
  seek(trb, 386, origin = "start")
  startOffset <- readBin(trb, "int", n = 1, size = 2)
  # Read the length
  seek(trb, 390, origin = "start")
  readLength <- readBin(trb, "int", n = 1, size = 2)

  # seek to the start
  seek(trb, startOffset, origin = "start")

  # we'll read four byte chunks
  floatCount <- readLength/4

  # read all our floats
  floatData <- c(readBin(trb, "double", floatCount, size = 4))

  close(trb)

  data.frame(wavenumber = seq(end, start, length = length(floatData)),
             absorbance = floatData)
}

#' @rdname read_spectra
#'
#' @importFrom hyperSpec read.jdx
#' @export
read_jdx <- function(file = ".", ...) {
  jdx <- read.jdx(file, ...)

  data.frame(wavenumber = jdx@wavelength,
             absorbance = as.numeric(unname(jdx@data$spc[1,])))
}

#' @rdname read_spectra
#'
#' @importFrom hyperSpec read.spc
#' @export
read_spc <- function(file = ".", ...) {
  spc <- read.spc(file)

  data.frame(wavenumber = spc@wavelength,
             absorbance = as.numeric(unname(spc@data$spc[1,])))
}

#' @rdname read_spectra
#'
#' @importFrom hexView readRaw blockString
#' @export
read_0 <- function(file = ".", ...) {
  if (!grepl("\\.[0-999]$", file)) stop("file type should be '0'")

  pa <- readRaw(file, offset = 0, nbytes = file.info(file)$size, human = "char",
                size = 1, endian = "little")
  pr <- pa$fileRaw

  # Get positions where the following parameters are found in the file
  codes <- c("ZFF", "RES", "SNM", "DAT", "LWN", "FXV", "LXV", "NPT", "MXY",
             "MNY", "END", "TIM")

  z <- grepRaw(codes[1],pr,all=TRUE)[1]+5
  re <- grepRaw(codes[2],pr,all=TRUE)[1]+5
  snm <- grepRaw(codes[3],pr,all=TRUE)[1]+7
  dat <- grepRaw(codes[4],pr,all=TRUE)[1]+7
  lwn <- grepRaw(codes[5],pr,all=TRUE)[1]+7
  fx <- grepRaw(codes[6],pr,all=TRUE)[3]+7
  lx <- grepRaw(codes[7],pr,all=TRUE)[3]+7
  npt0 <- grepRaw(codes[8],pr,all=TRUE)[2]+3
  npt1 <- grepRaw(codes[8],pr,all=TRUE)[3]+7
  mxy <- grepRaw(codes[9],pr,all=TRUE)[1]+7
  mny <- grepRaw(codes[10],pr,all=TRUE)[3]+7
  end <- grepRaw(codes[11],pr,all=TRUE)+11
  tim <- grepRaw(codes[12],pr,all=TRUE)+11

  ## calculate end and start of each block:
  offs <- sapply(5:10, function(x){end[x]})
  byts <- diff(offs)
  ZFF <- readRaw(file, offset=z, nbytes=4, human="int", size=2)[[5]][1]
  RES <- readRaw(file, offset=re, nbytes=4, human="int", size=2)[[5]][1]
  snm.lab.material <- blockString(readRaw(file, offset = snm, nbytes = 22, human = "char", size = 1, endian = "little"))

  ## Get number of data points for each spectra data block
  NPT0 <- readRaw(file, offset=npt0, nbytes=12, human="int", size=4)[[5]][2]
  NPT1 <- readRaw(file, offset=npt1, nbytes=4, human="int", size=4)[[5]][1]
  fxv <- readRaw(file, offset=fx, nbytes=16, human="real", size=8)[[5]][1] ## fxv:	Frequency of first point
  lxv <- readRaw(file, offset=lx, nbytes=16, human="real", size=8)[[5]][1] ## lxv:	Frequency of last point
  x <- rev(seq(lxv, fxv, (fxv-lxv)/(NPT1-1)))

  ## Read all through all the data blocks inside the OPUS file:
  nbytes1 <- NPT0*4 ## initial parameters
  smxa <- c()
  smna <- c()
  nbytes.f <- NPT1*4
  if(offs[1]<2000){
    offs.f<-offs[3]
  }

  if(offs[1]>20000){
    offs.f<-offs[2]
  }

  # Selected spectra block
  opus.p <- readRaw(file, width = NULL, offset = offs.f - 4,
                    nbytes = nbytes.f, human = "real", size = 4, endian="little")
  y <- opus.p[[5]]

  data.frame(wavenumber = x, absorbance = y)
}
