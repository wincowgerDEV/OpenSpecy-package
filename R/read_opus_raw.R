#' @title Read a Bruker OPUS spectrum binary raw string
#'
#' @description
#' Read single binary acquired with an Bruker Vertex FTIR Instrument
#'
#' @param rw a raw vector
#' @param type character vector of spectra types to extract from OPUS binary
#' file. Default is `"spec"`, which will extract the final spectra, e.g.
#' expressed in absorbance (named `AB` in Bruker OPUS programs). Possible
#' additional values for the character vector supplied to `type` are
#' `"spec_no_atm_comp"` (spectrum of the sample without compensation for
#' atmospheric gases, water vapor and/or carbon dioxide),
#' `"sc_sample"` (single channel spectrum of the sample measurement),
#' `"sc_ref"` (single channel spectrum of the reference measurement),
#' `"ig_sample"` (interferogram of the sample measurement) and `"ig_ref"`
#' (interferogram of the reference measurement).
#' @param atm_comp_minus4offset logical; whether spectra after atmospheric
#' compensation are read with an offset of -4 bytes from Bruker OPUS
#' files. Default is `FALSE`.
#'
#' @details
#' The type of spectra returned by the function when using
#' `type = "spec"` depends on the setting of the Bruker instrument: typically,
#' it can be either absorbance or reflectance.
#'
#' The type of spectra to extract from the file can also use Bruker's OPUS
#' software naming conventions, as follows:
#'
#' - `ScSm` corresponds to `sc_sample`
#' - `ScRf` corresponds to `sc_ref`
#' - `IgSm` corresponds to `ig_sample`
#' - `IgRf` corresponds to `ig_ref`
#'
#' @return
#' A list of 10 elements:
#'
#' \describe{
#'   \item{`metadata`}{a `data.frame` containing metadata from the OPUS file.}
#'   \item{`spec`}{if `"spec"` was requested in the `type` option, a matrix of
#'   the spectrum of the sample (otherwise set to `NULL`).}
#'   \item{`spec_no_atm_comp`}{if `"spec_no_atm_comp"` was requested in the
#'   `type` option, a matrix of the spectrum of the sample without atmospheric
#'   compensation (otherwise set to `NULL`).}
#'   \item{`sc_sample`}{if `"sc_sample"` was requested in the `type` option, a
#'   matrix of the single channel spectrum of the sample (otherwise set to
#'   `NULL`).}
#'   \item{`sc_ref`}{if `"sc_ref"` was requested in the `type` option, a matrix
#'   of the single channel spectrum of the reference (otherwise set to `NULL`).}
#'   \item{`ig_sample`}{if `"ig_sample"` was requested in the `type` option, a
#'   matrix of the interferogram of the sample (otherwise set to `NULL`).}
#'   \item{`ig_ref`}{if `"ig_ref"` was requested in the `type` option, a matrix
#'   of the interferogram of the reference (otherwise set to `NULL`).}
#'   \item{`wavenumbers`}{if `"spec"` or `"spec_no_atm_comp"` was requested in
#'   the `type` option, a numeric vector of the wavenumbers of the spectrum of
#'   the sample (otherwise set to `NULL`).}
#'   \item{`wavenumbers_sc_sample`}{if `"sc_sample"` was requested in the `type`
#'   option, a numeric vector of the wavenumbers of the single channel spectrum
#'   of the sample (otherwise set to `NULL`).}
#'   \item{`wavenumbers_sc_ref`}{if `"sc_ref"` was requested in the `type`
#'   option, a numeric vector of the wavenumbers of the single channel spectrum
#'   of the reference (otherwise set to `NULL`).}
#' }
#'
#' @author Philipp Baumann and Pierre Roudier
#'
#' @seealso
#' \code{\link{read_opus}()}
#'
#' @importFrom stats setNames
#' @export
read_opus_raw <- function(rw, type = "spec", atm_comp_minus4offset = FALSE) {
  # Silently support and convert the default Bruker values
  type <- switch (type,
    "spc" = "spec", # for backwards compatibility
    "spc_nocomp" = "spec_no_atm_comp", # for backwards compatibility
    "ScSm" = "sc_sample",
    "ScRf" = "sc_ref",
    "IgSm" = "ig_sample",
    "IgRf" = "ig_ref",
    type
  )

  # Sanity check on `type`
  if (!all(type %in% c("spec", "spec_no_atm_comp", "sc_sample", "sc_ref",
                       "ig_sample", "ig_ref"))) {
    stop("Invalid value for the `type` option.", call. = F)
  }

  # Avoid `R CMD check` NOTE: no visible binding for global variable ...
  x <- y <- i <- npt <- NULL

  # do not stop if one OPUS has erroneous parsing for any reason
  try({

  # Read byte positions for selected 3 letter strings that flag important
  # spectral information -------------------------------------------------------

  # Get positions of "END" strings
  end <- grepRaw("END", rw, all = TRUE) + 11
  # Get all positions of "NPT" (number of points) string
  npt_all <- grepRaw("NPT", rw, all = TRUE) + 3
  # Get frequency of first (FXV) and last point (LXV) positions
  fxv_all <- grepRaw("FXV", rw, all = TRUE) + 7
  lxv_all <- grepRaw("LXV", rw, all = TRUE) + 7

  # For some files, the number of positions where "FXV" and "LXV" occur
  # are not equal, e.g. for the file in
  # data/soilspec_esal_bin/BF_mo_01_soil_cal.0 ; As a consequence, the
  # fist and last point numbers (e.g. wavenumber or points for interferograms)
  # are not correctly read. This results in an error when trying to calculate
  # the wavenumbers; The below code is a quick and dirty fix to remove
  # FXV values that don't have LXV values and vice versa
  # (difference between "LXV" and "FXV" for a spectral data block
  # should be 16) --------------------------------------------------------------
  if (length(fxv_all) > length(lxv_all)) {

    # We detect the `fxv_all` values to remove by finding those that do NOT have
    # a 16 bit difference with the `lxv_all` values
    idx_extra <- which(!(fxv_all + 16) %in% lxv_all)
    fxv_all <- fxv_all[-idx_extra]
  }

  if (length(lxv_all) > length(fxv_all)) {

    # We detect the `lxv_all` values to remove by finding those that do NOT have
    # a 16 bit difference with the `fxv_all` values
    idx_extra <- which(!(lxv_all - 16) %in% fxv_all)
    lxv_all <- lxv_all[-idx_extra]
  }

  # Reduce size of npt_all -----------------------------------------------------
  # Some files have an extra "NPT" string without FXV, LXV, and spectral block
  if (length(npt_all) != length(fxv_all)) {

    diff_npt_fxv <- lapply(npt_all, function(x) fxv_all - x)

    min_bigger0_smallerequal40 <- lapply(
      diff_npt_fxv,
      function(x) {
        which_min_bigger0 <- x == min(x[x > 0])
        which_smallerequal40 <- x <= 40
        which_min_bigger0 & which_smallerequal40
      }
    )

    which_npt_valid <- vapply(
      min_bigger0_smallerequal40,
      FUN = function(x) any(x == TRUE),
      FUN.VALUE = logical(1)
    )

    npt_all <- npt_all[which_npt_valid]
  }

  # ----------------------------------------------------------------------------

  ## Read basic spectral information ===========================================
  con <- rawConnection(rw)

  # Read all number of points (NPT) at once
  NPT <- vapply(
    npt_all,
    FUN = function(npt) {
      seek(con, npt, origin = "start", rw = "read")
      readBin(con, what = "integer", n = 12, size = 4)[2]
    },
    FUN.VALUE = numeric(1)
  )

  # Specific error for file: <"data/soilspec_eth_bin/CI_tb_05_soil_cal.2">
  # "Invalid number of bytes" when trying to read spectra
  # -> Reason: NPT at position 1 is 995236000 !!!
  # Omit this entry in NPT and corresponding byte position in npt_all
  # Quick fix ------------------------------------------------------------------
  npt_all <- npt_all[NPT < 40000]
  NPT <- NPT[NPT < 40000]

  # ----------------------------------------------------------------------------

  # Figure out how many spectral blocks exist and select final spectra
  # positions; end_spc is vector of offsets where spectra start
  if (length(end) == 1) {
    end_spc <- end
  } else {
    end_spc <- end[diff(end) > 4 * min(NPT)]
  }

  ## Find final spectra information block positions
  ## that belong to spectra data ===============================================

  # Save positions that contain possible spectra data block
  # standard parameters
  spc_param_list <- list(
    'npt' = npt_all,
    'fxv' = fxv_all,
    'lxv' = lxv_all
  )

  ## Return list of final parameters corresponding to data blocks that contain
  ## spectra, elements are npt (number of points),
  ## fxv (frequency of first point) and lxv (frequency of last point);
  ## returned values represent byte positions in the file where spectra
  ## parameters are stored. ----------------------------------------------------
  return_spc_param <- function(end_spc, spc_param_list) {

    # Difference between any NPT position vector elements end_spc element
    # (end_spc[i] is a scalar, constant value at iteration i)
    diff_l <- lapply(end_spc, function(x) npt_all - x)
    # Test of any vector in list contains -164 (returns list of vectors
    # TRUE or FALSE)
    isminus164 <- lapply(diff_l, function(x) x == -164)

    # Find minimum positive difference within each list
    if (length(diff_l) == 1) {sel_min <- list(TRUE)} else {
      sel_min <- lapply(diff_l,
                        function(x) {
                          if (any(x > 0)) {
                            x == min(x[x > 0])
                          } else {x == -164}
                        })
    }

    # Set FALSE repeated vector in sel_min element where TRUE positions are
    # duplicated
    which_elem_dupl <- which(duplicated(vapply(sel_min, FUN = which,
                                               FUN.VALUE = numeric(1))))

    if (length(which_elem_dupl) > 1) {
      sel_min[which_elem_dupl] <- NULL
      # Reduce end_spc with duplicated elements
      end_spc <- end_spc[- which_elem_dupl]
    }

    # Select minimum difference NPT position for each END position
    npt_min <- Map(
      function(x, y) x[y],
      rep(list(npt_all), length(end_spc)),
      sel_min
    )

    npt_min <- Filter(length, npt_min)

    # Select spectra parameters that immediately follow END positions before
    # corresponding spectra
    param_min <- setNames(
      lapply(
        seq_along(spc_param_list),
        function(i) {
          Map(function(x, y) x[y],
              rep(list(spc_param_list[[i]]), length(end_spc)),
                  sel_min)
        }
      ),
      names(spc_param_list)
    )

    # Test if any difference in list is -164
    if (any(unlist(isminus164) == TRUE)) {
      # Find all list element that contain TRUE in logical vector
      minus164 <- lapply(isminus164, function(x) Find(isTRUE, x))
      # Return element position of last TRUE in list
      where <- function(f, x) {
        vapply(x, f, logical(1))
      }
      last_minus164 <- Position(isTRUE, where(isTRUE, minus164),
                                right = TRUE)
      # Replace positions in parameter list are at positions of last
      # -164 difference between end_spc element and NPT position
      param_min <- setNames(
        lapply(
          seq_along(spc_param_list),
          function(i) {
            param_min[[i]][[last_minus164]] <-
              spc_param_list[[i]][isminus164[[last_minus164]]]
              param_min[[i]]
          }
        ),
        names(spc_param_list)
      )
    }
    # Return list of final parameters corresponding to data blocks that
    # contain spectra
    param_spc <- lapply(param_min, unlist)
    param_spc$end_spc <- end_spc
    param_spc
  }

  # Save spectra parameter list
  param_spc <- return_spc_param(end_spc, spc_param_list)

  # Create individual vectors containing spectra parameters
  npt_spc <- param_spc[["npt"]]
  fxv_spc <- param_spc[["fxv"]]
  lxv_spc <- param_spc[["lxv"]]
  end_spc <- param_spc[["end_spc"]]

  # Read number of points corresponding to spectra in file ---------------------
  NPT_spc <- vapply(
    seq_along(npt_spc),
    FUN = function(i) {
      seek(con, npt_spc[i], origin = "start", rw = "read")
      readBin(con, what = "integer", n = 12, size = 4)[2]
    },
    FUN.VALUE = numeric(1)
  )

  # Delete NPT with negative signs
  NPT_spc <- NPT_spc[NPT_spc > 0]

  ## Read all spectra ==========================================================

  spc <- Map(
    function(end, NPT) {
      seek(con, end - 4, origin = "start", rw = "read")
      readBin(con, what = "numeric", n = NPT, size = 4, endian = "little")
    },
    end_spc,
    NPT_spc
  )

  # Read FXV and LXV and calculate wavenumbers  --------------------------------

  FXV_spc <- vapply(
    fxv_spc,
    FUN = function(fxv_spc) {
      seek(con, fxv_spc, origin = "start", rw = "read")
      readBin(con, what = "numeric", n = 16, size = 8)[1]
    },
    FUN.VALUE = numeric(1)
  )

  LXV_spc <- vapply(
    lxv_spc,
    FUN = function(lxv_spc) {
      seek(con, lxv_spc, origin = "start", rw = "read")
      readBin(con, what = "numeric", n = 16, size = 8)[1]
    },
    FUN.VALUE = numeric(1)
  )

  # Calculate wavenumbers
  wavenumbers <- lapply(
    seq_along(FXV_spc),
    function(i) {
      rev(seq(LXV_spc[i], FXV_spc[i], (FXV_spc[i] - LXV_spc[i]) /
                (NPT_spc[i] - 1)))
    }
  )

  ## Assigning list of initially read spectra depending on block type ==========

  # Assign an index name to the spectra and parameters for reading
  names(end_spc) <- paste0("idx", 1:length(end_spc))
  names(spc) <- paste0("idx", 1:length(spc))
  names(NPT_spc) <- paste0("idx", 1:length(NPT_spc))
  names(FXV_spc) <- paste0("idx", 1:length(FXV_spc))
  names(wavenumbers) <- paste0("idx", 1:length(wavenumbers))

  # Check if elements in FXV_spc (frequency of first point) are equal to 0;
  # these are interferogram spectra
  which_Ig <- FXV_spc[which(FXV_spc == 0)]
  Ig_assigned <- if (length(which_Ig) == 0) {
    NULL
  } else if (length(which_Ig) == 1) {
    list(
      spc_idx = names(which_Ig),
      spc_code = "ig_sample"
    )
  } else if (length(which_Ig) == 3) {
    list(
      spc_idx = names(which_Ig)[c(1, 3)],
      spc_code = c("ig_sample", "ig_ref")
    )
  } else {
    list(
      spc_idx = names(which_Ig),
      spc_code = c("ig_sample", "ig_ref")
    )
  }

  na_assigned <- list(
    spc_idx = NULL,
    spc_code = NULL
  )
  if (length(which_Ig) == 3) {
    # Assign duplicated interferogram spectrum to 'not available' assigned
    na_assigned <- list(
      spc_idx = names(which_Ig)[2],
      spc_code = NA
    )
  }

  # Remove NA assigned spectra in spc list -------------------------------------
  if (!is.null(na_assigned$spc_idx)) {
    spc[na_assigned$spc_idx] <- NULL
    # Remove wavenumbers with NA assigned spectra in spc list
    wavenumbers[na_assigned$spc_idx] <- NULL
  }

  # Assign single channel spectra if present in file ---------------------------

  # Return idx (index names) of all remaining spectra that are not
  # interferograms
  notIg <- names(spc)[
    !names(spc) %in% c(Ig_assigned$spc_idx, na_assigned$spc_idx)
  ]

  # Check if the MIR range was measured
  wavenumbers_mir <- lapply(
    names(wavenumbers[notIg]),
    function(i) {
      spc[[i]][wavenumbers[notIg][[i]] < 2392 &
      wavenumbers[notIg][[i]] > 2358]
    })

  is_mir <- any(
    vapply(
      wavenumbers_mir,
      FUN = function(x) {length(x) != 0},
      FUN.VALUE = logical(1)
    )
  )

  if (isTRUE(is_mir)) {
    # Calculate peak ratio for absorbance at around 2392 cm^(-1)
    # and 2358 cm^(-1)
    peak_ratio <- lapply(
      lapply(names(wavenumbers[notIg]),
             function(i) {
               spc[[i]][wavenumbers[notIg][[i]] < 2392 &
                        wavenumbers[notIg][[i]] > 2358]
             }),
      function(j) j[[1]] / j[[length(j)]]
    )
    names(peak_ratio) <- names(spc[notIg])
    # Single channel (Sc) assignment list
    which_Sc <- names(which(peak_ratio > 2))
  } else {
    peak_ratio <- lapply(
      lapply(
        names(wavenumbers[notIg]),
        function(i) {
          spc[[i]][wavenumbers[notIg][[i]] < 5340 &
                     wavenumbers[notIg][[i]] > 5318]
        }
      ),
      function(j) {
        j[[1]] / j[[length(j)]]
      }
    )
    names(peak_ratio) <- names(spc[notIg])
    # Single channel (Sc) assignment list
    which_Sc <- names(which(peak_ratio < 0.9))
  }

  # browser()

  # Check for single channel, exclude spectral blocks already assigned to
  # interferograms
  Sc_assigned <- if (length(which_Sc) == 0) {
    NULL
  } else if (length(which_Sc) == 1) {
    list(
      spc_idx = which_Sc,
      spc_code = "sc_sample"
    )
  } else {
    list(
      spc_idx = which_Sc,
      spc_code = c("sc_sample", "sc_ref")
    )
  }

  # Assign corrected and uncorrected (if present) ------------------------------
  # AB spectra list
  which_AB <- names(spc)[
    !names(spc) %in% c(
      Ig_assigned[["spc_idx"]],
      na_assigned[["spc_idx"]],
      Sc_assigned[["spc_idx"]]
      )
    ]

  AB_assigned <- if (length(which_AB) == 1) {
    list(
      spc_idx = which_AB,
      spc_code = "spec"
    )
  } else {
    list(
      spc_idx = which_AB,
      spc_code = c("spec_no_atm_comp", "spec")
    )
  }

  # Read result spectrum with new offset (no `-4`) when atmospheric
  # compensation was done by the OPUS software; replace the spectrum position
  # with index name idx that corresponds to final spectrum after atmospheric
  # compensation; OPUS files from particular spectrometers/OPUS software
  # versions do still need the same offset end_spc[[spc_idx]] - 4 as the other
  # spectra types; new argument atm_comp_minus4offset (default FALSE) is a
  # quick fix to read files with different offsets after atmospheric
  # compensation ---------------------------------------------------------------
  if (length(which_AB) == 2 && !atm_comp_minus4offset) {
    seek(con, end_spc[which_AB[length(which_AB)]], origin = "start",
         rw = "read")
    spc[[which_AB[length(which_AB)]]] <- readBin(
      con,
      what = "numeric",
      # n = NPT_spc[which_AB[length(which_AB)]] * 4,
      n = NPT_spc[which_AB[length(which_AB)]],
      size = 4,
      endian = "little"
    )

  }

  # Assign spectra type for final spectra in element names of spc list ---------
  # Combine spectral assignments lists
  list_assigned <- list(
    'Ig' = Ig_assigned,
    'Sc' = Sc_assigned,
    'AB' = AB_assigned
  )

  # Transpose spectra assignment list, first remove NULL elements in list
  # https://stackoverflow.com/questions/54970592/how-to-transpose-a-list-of-vectors
  .base_transpose <- function(l) do.call(Map, c(f = list, l))
  # to avoid bringing purrr::transpose

  list_assigned_t <- .base_transpose(
    Filter(Negate(function(x) is.null(unlist(x))), list_assigned)
  )

  # Save spectra index (spc_idx) and spectra code (spc_code)
  # in character vector
  spc_idx <- unlist(list_assigned_t[["spc_idx"]])
  spc_code <- unlist(list_assigned_t[["spc_code"]])

  # Order spc_idx from 1 to n spectra (n = length of end_spc)
  order_spc <- as.numeric(
    sub(".*idx", "", unlist(list_assigned_t[["spc_idx"]]))
  )

  spc_type <- spc_code[order(order_spc)]

  # Set spectrum type as element names of spectra list (spc)
  names(spc) <- spc_type

  # Set spectrum type in wavenumbers list
  names(wavenumbers) <- spc_type

  # Read with new offset when first value of
  # ScSm  single channel sample spectrumspectrum is 0 and replace previous ---
  if (any(names(spc) %in% "sc_sample" & spc[["sc_sample"]][1] == 0)) {
    seek(
      con,
      end_spc[Sc_assigned$spc_idx[Sc_assigned$spc_code == "sc_sample"]],
      origin = "start",
      rw = "read"
    )
    spc[["sc_sample"]] <- readBin(
      con,
      what = "numeric",
      # n = NPT_spc[Sc_assigned$spc_idx[Sc_assigned$spc_code == "ScSm"]] * 4,
      n = NPT_spc[Sc_assigned$spc_idx[Sc_assigned$spc_code == "sc_sample"]],
      size = 4,
      endian = "little"
    )
  }

  ## Get additional parameters from OPUS binary file ===========================

  # Instrument parameters ------------------------------------------------------

  ins <- grepRaw("INS", rw, all = TRUE) # Instrument type
  seek(con, ins[length(ins)] + 7, origin = "start", rw = "read")
  INS <- readBin(
    con,
    what = "character",
    n = 10,
    size = 1,
    endian = "little"
  )[1]

  lwn <- grepRaw("LWN", rw, all = TRUE)[1] + 7 # Laser wavenumber
  seek(con, lwn, origin = "start", rw = "read")
  LWN <- readBin(
    con,
    what = "numeric",
    n = 8,
    size = 8
  )[1]

  tsc <- grepRaw("TSC", rw, all = TRUE) + 7 # Scanner temperature
  TSC_all <- lapply(tsc, function(tsc) {
    seek(con, tsc, origin = "start", rw = "read")
    readBin(
      con,
      what = "numeric",
      n = 16,
      size = 8
    )[[1]]
  })

  # Read relative humidity of the interferometer during measurement
  hum_rel <- grepRaw("HUM", rw, all = TRUE) + 7
  HUM_rel <- lapply(
    hum_rel,
    function(hum_rel) {
      # can include sample and background humidity
      seek(con, hum_rel, origin = "start", rw = "read")
      readBin(
        con,
        what = "integer",
        n = 16,
        size = 8
      )[[1]]
    })
  HUM_rel <- HUM_rel[[1]]

  # Read absolute humidity of the interferometer during measurement
  hum_abs <- grepRaw("HUA", rw, all = TRUE) + 7
  HUM_abs <- lapply(
    hum_abs,
    function(hum_abs) {
      # can include sample and background humidity
      seek(con, hum_abs, origin = "start", rw = "read")
      readBin(
        con,
        what = "numeric",
        n = 16,
        size = 8
      )[[1]]
    })
  HUM_abs <- unlist(HUM_abs)
  if (is.null(HUM_abs)) HUM_abs <- NA

  # Optics parameters ----------------------------------------------------------

  src <- grepRaw("SRC", rw, all = TRUE) # Source: MIR or NIR
  seek(con, src[length(src)] + 4, origin = "start", rw = "read")
  SRC <- readBin(
    con,
    what = "character",
    n = 3,
    size = 1,
    endian = "little"
  )[[1]][1]

  # instrument range
  instr_range <- paste(INS, SRC, sep = "-")
  instr_range <- unlist(strsplit(instr_range, "'"))[1]

  bms <- grepRaw("BMS", rw, all = TRUE) # Beamsplitter
  seek(con, bms[length(bms)] + 4, origin = "start", rw = "read")
  BMS <- readBin(
    con,
    what = "character",
    n = 3,
    size = 1,
    endian = "little"
  )[[1]][1]
  BMS <- unlist(strsplit(BMS, "'", useBytes = TRUE))[1]

  # Fourier transform parameters -----------------------------------------------
  zff <- grepRaw("ZFF", rw, all = TRUE)[1] + 5 # Zero filling factor (numeric)
  seek(con, zff, origin = "start", rw = "read")
  ZFF <- readBin(
    con,
    what = "integer",
    n = 4,
    size = 2,
    endian = "little"
  )[1]

  # (Additional) Standard parameters -------------------------------------------

  csf_all <- grepRaw("CSF", rw, all = TRUE) + 7 # y-scaling factor
  # Read only CSF byte positions that correspond to final spectra
  CSF <- lapply(
    csf_all[npt_all %in% npt_spc],
    function(csf) {
      seek(con, csf, origin = "start", rw = "read")
      readBin(
        con,
        what = "numeric",
        n = 8,
        size = 8,
        endian = "little"
      )[1]
    })

  mxy_all <- grepRaw("MXY", rw, all = TRUE) + 7 # Y-maximum
  MXY <- unlist(
    lapply(
      mxy_all[npt_all %in% npt_spc],
      function(mxy) {
        seek(con, mxy, origin = "start", rw = "read")
        readBin(
          con,
          what = "numeric",
          n = 8,
          size = 8
        )[1]
      }
    )
  )

  mny <- grepRaw("MNY", rw, all = TRUE) + 7 # Y-minimum

  dxu_all <- grepRaw("DXU", rw, all = TRUE) + 7 # X units
  DXU <- lapply(
    dxu_all,
    function(dxu) {
      seek(con, dxu, origin = "start", rw = "read")
      readBin(
        con,
        what = "character",
        n = 3,
        size = 1,
        endian = "little"
      )[1]
    }
  )

  # Y units -> PR: there is no DYU present in file -> PB: yes, but there is in
  # others
  dyu_all <- grepRaw("DYU", rw, all = TRUE) + 7
  dat <- grepRaw("DAT", rw, all = TRUE) + 7 # Date

  tim <- grepRaw("TIM", rw, all = TRUE) + 7 # Time

  time <- unlist(
    lapply(
      tim,
      function(tim) {
        seek(con, tim, origin = "start", rw = "read")
        readBin(
          con,
          what = "character",
          n = 22,
          size = 1,
          endian = "little"
        )[1]
      }
    )
  )

  # Time needs to have a valid encoding; replace entries that have invalid
  # encoding with NA
  time_invalid <- !vapply(time, validEnc, FUN.VALUE = logical(1))
  time[time_invalid] <- NA

  # Only select "DAT" string positions that are immediately before time
  dat_sel <- vapply(
    seq_along(tim),
    FUN = function(i) {
      diff_sel <- dat - tim[i]
      res <- dat[which(diff_sel <= 32 & diff_sel >= -20)]

      # If no valid position can be extracted we flag value as NA
      if (length(res) == 0) res <- NA

      res
    },
    FUN.VALUE = numeric(1)
  )

  date <- lapply(
    dat_sel,
    function(dat){

      # If not date string was extracted we just return NA
      if (is.na(dat)) {
        res <- NA
      } else {
        seek(con, dat, origin = "start", rw = "read")
        res <- readBin(
          con,
          what = "character",
          n = 10,
          size = 1,
          endian = "little"
        )[1]
      }

      res

    }
  )

  date_time <- unique(paste(date, time))

  # Convert date_time from character to class POSIXct (calendar date and time)
  date_time <- as.POSIXct(date_time, format = "%d/%m/%Y %H:%M:%S")
  # , tz = "GMT+1") # tz is argument for time zone

  # Scale all spectra with y-scaling factor if any of spectra types present
  # in file are not 1 ----------------------------------------------------------
  # Set names of CSF elements equal to spectra list element names
  names(CSF) <- names(spc)

  if (any(unlist(CSF) != 1)) {
    # Return all elements in CSF that have scaling value not equal to 1
    CSF_toscale <- Filter(function(x) x != 1, CSF)
    # Apply scaling for spectra with CSF value not equal to 1;
    # Map() returns list
    spc_scaled <- Map(function(CSF, spc) CSF * spc, unlist(CSF_toscale),
                      spc[names(CSF_toscale)])

    # Replace all spc list elements that have CSF not equal 1 with
    # scaled values
    spc <- replace(x = spc, list = names(CSF_toscale), values = spc_scaled)
  }

  # Data acquisition parameters ------------------------------------------------

  plf <- grepRaw("PLF", rw, all = TRUE) + 4 # Result spectrum
  PLF_all <- lapply(
    plf,
    function(plf) {
      seek(con, plf, origin = "start", rw = "read")
      res <- readBin(
        con,
        what = "character",
        n = 2,
        size = 1,
        endian = "little"
      )[1]
      unlist(strsplit(res, ",", useBytes = TRUE))[1]
    }
  )

  # Select only result spectra abbreviations that have valid encoding and
  # are more than 0 characters long; some OPUS files had invalid encoding
  # for this entry (first element)
  PLF_invalid <- !vapply(PLF_all, validEnc, FUN.VALUE = logical(1))
  PLF_all[PLF_invalid] <- NA
  PLF <- unlist(PLF_all[lapply(PLF_all, nchar) > 0])
  PLF <- unique(unlist(strsplit(PLF, "'")))

  res <- grepRaw("RES", rw, all = TRUE)[1] + 5 # Resolution (wavenumber)
  seek(con, res, origin = "start", rw = "read")
  RES <- readBin(
    con,
    what = "integer",
    n = 4,
    size = 2,
    endian = "little"
  )[1]

  ## Create sample metadata objects ============================================

  snm <- grepRaw("SNM", rw, all = TRUE)[1] + 7
  seek(con, snm, origin = "start", rw = "read")
  SNM <- readBin(
    con,
    what = "character",
    n = 30,
    size = 1,
    endian = "little"
  )[1]

  # Close connection
  close(con)

  # == sample ID ==

  sample_name <- unlist(strsplit(SNM, ";", useBytes = TRUE))[1]
  sample_id <- sample_name
  # PB: 2021-09-01: todo: add `rep_no` via `file_id` or `stream_id`
  file_name_nopath <- NA # PB: 2021-08-19: to fix

  # Create unique_id using file_name and time
  ymdhms_id <- max(date_time, na.rm = TRUE)
  unique_id <- paste0(sample_id, "_", ymdhms_id)

  ## Convert all spectra in list spc into a matrix of 1 row ====================
  spc_m <- lapply(spc, function(x) matrix(x, ncol = length(x), byrow = FALSE))

  # Add dimnames (wavenumbers for columns and unique_id for rows
  spc_m <- setNames(
    lapply(
      seq_along(spc_m),
      function(i) {
        colnames(spc_m[[i]]) <- round(wavenumbers[[i]], 1)
        rownames(spc_m[[i]]) <- unique_id
        spc_m[[i]]
      }
    ),
    names(spc_m)
  )

  # Save all relevant metadata
  metadata <- data.frame(
    unique_id = unique_id,
    # Removing "file_id" which does not make sense on RAW streams
    # file_id = file_name_nopath, # pb (20170514): changed `scan_id` to `file_id`
    sample_id = sample_id,
    # rep_no = as.numeric(rep_no), # pb 2021-09-01: find workaround to re-add
    date_time_sm = max(date_time, na.rm = TRUE),
    date_time_rf = min(date_time, na.rm = TRUE),
    sample_name = SNM,
    instr_name_range = instr_range,
    resolution_wn = RES,
    # Result spectrum; e.g. "AB" = Absorbance
    # result_spc = ifelse(length(unique(PLF)) == 1, unique(PLF), unique(PLF)[2]),
    # // pb: 2019-11-19: allow NULL value for PLF
    result_spc = if (length(unique(PLF)) == 1) {
        unique(PLF)
      } else if (length(unique(PLF)) > 1) {
        unique(PLF)[2]
      } else {
        NA
      },
    beamspl = BMS,
    laser_wn = LWN,
    # `spc_in_file`: character vector of spectra found in OPUS file
    spc_in_file = paste(unlist(list_assigned_t[["spc_code"]]),
                        collapse = ";", sep = ";"),
    zero_filling = ZFF, # Zero filling factor for fourier transformation
    # Temperature of scanner during sample measurement
    temp_scanner_sm = TSC_all[[length(TSC_all)]], # select last element
    # Temperature of scanner during reference measurement;
    # if there is only one element in TSC_all, temperature during reference
    # measurement is not saved
    temp_scanner_rf = ifelse(length(TSC_all) == 1, NA, TSC_all[[1]]),
    # Relative humidity
    hum_rel_sm = HUM_rel[[length(HUM_rel)]], # sample measurement
    hum_rel_rf = ifelse(length(HUM_rel) == 1, NA, HUM_rel[[1]]), # reference
    # measurement
    # Absolute humidity; sample measurement (sm); reference measurment (rf);
    # note: for Vertex 70 instrument HUA is not present, in this case,
    # HUM_abs is a list without elements
    hum_abs_sm = ifelse(length(HUM_abs) != 0, HUM_abs[[length(HUM_abs)]], NA),
    hum_abs_rf = ifelse(
      length(HUM_abs) == 1 | length(HUM_abs) == 0,
      NA,
      HUM_abs[[1]]
    ), # reference measurement
    stringsAsFactors = FALSE
  )

  ## Allocate and return data from spectra in output list (out) ================

  out <- list(
    # Metadata
    'metadata' = metadata,
    'spec' = if("spec" %in% type) {
        if ("spec" %in% names(spc_m)) {
          spc_m[["spec"]]
        } else {
          warning("No spectra found", call. = F)
          NULL
        }
      } else {
        NULL
      },
    'spec_no_atm_comp' = if ("spec_no_atm_comp" %in% type) {
        if ("spec_no_atm_comp" %in% names(spc_m)) {
          spc_m[["spec_no_atm_comp"]]
        } else {
          warning("No 'spec_no_atm_comp' spectra found", call. = F)
          NULL
        }
    } else {
      NULL
    },
    'sc_sample' = if ("sc_sample" %in% type) {
        if ("sc_sample" %in% names(spc_m)) {
          spc_m[["sc_sample"]]
        } else {
          warning("No 'sc_sample' spectra found", call. = F)
          NULL
        }
    } else {
      NULL
    },
    'sc_ref' = if ("sc_ref" %in% type) {
        if ("sc_ref" %in% names(spc_m)) {
          spc_m[["sc_ref"]]
        } else {
          warning("No 'sc_ref' spectra found", call. = F)
          NULL
        }
    } else {
      NULL
    },
    'ig_sample' = if ("ig_sample" %in% type) {
        if ("ig_sample" %in% names(spc_m)) {
          spc_m[["ig_sample"]]
        } else {
          warning("No 'ig_sample' spectra found", call. = F)
          NULL
        }
    } else {
      NULL
    },
    'ig_ref' = if ("ig_ref" %in% type) {
        if("ig_ref" %in% names(spc_m)) {
          spc_m[["ig_ref"]]
        } else {
          warning("No 'ig_ref' spectra found", call. = F)
          NULL
        }
    } else {
      NULL
    },
    # Wavenumbers of final AB spectra
    wavenumbers = wavenumbers[["spec"]],
    wavenumbers_sc_sample = if ("sc_sample" %in% type) {
      wavenumbers[["sc_sample"]]
    } else {
      NULL
    },
    wavenumbers_sc_ref = if ("sc_ref" %in% type) {
      wavenumbers[["sc_ref"]]
    } else {
      NULL
    }
  )

  # Return spectra data and metadata contained as elements in list out
  return(out)
  }) # closes try() function
}
