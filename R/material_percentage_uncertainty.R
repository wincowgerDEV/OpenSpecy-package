#' Calculate material percentage uncertainty from observed particle counts
#'
#' @description
#' Calculates the absolute confidence-interval half-width for an observed
#' material-class percentage from the total number of particles characterized.
#' The calculation is the single-property proportion equation described by
#' Cowger et al. (2024), rearranged to solve for error after observation.
#'
#' @param count positive whole-number total particle count. May be a scalar or
#'   a numeric vector.
#' @param percentage observed material-class percentage in the closed interval
#'   0--100. May be a scalar or a numeric vector.
#' @param confidence confidence level in the open interval 0--1. Defaults to
#'   0.95. May be a scalar or a numeric vector.
#'
#' @return
#' A numeric vector containing absolute uncertainty half-widths in percentage
#' points. Inputs must either have length one or share one common length.
#'
#' @details
#' For proportion `p = percentage / 100`, total observed count `n`, and
#' two-tailed normal critical value `z`, the returned half-width is
#' `100 * abs(z) * sqrt(p * (1 - p) / n)`. This is a material-composition
#' uncertainty under representative random particle sampling. It does not
#' include laboratory, spectral-identification, concentration, finite-
#' population, or multiple-property uncertainty. In particular, the published
#' Wald-style equation returns zero at exactly 0 and 100 percent.
#'
#' @examples
#' material_percentage_uncertainty(100, c(20, 50, 80))
#' material_percentage_uncertainty(100, 50, confidence = 0.90)
#'
#' @references
#' Cowger W, Markley LAT, Moore S, Gray AB, Upadhyay K, Koelmans AA (2024).
#' "How many microplastics do you need to (sub)sample?" *Ecotoxicology and
#' Environmental Safety*, **275**, 116243.
#' \doi{10.1016/j.ecoenv.2024.116243}.
#'
#' @author Win Cowger
#' @export
material_percentage_uncertainty <- function(
    count,
    percentage,
    confidence = 0.95) {
  inputs <- list(
    count = count,
    percentage = percentage,
    confidence = confidence
  )
  lengths <- lengths(inputs)
  output_length <- max(lengths)
  if (output_length == 0L || any(!lengths %in% c(1L, output_length))) {
    stop(
      "'count', 'percentage', and 'confidence' must have length one or a ",
      "shared common length",
      call. = FALSE
    )
  }
  for (name in names(inputs)) {
    value <- inputs[[name]]
    if (!is.numeric(value) || any(!is.finite(value))) {
      stop("'", name, "' must contain only finite numeric values", call. = FALSE)
    }
    if (length(value) == 1L) inputs[[name]] <- rep(value, output_length)
  }
  count <- inputs$count
  percentage <- inputs$percentage
  confidence <- inputs$confidence
  if (any(count <= 0 | count != floor(count))) {
    stop("'count' must contain positive whole numbers", call. = FALSE)
  }
  if (any(percentage < 0 | percentage > 100)) {
    stop("'percentage' must be between 0 and 100", call. = FALSE)
  }
  if (any(confidence <= 0 | confidence >= 1)) {
    stop("'confidence' must be between 0 and 1, exclusive", call. = FALSE)
  }

  proportion <- percentage / 100
  critical_value <- abs(stats::qnorm((1 - confidence) / 2))
  as.numeric(
    100 * critical_value * sqrt(proportion * (1 - proportion) / count)
  )
}

.particle_uncertainty_columns <- function(class_count, confidence = 0.95) {
  class_count <- as.numeric(class_count)
  if (!length(class_count)) {
    return(data.table::data.table(
      total_particle_count = integer(),
      percentage = numeric(),
      confidence_level = numeric(),
      percentage_uncertainty = numeric(),
      percentage_ci_lower = numeric(),
      percentage_ci_upper = numeric(),
      total_concentration_rsd = numeric()
    ))
  }
  total_count <- sum(class_count)
  percentage <- 100 * class_count / total_count
  uncertainty <- material_percentage_uncertainty(
    count = total_count,
    percentage = percentage,
    confidence = confidence
  )
  data.table::data.table(
    total_particle_count = rep(as.integer(total_count), length(class_count)),
    percentage = percentage,
    confidence_level = rep(as.numeric(confidence), length(class_count)),
    percentage_uncertainty = uncertainty,
    percentage_ci_lower = pmax(0, percentage - uncertainty),
    percentage_ci_upper = pmin(100, percentage + uncertainty),
    total_concentration_rsd = rep(total_count^(-1 / 2), length(class_count))
  )
}
