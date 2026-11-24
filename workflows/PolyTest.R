# From the package or its parent: Rscript --vanilla PolyTest.R
# Optional arguments: derivative FTIR RDS, new output folder, unfiltered run folder.
# Supplying the previous unfiltered run requests the faster forward-only check.
# From RStudio: source("PolyTest.R"); result <- run_poly_test()
# Match scores are Pearson r for the cropped first derivatives.

poly_log <- function(fmt, ...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(fmt, ...)))
  flush.console()
}

poly_repo <- function() {
  candidates <- c(".", "OpenSpecy-package", "..")
  hit <- candidates[file.exists(file.path(candidates, "R", "match_spec.R"))]
  if (!length(hit)) stop("Run from the OpenSpecy package, its parent, or workflows/.")
  normalizePath(hit[[1L]], winslash = "/")
}

poly_slice <- function(x, rows, cols) {
  # Carry the recipe and intensity attributes forward. The matcher needs only IDs.
  result <- x
  result$wavenumber <- x$wavenumber[rows]
  result$spectra <- x$spectra[rows, cols, drop = FALSE]
  result$metadata <- x$metadata[cols, .(sample_name, file_id)]
  result
}

poly_eligibility <- function(x, rows, cols, chunk_size = 25000L) {
  status <- character(length(cols))
  for (start in seq.int(1L, length(cols), by = chunk_size)) {
    i <- seq.int(start, min(length(cols), start + chunk_size - 1L))
    block <- x$spectra[rows, cols[i], drop = FALSE]
    finite <- colSums(!is.finite(block)) == 0L
    status[i] <- "incomplete_or_nonfinite"
    if (any(finite)) {
      retained <- which(finite)
      ranges <- matrixStats::colRanges(block, cols = retained, useNames = FALSE)
      status[i[retained]] <- ifelse(ranges[, 2L] > ranges[, 1L], "eligible", "constant")
    }
  }
  status
}

poly_scale <- function(spectra) {
  centered <- sweep(spectra, 2L, colMeans(spectra))
  sweep(centered, 2L, sqrt(matrixStats::colSums2(centered^2)), "/")
}

poly_pearson_winners <- function(reference, query) {
  # Complete, centered unit vectors give Pearson r by dot product.
  poly_column_winners(crossprod(reference, query), colnames(reference))
}

poly_column_winners <- function(scores, reference_ids) {
  columns <- seq_len(ncol(scores))
  winner <- vapply(columns, function(i) which.max(scores[, i]), integer(1L))
  list(id = reference_ids[winner], score = scores[cbind(winner, columns)])
}

poly_reverse_winners <- function(scores, columns, previous, indices) {
  block_best <- matrixStats::rowMaxs(scores, useNames = FALSE)
  possible <- which(block_best > previous |
                      (block_best == previous & columns[1L] < indices))
  if (length(possible)) {
    candidate <- columns[max.col(scores[possible, , drop = FALSE], ties.method = "first")]
    replace <- block_best[possible] > previous[possible] | candidate < indices[possible]
    where <- possible[replace]
    previous[where] <- block_best[where]
    indices[where] <- candidate[replace]
  }
  list(values = previous, indices = indices, block_best = block_best)
}

poly_thresholds <- function(forward, reverse, cutoffs) {
  summarize <- function(values, label) {
    data.table::data.table(
      direction = label, min_r = cutoffs,
      count = vapply(cutoffs, function(t) sum(values >= t), integer(1L)),
      total = length(values)
    )[, fraction := count / total]
  }
  data.table::rbindlist(list(
    summarize(forward, "observed_plastic_to_computed_nonplastic"),
    if (length(reverse)) summarize(reverse, "computed_nonplastic_to_observed_plastic")
  ))
}

poly_bound_scores <- function(values) {
  # Normalize round-off at the legal Pearson endpoints.
  edge <- 8 * .Machine$double.eps
  values[values >= 1 - edge] <- 1
  values[values <= -1 + edge] <- -1
  values
}

poly_box_stats <- function(values, groups) {
  lapply(split(values, groups, drop = TRUE), grDevices::boxplot.stats,
         do.conf = FALSE, do.out = FALSE)
}

poly_box_panel <- function(boxes, title, color, limits, references = NULL) {
  at <- seq_along(boxes)
  labels <- names(boxes)
  # The full library names and exact box statistics are saved separately.
  labels <- sub(" - (Fourier-transform Infrared.*|Computed .*|Calculated .*|TranSpec .*)$",
                "", labels)
  ambiguous <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  labels[ambiguous] <- names(boxes)[ambiguous]
  counts <- vapply(boxes, function(b) b$n, integer(1L))
  stats <- vapply(boxes, function(b) b$stats, numeric(5L))
  graphics::bxp(
    list(stats = stats, n = counts, names = labels),
    horizontal = TRUE, outline = FALSE, las = 1, boxfill = color,
    # bxp swaps its argument axes when drawing horizontal boxes.
    ylim = limits, main = title, xlab = "Best Pearson correlation",
    cex.axis = 0.76
  )
  group_limits <- graphics::par("usr")[3:4]
  stopifnot(all(at >= group_limits[1L] & at <= group_limits[2L]))
  graphics::abline(v = 0.90, lty = 3, col = "grey55")
  count_text <- paste0("n=", format(counts, big.mark = ",", trim = TRUE))
  if (!is.null(references)) {
    count_text <- paste0(count_text, " | refs=", format(references[names(boxes)],
                                                       big.mark = ",", trim = TRUE))
  }
  graphics::axis(4, at = at, labels = count_text, las = 1, tick = FALSE,
                 cex.axis = 0.72)
}

poly_bidirectional_reports <- function(output, forward, reverse, type_best,
                                       observed_libraries, computed_libraries,
                                       type_counts) {
  grid <- seq(-1, 1, by = 0.0005)
  cdf <- data.table::data.table(
    r = grid,
    observed_plastic_to_computed_nonplastic = findInterval(grid, sort(forward)) / length(forward),
    computed_nonplastic_to_observed_plastic = findInterval(grid, sort(reverse)) / length(reverse)
  )
  data.table::fwrite(cdf, file.path(output, "top_correlation_cdf.csv"))
  draw_cdf <- function() {
    grDevices::png(file.path(output, "top_correlation_cdf.png"), 1350, 900, res = 140)
    on.exit(grDevices::dev.off())
    colors <- c("#2166AC", "#B84717")
    graphics::plot(cdf$r, cdf[[2L]], type = "s", col = colors[1L], lwd = 2.5,
                   xlim = range(min(forward, reverse), 1), ylim = c(0, 1),
                   xlab = "Best Pearson correlation", ylab = "Fraction at or below r",
                   main = "Best FTIR matches in both directions")
    graphics::lines(cdf$r, cdf[[3L]], type = "s", col = colors[2L], lwd = 2.5)
    graphics::legend(
      "topleft",
      sprintf(c("Observed plastic to computed non-plastic (n=%s)",
                "Computed non-plastic to observed plastic (n=%s)"),
              format(c(length(forward), length(reverse)), big.mark = ",", trim = TRUE)),
      col = colors, lwd = 2.5, bty = "n", cex = 0.85
    )
  }
  draw_cdf()

  observed_boxes <- poly_box_stats(forward, observed_libraries)
  computed_boxes <- poly_box_stats(reverse, computed_libraries)
  type_boxes <- lapply(seq_len(ncol(type_best)), function(j) {
    grDevices::boxplot.stats(type_best[, j], do.conf = FALSE, do.out = FALSE)
  })
  names(type_boxes) <- colnames(type_best)
  draw_libraries <- function() {
    n <- c(length(observed_boxes), length(computed_boxes))
    grDevices::png(file.path(output, "max_cor_by_library.png"), 2200,
                   max(1000L, 38L * sum(n) + 330L), res = 130)
    on.exit(grDevices::dev.off())
    graphics::layout(matrix(1:2, ncol = 1L), heights = n + 4L)
    graphics::par(mar = c(4, 39, 3, 10), mgp = c(2.1, 0.6, 0))
    limits <- range(min(forward, reverse), 1)
    poly_box_panel(observed_boxes, "Observed plastic queries by source library", "#AFCDE5", limits)
    poly_box_panel(computed_boxes, "Computed non-plastic queries by source library", "#F4C4AF", limits)
  }
  draw_libraries()
  draw_types <- function() {
    grDevices::png(file.path(output, "max_cor_by_polymer_type.png"), 2200,
                   max(1000L, 40L * length(type_boxes) + 260L), res = 130)
    on.exit(grDevices::dev.off())
    graphics::par(mar = c(4, 26, 4, 20), mgp = c(2.1, 0.6, 0))
    poly_box_panel(type_boxes, "Computed non-plastic queries vs. each observed polymer type",
                   "#F4C4AF", range(min(type_best), 1), type_counts)
  }
  draw_types()
  stats <- function(boxes, kind) {
    data.table::rbindlist(lapply(names(boxes), function(name) {
      b <- boxes[[name]]
      data.table::data.table(
        grouping = kind, group = name, n = b$n,
        polymer_references = if (kind == "polymer_type") unname(type_counts[name]) else NA_integer_,
        lower_whisker = b$stats[1L], q1 = b$stats[2L], median = b$stats[3L],
        q3 = b$stats[4L], upper_whisker = b$stats[5L]
      )
    }))
  }
  data.table::fwrite(data.table::rbindlist(list(
    stats(observed_boxes, "observed_query_library"),
    stats(computed_boxes, "computed_query_library"),
    stats(type_boxes, "polymer_type")
  )), file.path(output, "max_correlation_boxplot_stats.csv"))
}

poly_review_plot <- function(review, pair = 1L) {
  stopifnot(length(pair) == 1L, pair %in% seq_len(nrow(review$pairs)))
  p <- review$pairs[pair, , drop = FALSE]
  wn <- review$wavenumber
  y <- cbind(
    scale(review$observed[, p$object_id]),
    scale(review$computed[, p$library_id])
  )
  graphics::matplot(
    wn, y, type = "l", lty = c(1, 2), col = c("#2166AC", "#B84717"),
    xlim = rev(range(wn)), xlab = expression("Wavenumber (cm"^{-1}*")"),
    ylab = "First derivative (z-score)",
    main = sprintf("Pair %d  |  r = %.4f", pair, p$match_val)
  )
  graphics::abline(v = c(1800, 2800, 3050), col = "grey80", lty = 3)
  short <- function(x) substr(gsub("[[:cntrl:]]", " ", x), 1L, 57L)
  graphics::legend(
    "topleft", paste(c("Observed", "Computed"), short(c(
      p$observed_spectrum_identity, p$computed_spectrum_identity
    )), sep = ": "),
    col = c("#2166AC", "#B84717"), lty = c(1, 2), bty = "n", cex = 0.74
  )
}

poly_histogram <- function(scores, path, review_cutoff = 0.90) {
  grDevices::png(path, 1200, 750, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  h <- graphics::hist(pmax(-1, pmin(1, scores)),
                      breaks = seq(-1, 1, by = 0.01), plot = FALSE)
  first_bin <- which(h$counts > 0L)[1L]
  graphics::plot(
    h, xlim = c(h$breaks[first_bin], 1), freq = TRUE,
    main = sprintf("Best computed non-plastic match per observed plastic (n = %s)",
                   format(length(scores), big.mark = ",")),
    xlab = expression("Best Pearson correlation, 800 to 3200 cm"^{-1}),
    ylab = "Observed spectra", col = "#78A4C7", border = "white"
  )
  graphics::abline(v = review_cutoff, lty = 2, col = "#B84717", lwd = 2)
}

run_poly_test <- function(input = NULL, output = NULL, batch_size = 512L,
                          reference_block = 50000L, review_cutoff = 0.90,
                          baseline = NULL) {
  repo <- poly_repo()
  # The installed CRAN package may predate the bounded matcher. Load this checkout.
  pkgload::load_all(repo, quiet = TRUE)
  if (is.null(input)) {
    input <- file.path(repo, "rawrefdata", "updatedv1", "Processed",
                       "reference-library-overhaul-r7-metadata-v1", "releases",
                       "0377ccf73c12", "derivative_ftir.rds")
  }
  if (is.null(output)) {
    output <- file.path(repo, "rawrefdata", "updatedv1", "Processed",
                        "polytest", format(Sys.time(), "%Y%m%d-%H%M%S"))
  }
  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  stopifnot(batch_size >= 1L, reference_block >= 1L, is.finite(review_cutoff),
            review_cutoff >= -1, review_cutoff <= 1)
  if (!dir.create(output, recursive = TRUE)) stop("Use a new output directory.")
  output <- normalizePath(output, winslash = "/")
  sink(file.path(output, "run.log"), split = TRUE)
  on.exit(sink(), add = TRUE)

  started <- proc.time()[["elapsed"]]
  poly_log("Loading %s (%.2f GiB on disk)", input, file.size(input) / 1024^3)
  x <- readRDS(input)
  poly_log("Loaded in %.1f s; hashing input", proc.time()[["elapsed"]] - started)
  sha256 <- digest::digest(input, file = TRUE, algo = "sha256")
  ids <- colnames(x$spectra)
  md <- x$metadata
  stopifnot(OpenSpecy::is_OpenSpecy(x), is.matrix(x$spectra), is.numeric(x$spectra),
            data.table::is.data.table(md), nrow(md) == ncol(x$spectra),
            nrow(x$spectra) == length(x$wavenumber), !anyNA(ids),
            !anyDuplicated(ids), identical(md$sample_name, ids),
            all(is.finite(x$wavenumber)), !is.unsorted(x$wavenumber, strictly = TRUE),
            all(c("file_id", "spectrum_type", "spectrum_origin", "material_type",
                  "material_class", "spectrum_identity", "library_name") %in% names(md)),
            all(tolower(md$spectrum_type) == "ftir"))
  if (is.null(attr(x, "derivative_order")) || attr(x, "derivative_order") != 1L) {
    stop("Use a first-derivative FTIR artifact.")
  }
  rows <- which(x$wavenumber >= 800 & x$wavenumber <= 3200)
  stopifnot(length(rows) >= 3L, min(x$wavenumber) <= 800, max(x$wavenumber) >= 3200)
  wn <- x$wavenumber[rows]
  poly_log("Artifact SHA-256: %s", sha256)
  poly_log("Window: %g to %g cm^-1 on the native grid (%d points)", min(wn), max(wn), length(wn))

  origin <- trimws(as.character(md$spectrum_origin))
  material <- tolower(trimws(as.character(md$material_type)))
  query_cols <- which(origin == "Observed" & material == "plastic")
  ref_cols <- which(origin == "Computed" & material == "not plastic")
  if (!length(query_cols) || !length(ref_cols)) stop("An input cohort is empty.")
  eligibility <- function(cols, cohort) {
    status <- poly_eligibility(x, rows, cols)
    count <- data.table::data.table(
      cohort = cohort, origin = origin[cols], material_type = material[cols],
      material_class = md$material_class[cols], library_name = md$library_name[cols],
      status = status
    )[, .(spectra = .N), by = .(cohort, origin, material_type, material_class, library_name, status)]
    list(cols = cols[status == "eligible"], counts = count)
  }
  query_info <- eligibility(query_cols, "observed_plastic")
  ref_info <- eligibility(ref_cols, "computed_nonplastic")
  counts <- data.table::rbindlist(list(query_info$counts, ref_info$counts))
  excluded_library <- "Gicquel Synthetic Microplastics - Computed Fourier-transform Infrared"
  excluded <- ref_info$cols[md$library_name[ref_info$cols] %in% excluded_library]
  reference_cols <- ref_info$cols[!ref_info$cols %in% excluded]
  query_ids <- ids[query_info$cols]
  n_query <- length(query_ids)
  n_reference <- length(reference_cols)
  if (!n_query || !n_reference) stop("An eligible cohort is empty.")
  best_score <- rep(-Inf, n_query)
  best_id <- rep(NA_character_, n_query)
  scan <- seq_len(n_query)
  baseline_checksum <- NULL
  if (!is.null(baseline)) {
    # Removing candidates can only change a winner that is itself removed.
    baseline <- normalizePath(baseline, winslash = "/", mustWork = TRUE)
    summary <- readLines(file.path(baseline, "run_summary.txt"), warn = FALSE)
    old_eligibility <- data.table::fread(file.path(baseline, "eligibility.csv"))
    old_path <- file.path(baseline, "top_matches.csv")
    baseline_checksum <- digest::digest(old_path, file = TRUE, algo = "sha256")
    old <- data.table::fread(old_path, select = c("object_id", "library_id", "match_val"))
    if (!any(grepl(sha256, grep("^ *\\$ sha256 ", summary, value = TRUE), fixed = TRUE)) ||
        !identical(as.data.frame(old_eligibility), as.data.frame(counts)) ||
        anyDuplicated(old$object_id) || !setequal(old$object_id, query_ids) ||
        !all(old$library_id %in% ids[ref_info$cols]) ||
        !all(is.finite(old$match_val) & abs(old$match_val) <= 1 + 1e-12)) {
      stop("Baseline must be the complete unfiltered run for the same artifact and eligibility rules.")
    }
    old <- old[match(query_ids, object_id)]
    scan <- which(old$library_id %in% ids[excluded])
    keep <- setdiff(seq_len(n_query), scan)
    # CSV scores have finite decimal precision. Confirm retained scores
    # against the actual paired traces before using them in this run.
    old_refs <- match(old$library_id[keep], ids)
    direct_scores <- vapply(seq_along(keep), function(i) {
      stats::cor(x$spectra[rows, query_info$cols[keep[i]]],
                 x$spectra[rows, old_refs[i]])
    }, numeric(1L))
    stopifnot(all(abs(direct_scores - old$match_val[keep]) < 1e-12))
    best_score[keep] <- direct_scores
    best_id[keep] <- old$library_id[keep]
    poly_log("Reused %d retained winners; %d removed winners require an exact rescan.",
             length(keep), length(scan))
  }
  counts[cohort == "computed_nonplastic" & library_name == excluded_library &
           status == "eligible", status := "excluded_source"]
  data.table::fwrite(counts, file.path(output, "eligibility.csv"))
  data.table::fwrite(
    data.table::data.table(origin, material_type = material,
                          material_class = md$material_class,
                          library_name = md$library_name)[
                            , .(spectra = .N), by = .(origin, material_type, material_class, library_name)
                          ],
    file.path(output, "input_taxonomy.csv")
  )
  poly_log("Eligible observed plastic: %s of %s; computed non-plastic retained: %s of %s",
           n_query, length(query_cols), n_reference, length(ref_cols))
  poly_log("Excluded %d eligible references from %s.", length(excluded), excluded_library)

  n_scan <- length(scan)
  query <- poly_slice(x, rows, query_info$cols[if (n_scan) scan else 1L])
  starts <- if (n_scan) seq.int(1L, n_reference, by = reference_block) else integer()
  bidirectional <- is.null(baseline)
  batches <- function(indices) split(indices, ceiling(seq_along(indices) / batch_size))
  if (bidirectional) {
    polymer_type <- as.character(md$material_class[query_info$cols])
    polymer_type[is.na(polymer_type) | !nzchar(trimws(polymer_type))] <- "Unclassified"
    groups <- split(seq_len(n_query), polymer_type)
    type_counts <- lengths(groups)
    query_batches <- unlist(lapply(groups, batches), recursive = FALSE, use.names = FALSE)
    batch_types <- match(vapply(query_batches, function(q) polymer_type[q[1L]], character(1L)),
                         names(groups))
    type_best <- matrix(-Inf, nrow = n_reference, ncol = length(groups),
                        dimnames = list(NULL, names(groups)))
    reverse_score <- numeric(n_reference)
    reverse_index <- integer(n_reference)
  } else {
    query_batches <- batches(seq_len(n_scan))
    reverse_score <- numeric()
  }
  completed_queries <- cumsum(lengths(query_batches))
  n_batches <- length(query_batches)
  total_steps <- n_batches * length(starts)
  total_pairs <- as.double(n_scan) * n_reference

  probe_q <- poly_slice(query, seq_along(wn), seq_len(min(ncol(query$spectra), 16L)))
  probe_lib <- poly_slice(x, rows, head(reference_cols, 1024L))
  probe <- OpenSpecy::match_spec(
    probe_q, probe_lib, top_n = 1L, batch_size = 4L,
    progress = function(completed_blocks, total_blocks) {
      poly_log("match_spec() verification batch %d/%d", completed_blocks, total_blocks)
    }
  )
  direct <- stats::cor(probe_lib$spectra, probe_q$spectra)
  probe_max <- apply(direct, 2L, max)
  fast <- poly_pearson_winners(poly_scale(probe_lib$spectra), poly_scale(probe_q$spectra))
  probe_reverse <- poly_reverse_winners(
    crossprod(poly_scale(probe_lib$spectra), poly_scale(probe_q$spectra)),
    seq_len(ncol(direct)), rep(-Inf, nrow(direct)), rep(NA_integer_, nrow(direct))
  )
  stopifnot(OpenSpecy::check_OpenSpecy(probe_q), OpenSpecy::check_OpenSpecy(probe_lib),
            identical(probe$object_id, colnames(probe_q$spectra)),
            max(abs(probe$match_val - probe_max)) < 1e-12,
            max(abs(fast$score - probe_max)) < 1e-12,
            max(abs(probe_reverse$values - apply(direct, 1L, max))) < 1e-12,
            all(abs(direct[cbind(seq_len(nrow(direct)), probe_reverse$indices)] -
                    probe_reverse$values) < 1e-12),
            all(abs(direct[cbind(match(fast$id, rownames(direct)),
                                 seq_along(probe_max))] - probe_max) < 1e-12),
            all(abs(direct[cbind(match(probe$library_id, rownames(direct)),
                                 seq_along(probe_max))] - probe_max) < 1e-12))
  rm(probe_q, probe_lib, probe, direct, probe_max, fast, probe_reverse)
  scaled_query <- poly_scale(query$spectra)
  poly_log("Verified exact top scores against match_spec() and cor(). Comparing %s pairs in %d batches.",
           format(total_pairs, big.mark = ",", scientific = FALSE), total_steps)

  match_start <- proc.time()[["elapsed"]]
  for (j in seq_along(starts)) {
    first <- starts[j]
    part <- seq.int(first, min(n_reference, first + reference_block - 1L))
    ref <- poly_scale(x$spectra[rows, reference_cols[part], drop = FALSE])
    local_score <- rep(-Inf, length(part))
    local_index <- rep(NA_integer_, length(part))
    for (b in seq_along(query_batches)) {
      q <- query_batches[[b]]
      scores <- crossprod(ref, scaled_query[, q, drop = FALSE])
      hits <- poly_column_winners(scores, colnames(ref))
      slots <- scan[q]
      better <- hits$score > best_score[slots]
      best_score[slots[better]] <- hits$score[better]
      best_id[slots[better]] <- hits$id[better]
      if (bidirectional) {
        reverse <- poly_reverse_winners(scores, q, local_score, local_index)
        local_score <- reverse$values
        local_index <- reverse$indices
        type <- batch_types[b]
        type_best[part, type] <- pmax(type_best[part, type], reverse$block_best)
      }
      rm(scores)
      done <- as.double(first - 1L) * n_scan +
        as.double(length(part)) * completed_queries[b]
      elapsed <- proc.time()[["elapsed"]] - match_start
      remaining <- elapsed * (total_pairs / done - 1)
      poly_log("Batch %d/%d | reference block %d/%d, query batch %d/%d | %.1f%% | elapsed %.1f min | ETA %.1f min",
               (j - 1L) * n_batches + b, total_steps,
               j, length(starts), b, n_batches,
               100 * done / total_pairs, elapsed / 60, remaining / 60)
    }
    if (bidirectional) {
      reverse_score[part] <- local_score
      reverse_index[part] <- local_index
    }
    rm(ref, hits)
    gc(verbose = FALSE)
  }
  stopifnot(all(is.finite(best_score)), all(best_id %in% ids[reference_cols]))
  if (!is.null(baseline)) {
    stopifnot(all(best_score <= old$match_val + 1e-12))
  }
  if (bidirectional) {
    stopifnot(all(is.finite(reverse_score)), !anyNA(reverse_index),
              all(is.finite(type_best)),
              identical(reverse_score, matrixStats::rowMaxs(type_best, useNames = FALSE)))
  }
  best_score <- poly_bound_scores(best_score)
  reverse_score <- poly_bound_scores(reverse_score)
  thresholds <- poly_thresholds(best_score, reverse_score, c(seq(0.30, 1, by = 0.50), 1))
  fine_thresholds <- poly_thresholds(best_score, reverse_score, c(seq(30, 95, by = 5), 99, 100) / 100)
  matches <- data.table::data.table(
    object_id = query_ids, library_id = best_id, match_val = best_score,
    observed_library_name = md$library_name[query_info$cols],
    observed_material = md$material_class[query_info$cols],
    computed_library_name = md$library_name[match(best_id, ids)],
    computed_material = md$material_class[match(best_id, ids)]
  )
  data.table::setorder(matches, -match_val)
  data.table::fwrite(matches, file.path(output, "top_matches.csv"))
  data.table::fwrite(thresholds, file.path(output, "threshold_counts.csv"))
  data.table::fwrite(fine_thresholds, file.path(output, "threshold_counts_0p05.csv"))

  if (bidirectional) {
    computed_ids <- ids[reference_cols]
    computed_libraries <- md$library_name[reference_cols]
    observed_libraries <- md$library_name[query_info$cols]
    reverse_matches <- data.table::data.table(
      object_id = computed_ids,
      library_id = query_ids[reverse_index],
      match_val = reverse_score,
      computed_library_name = computed_libraries,
      computed_material = md$material_class[reference_cols],
      observed_library_name = observed_libraries[reverse_index],
      observed_material = polymer_type[reverse_index]
    )
    poly_log("Writing reverse matches and per-type scores for %s computed spectra.",
             format(n_reference, big.mark = ","))
    data.table::fwrite(reverse_matches, file.path(output, "reverse_top_matches.csv.gz"),
                       compress = "gzip")
    type_best <- poly_bound_scores(type_best)
    saveRDS(list(computed_ids = computed_ids, polymer_reference_counts = type_counts,
                 max_correlation = type_best),
            file.path(output, "computed_max_by_polymer_type.rds"), compress = FALSE)
    poly_bidirectional_reports(output, best_score, reverse_score, type_best,
                               observed_libraries, computed_libraries, type_counts)
  }

  # Keep every high-scoring winner and at least the 25 strongest for inspection.
  review <- data.table::copy(matches[seq_len(min(n_query, max(25L, sum(best_score >= review_cutoff))))])
  review[, above_review_cutoff := match_val >= review_cutoff]
  object_rows <- match(review$object_id, ids)
  library_rows <- match(review$library_id, ids)
  observed <- x$spectra[rows, unique(object_rows), drop = FALSE]
  computed <- x$spectra[rows, unique(library_rows), drop = FALSE]
  band_score <- function(band) {
    vapply(seq_len(nrow(review)), function(i) {
      a <- observed[band, review$object_id[i]]
      b <- computed[band, review$library_id[i]]
      if (min(length(a), length(b)) < 3L || diff(range(a)) == 0 || diff(range(b)) == 0) {
        return(NA_real_)
      }
      stats::cor(a, b)
    }, numeric(1L))
  }
  ch <- wn >= 2800 & wn <= 3050
  review[, `:=`(
    r_fingerprint_800_1800 = band_score(wn <= 1800),
    r_1800_2800 = band_score(wn > 1800 & wn < 2800),
    r_ch_2800_3050 = band_score(ch),
    r_without_2800_3050 = band_score(!ch),
    strongest_observed_cm1 = wn[apply(abs(observed[, object_id, drop = FALSE]), 2L, which.max)],
    strongest_computed_cm1 = wn[apply(abs(computed[, library_id, drop = FALSE]), 2L, which.max)]
  )]
  # Prefix both metadata sets so similarly named fields stay unambiguous.
  review[, c("observed_library_name", "computed_library_name",
             "observed_material", "computed_material") := NULL]
  for (side in c("observed", "computed")) {
    meta <- md[if (side == "observed") object_rows else library_rows]
    data.table::setnames(meta, paste0(side, "_", names(meta)))
    review <- cbind(review, meta)
  }
  review_data <- list(pairs = review, wavenumber = wn, observed = observed, computed = computed)
  saveRDS(review_data, file.path(output, "review_pairs.rds"))
  # The RDS preserves all columns and native types. Trim entirely blank CSV columns.
  keep <- vapply(review, function(col) any(!is.na(col) & nzchar(as.character(col))), logical(1L))
  flat <- review[, which(keep), with = FALSE]
  for (field in names(flat)[vapply(flat, is.list, logical(1L))]) {
    data.table::set(flat, j = field, value = vapply(flat[[field]], toString, character(1L)))
  }
  data.table::fwrite(flat, file.path(output, "review_pairs.csv"))

  poly_histogram(best_score, file.path(output, "top_correlation_histogram.png"), review_cutoff)

  distinct <- which(!duplicated(review[, .(
    observed_spectrum_identity, computed_spectrum_identity, computed_library_name
  )]))
  grDevices::png(file.path(output, "strongest_pair_overlays.png"), 1800, 2000, res = 145)
  graphics::par(mfrow = c(4, 3), mar = c(3.4, 3.7, 2.4, 0.6), mgp = c(2.1, 0.65, 0))
  for (i in head(distinct, 12L)) poly_review_plot(review_data, i)
  grDevices::dev.off()

  report <- list(input = input, sha256 = sha256, window = range(wn), points = length(wn),
                 query_count = n_query, reference_count = n_reference,
                 excluded_library = excluded_library, excluded_count = length(excluded),
                 baseline = baseline, baseline_matches_sha256 = baseline_checksum,
                 bidirectional = bidirectional,
                 rescanned_queries = n_scan, reused_queries = n_query - n_scan,
                 comparison_count = total_pairs, batch_size = batch_size,
                 reference_block = reference_block, review_cutoff = review_cutoff,
                 elapsed_seconds = proc.time()[["elapsed"]] - started,
                 quantiles = stats::quantile(best_score, c(0, .25, .5, .75, .9, .95, .99, 1)),
                 reverse_quantiles = if (bidirectional) stats::quantile(
                   reverse_score, c(0, .25, .5, .75, .9, .95, .99, 1)),
                 thresholds = thresholds, eligibility = counts[, .(spectra = sum(spectra)),
                                                                  by = .(cohort, status)])
  capture.output(str(report, give.attr = FALSE), file = file.path(output, "run_summary.txt"))
  print(report$eligibility)
  print(report$thresholds)
  print(report$quantiles)
  poly_log("Finished in %.1f min. Results: %s", report$elapsed_seconds / 60, output)
  invisible(list(output = output, matches = matches, review = review_data, summary = report))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  labels <- c("input", "output", "baseline")
  if (length(args) > length(labels)) {
    stop("Usage: Rscript PolyTest.R [input.rds] [new-output-directory] [unfiltered-run-directory]")
  }
  do.call(run_poly_test, setNames(as.list(args), head(labels, length(args))))
}
