pkgload::load_all(".", export_all = TRUE, quiet = TRUE)

path <- "workflows/data/classes_reference.csv"
classes <- data.table::fread(path)
is_rule <- grepl("^regex:", classes$spectrum_identity)
rules <- classes[is_rule, .(
  pattern = sub("^regex:", "", spectrum_identity),
  material
)]
exact <- data.table::copy(classes[!is_rule])
exact[, original_identity := spectrum_identity]
exact[, spectrum_identity := .lib_clean_spectrum_identity(spectrum_identity)]

conflicts <- exact[
  !is.na(material) & nzchar(material),
  .(
    materials = paste(sort(unique(material)), collapse = "; "),
    original_identities = paste(sort(unique(original_identity)),
                                collapse = "; ")
  ),
  by = spectrum_identity
][grepl("; ", materials, fixed = TRUE)]

cat("exact before:", sum(!is_rule), "normalized keys:",
    data.table::uniqueN(exact$spectrum_identity), "conflicts:",
    nrow(conflicts), "regex rules:", nrow(rules), "\n")
print(conflicts, nrows = Inf)
if (nrow(conflicts) > 0L) quit(status = 2L)

compressed <- exact[, {
  values <- unique(material[!is.na(material) & nzchar(material)])
  list(material = if (length(values) == 0L) NA_character_ else values[[1L]])
}, by = spectrum_identity]
data.table::setorder(compressed, spectrum_identity)
data.table::setorder(rules, pattern)

stopifnot(
  !anyNA(compressed$spectrum_identity),
  !any(!nzchar(compressed$spectrum_identity)),
  !anyDuplicated(compressed$spectrum_identity),
  !anyNA(rules$pattern), !any(!nzchar(rules$pattern)),
  !anyNA(rules$material), !any(!nzchar(rules$material)),
  !anyDuplicated(rules$pattern)
)

data.table::fwrite(compressed, path, na = "")
data.table::fwrite(rules, "workflows/data/classes_regex.csv", na = "")
cat("wrote exact:", nrow(compressed), "regex:", nrow(rules), "\n")
