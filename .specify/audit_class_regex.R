x <- read.csv(
  "workflows/data/classes_reference.csv",
  na.strings = c("", "NA"),
  check.names = FALSE
)

blank <- is.na(x$material)
s <- x[blank & grepl("^scipoly[0-9]+_", x$spectrum_identity), , drop = FALSE]
s$base <- sub("^scipoly[0-9]+_", "", s$spectrum_identity)
e <- x[!is.na(x$material), c("spectrum_identity", "material")]
m <- merge(
  s,
  e,
  by.x = "base",
  by.y = "spectrum_identity",
  all.x = TRUE,
  sort = FALSE
)
print(m[!is.na(m$material.y), ], row.names = FALSE)
cat("direct base matches=", sum(!is.na(m$material.y)), "of", nrow(m), "\n")

cat("\nTF blank summary by token:\n")
t <- x[blank & grepl("^tf[0-9]+_", x$spectrum_identity), ]
toks <- c(
  "acetate",
  "viscose|modal|rayon",
  "nylon",
  "cotton",
  "wool",
  "silk",
  "linen",
  "polyester",
  "spandex|lycra",
  "acrylic",
  "olefin|polypropylene"
)
for (p in toks) cat(p, sum(grepl(p, t$spectrum_identity)), "\n")

cat("\nAll blank textile identities:\n")
print(t, row.names = FALSE)
