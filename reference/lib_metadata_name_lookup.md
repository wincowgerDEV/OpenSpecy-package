# Create and apply metadata-name lookup rules

`lib_metadata_name_lookup()` returns the default editable rules used to
merge synonymous metadata columns. `lib_clean_name()` converts names to
lowercase underscore form. `lib_clean_metadata()` cleans table names and
coalesces columns that map to the same canonical name.

## Usage

``` r
lib_metadata_name_lookup(
  ...,
  regex = NULL,
  defaults = TRUE,
  match_without_underscores = TRUE,
  match_singular_plural = TRUE
)

lib_clean_name(x)

lib_clean_metadata(
  x,
  name_lookup = lib_metadata_name_lookup(),
  clean_values = FALSE
)
```

## Arguments

- ...:

  named character vectors of exact aliases, where each argument name is
  the canonical name, or data.frame/data.table rule tables with
  `canonical_name`, `source_name`, and optional `regex` columns.

- regex:

  an optional named character vector or named list of regular
  expressions. Names identify the canonical metadata names.

- defaults:

  logical; whether to include OpenSpecy's default semantic aliases
  before merging user rules.

- match_without_underscores:

  logical; whether names that differ only by underscores should match
  automatically.

- match_singular_plural:

  logical; whether names that differ only by one terminal `s` should
  match automatically.

- x:

  a character vector of names for `lib_clean_name()`, or a
  data.frame/data.table for `lib_clean_metadata()`.

- name_lookup:

  a table returned by `lib_metadata_name_lookup()` or a compatible rule
  table. Use `NULL` to clean names without alias merging.

- clean_values:

  logical; whether `lib_clean_metadata()` should also lowercase, trim,
  ASCII-normalize, and normalize blank/unknown character or factor
  metadata values.

## Value

`lib_metadata_name_lookup()` returns a data.table of rules.
`lib_clean_name()` returns a character vector. `lib_clean_metadata()`
returns a data.table with cleaned, coalesced columns.

## Details

Exact rules determine a column's target before automatic matching that
can ignore underscores and a single terminal plural `s`. When values are
coalesced, canonical and mechanically equivalent canonical names come
before semantic aliases. Regular-expression rules are applied last to
names that remain unmatched. Regex patterns are evaluated against names
after `lib_clean_name()` has been applied.

Matching options selected in `lib_metadata_name_lookup()` are stored
with the returned table and used by `lib_clean_metadata()`. User rules
supplied through `...` are merged with the defaults. Set
`defaults = FALSE` to construct a lookup from only user rules.

## Examples

``` r
lib_clean_name(c("User Name", "Laser (%)", "Method...3"))
#> [1] "user_name"  "laser_perc" "method_3"  

name_lookup <- lib_metadata_name_lookup(
  project_code = "campaign name",
  regex = list(instrument_mode = "^method_[0-9]+$")
)
metadata <- data.frame(
  UserName = c("A", NA),
  user_name = c(NA, "B"),
  Campaign.Name = c("one", "two"),
  Method.23 = c("ftir", "raman")
)
lib_clean_metadata(metadata, name_lookup)
#>    user_name project_code instrument_mode
#>       <char>       <char>          <char>
#> 1:         A          one            ftir
#> 2:         B          two           raman
```
