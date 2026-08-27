---
name: openspecy-curate-class-reference
description: Curate and validate OpenSpecy reference-library material mappings. Use when editing classes_reference.csv or classes_regex.csv, predicting classes for unmatched spectrum_identity values, reviewing regex overlaps or clashes, or auditing class coverage against saved library metadata.
---

# Curate OpenSpecy Class References

Keep exact and pattern-based evidence separate and make every inferred mapping
reviewable.

## Workflow

1. Read the active Spec Kit plan and the reference-library workflow before
   editing lookup data.
2. Treat `workflows/data/classes_reference.csv` as the authoritative exact
   lookup. Require unique, normalized `spectrum_identity` keys. Never place
   regex syntax in this file.
3. Treat `workflows/data/classes_regex.csv` as the reviewed fallback. Require
   unique, nonblank `pattern` values and nonblank `material` values. Patterns
   may overlap populated exact identities because exact values always win.
4. Normalize filename-derived identities with the same internal rule used by
   `build_lib()`. In particular, remove a terminal period followed only by one
   or more digits as an OPUS extension; do not preserve `.10`, `.123`, or other
   numeric suffixes as identity content.
5. Apply the exact lookup first. Then call
   `predict_class_reference(metadata, regex_reference, return = "report")` on
   the remaining blank `material` values. Never overwrite an exact material.
6. Stop on regex clashes where one unmatched identity predicts distinct
   materials. Review `overlaps` separately; overlaps are allowed and should
   confirm that exact mappings remain authoritative.
7. Leave fouling, `like`, unknown, morphotype, composite, or ambiguous labels
   unmatched unless a reviewed rule has one defensible material outcome.
8. Report exact coverage, regex predictions, clashes, allowed overlaps, and the
   remaining unmatched queue. Validate against the saved library read-only;
   write rebuilds only to a staged temporary output.

## Pattern Guidance

- Anchor rules whenever the intended boundary is known.
- Scope structural rules to their source prefix when a token is meaningful only
  within one contributor's naming convention.
- Prefer one flexible reviewed pattern over filename-extension aliases.
- Do not turn correlation, fuzzy similarity, or a common substring into a
  chemical assignment.
- Add an exact row when a broad pattern would clash; the exact row wins without
  weakening the useful pattern.

## Verification

Run focused `build_lib` and `read_multi` tests, validate both CSVs for duplicate
or blank keys, regenerate roxygen output when the public function changes, run
the full package tests once, and run `-HostedAppStatic` because `R/` is a shared
hosted input.
