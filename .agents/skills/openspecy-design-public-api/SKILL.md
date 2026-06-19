---
name: openspecy-design-public-api
description: Review proposed or changing OpenSpecy public R functions before implementation. Use for new exports, builder functions, argument additions, default changes, helper extraction, monolithic workflows, or APIs adapted from one-off scripts.
---

# Design OpenSpecy Public APIs

Run this review before editing public function signatures.

## Workflow

1. Read the current function, nearby package patterns, calling tests, vignette
   examples, and the source script being generalized.
2. Write the primary user workflow in one short pipeline using `OpenSpecy`
   objects. Treat that pipeline as the default behavior.
3. Classify every proposed argument:
   - **Required input**: data the function cannot infer.
   - **Meaningful policy**: a common choice with at least two credible uses.
   - **Advanced tuning**: pass through `...` when the underlying function
     already owns the option.
   - **Derived state**: infer it; do not expose an argument.
   - **Speculative**: remove it until a demonstrated workflow needs it.
4. Prefer input presence over paired booleans. For example, a non-`NULL`
   lookup should trigger its join without a separate `join_lookup` flag.
5. Keep advanced operations composable as standalone functions. Add them to a
   monolith only when they are integral to the standard workflow and have
   conservative defaults.
6. Keep a helper internal when it has one caller and no independent user value.
   Export it when it is broadly reusable, has a stable contract, and can be
   documented and tested independently.
7. Reuse package primitives and `OpenSpecy` structure. Do not add table-only
   pathways when the feature exists to protect spectral/metadata alignment.
8. Check names and descriptions for domain generality. Prefer material,
   hierarchy, metadata, and spectra terminology over one dataset's taxonomy.

## Approval Gate

Before implementation, confirm:

- Each public argument has a concrete current use case.
- Defaults reproduce the maintainer's standard workflow.
- Optional steps can be composed with base `|>`.
- `...` is used only for a clearly identified downstream function.
- Return types and side effects are singular and predictable.
- The plan names compatibility, tests, docs, NEWS, and benchmark impact.

If any item is unclear, simplify the API before adding code.
