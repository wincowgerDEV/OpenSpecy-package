<!-- SPECKIT START -->
Use `.specify/memory/constitution.md` as durable project memory and the active
concise feature plan as the implementation brief. Default Spec Kit workflow:
create or update one `plan.md` under `specs/<feature>/`; keep it under 100
nonblank lines with goals, requirements, technical decisions, package surfaces,
a short work checklist, verification, and open questions. Do not create separate
`spec.md`, `tasks.md`, research, data-model, contract, quickstart, or checklist
artifacts unless explicitly requested.

For this R package, do not edit generated `NAMESPACE`, `man/*.Rd`, or pkgdown
HTML directly; update roxygen or package metadata and regenerate with the
configured R tooling version. Inspect generated diffs immediately and treat
unexpected authorship, reference, alias, or export changes as source/toolchain
failures. Center `OpenSpecy` object structure (`wavenumber`, `spectra`,
`metadata`) and object attributes in function flows and examples.
Review public APIs before implementation: infer derived state, avoid speculative
flags, prefer input-triggered optional steps, keep one-caller helpers internal,
and preserve base-pipe composability.
For same-output function improvements, keep old comparison code in
`benchmarks/`, add or update a repeated benchmark that flags material
regressions, and keep `tests/` focused on current package behavior. Run focused
tests before full tests, documentation, and package checks. Network tests must
guard the actual download host. Keep long-running tests manual or GitHub Actions
guarded. The
Shiny application lives in `wincowgerDEV/OpenSpecy-shiny`; consider
compatibility when relevant, but package functionality takes precedence and
Shiny application code does not belong in this repository.
<!-- SPECKIT END -->
