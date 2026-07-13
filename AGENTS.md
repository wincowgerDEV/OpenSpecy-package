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
guarded. For reference-library or other long-running external workflows, run
subset probes and staged temp-output/logged rebuilds before a full run; compare
rebuilt artifacts against available legacy IDs, wavenumber axes, metadata
counts/names, warnings, and representative `OpenSpecy` joins or matches before
claiming completion. The Shiny application may be bundled in this repository
under `inst/`; when porting from `wincowgerDEV/OpenSpecy-shiny`, keep app code
there, compress/downsample images, remove orphaned/duplicate/raw/generated
assets, report package-size impact, test helpers/server modules headlessly where
feasible, verify installed app paths/assets, and use manual or CI-guarded app
smoke tests. Package functionality and CRAN readiness take precedence over app
convenience. The hosted Shinylive/WebAssembly app should be generated from the
bundled app by GitHub Actions, use the repo's wasm CRAN-like package repository
from a hardcoded package version/commit pin, stage only the small medoid/model
libraries with guarded `get_lib()` workflows, and verify startup, assets, and
library matching without hand-editing generated web artifacts.
<!-- SPECKIT END -->
