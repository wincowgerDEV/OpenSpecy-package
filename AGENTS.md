<!-- SPECKIT START -->
Use `.specify/memory/constitution.md` as durable project memory and the active
concise feature plan as the implementation brief. Default Spec Kit workflow:
the active plan is `specs/025-random-forest-models/plan.md`;
create or update one `plan.md` under `specs/<feature>/`; keep it under 100
nonblank lines and target 1,500 words, with goals, requirements, technical
decisions, package surfaces, a short work checklist, verification, and open
questions. Close a plan to unrelated refinements once implementation is done
except for maintainer, CI, or post-push work. Do not create separate
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
tests before full tests, documentation, and package checks. Classify the current
tranche as presentation-only, bundled-app behavior, package/scientific, or
hosted/release; run only the smallest invalidated gate during iteration and run
triggered broad gates once on the final candidate. Reuse passing evidence while
its covered files, inputs, dependencies, and contracts are unchanged. After two
failures in the same broad stage, isolate a focused reproducer before rerunning.
Network tests must
guard the actual download host. Keep long-running tests manual or GitHub Actions
guarded. For reference-library or other long-running external workflows, run
subset probes and staged temp-output/logged rebuilds before a full run; define
representative kernel dimensions, expected time/memory, progress boundaries,
checkpoint behavior, and a stop/restart threshold before production scale; compare
rebuilt artifacts against available legacy IDs, wavenumber axes, metadata
counts/names, warnings, and representative `OpenSpecy` joins or matches before
claiming completion. The Shiny application may be bundled in this repository
under `inst/`; when porting from `wincowgerDEV/OpenSpecy-shiny`, keep app code
there, compress/downsample images, remove orphaned/duplicate/raw/generated
assets, report package-size impact, test helpers/server modules headlessly where
feasible, verify installed app paths/assets, and use manual or CI-guarded app
smoke tests. Apply `openspecy-develop-shiny-app` for local app work: feed plots,
summaries, identification, quantification, metadata, and downloads from one
canonical final spectral reactive (`OpenSpecy`, or a plan-approved compact
`Specs` map with bounded indexed conversion); gate child inputs behind owner controls;
preserve native downloads; and verify affected no-upload/processed/identified
states with genuine files plus console/screenshot review. Routine app iteration
does not require R CMD check unless explicitly requested or release-facing.
Treat `.specify/memory/pipeline-diagram.html` as the canonical map of that
pipeline: do not deviate from it outside an explicit plan naming the diagram
component changed, and update the diagram in the same change.
For non-obvious scientific or processing controls, provide adjacent guidance
that names each input, its units or scale, the effect of choices or higher/lower
values, and important rejection, no-op, or interpretation consequences.
Package functionality and CRAN readiness take precedence over app convenience.
Locate symbols with `rg` and read bounded source/diff regions before whole large
files. Select only skills required by the affected surface, use compact test
reporters and decision-relevant failure excerpts, and keep complete verbose logs
in task-specific ignored `.codex-*` or OS temporary paths, never as loose files in the
repository root. Before handoff, reconcile every plan checkbox with evidence,
state deferred gates, inspect owned processes and `git status`, and clean task
scratch artifacts. Do not claim completion while promised long work is active
unless the user explicitly accepts a partial handoff. If routine app work exceeds 30 minutes
or reveals a higher change class, report and re-baseline that scope expansion.
The hosted Shinylive/WebAssembly app should be generated from the
bundled app by GitHub Actions, use the repo's wasm CRAN-like package repository
from a hardcoded package version/commit pin plus pinned app dependency closure,
stage only the small medoid/model libraries with guarded `get_lib()` workflows,
and verify startup, assets, dependency resolution, and library matching without
hand-editing generated web artifacts. Keep GitHub's `README.md` free of the
interactive embed. Maintain the pure HTML/CSS/JS landing and app shell in
`site/` at `/`, keep its iframe on relative `app/`, and build conventional
README-driven pkgdown docs at `/pkgdown/`. Treat
`Moore-Institute-4-Plastic-Pollution-Res/openspecy` as the hosting bridge and
keep workflows fork-portable while this package remains canonical. For
hosted UI work, preserve upload/download app mode with page-owned viewport
state, debounce brief Shiny busy transitions, and run the action-equivalent
preflight plus nested-frame browser smoke when a matching wasm artifact exists.
Every plan must classify hosted impact. Changes to `R/`, `DESCRIPTION`,
`inst/shiny/`, `site/`, README/pkgdown inputs, `tools/wasm/`, or deployment
workflows must run the fast `-HostedAppStatic` gate; add an exact-artifact
preflight for hosted runtime/route/interaction/assembly changes and reserve the
full clean-commit wasm rebuild for dependency/image/driver/pin or release-facing
changes. Hosted smoke fixtures must explicitly set output-determining controls
instead of relying on incidental defaults.
Remote synchronization is maintainer-owned by default: do not run `git push`,
`git pull`, or `git pull --rebase` unless the user explicitly authorizes that
specific operation in the current request. Earlier permission does not carry
forward to later operations.
<!-- SPECKIT END -->
