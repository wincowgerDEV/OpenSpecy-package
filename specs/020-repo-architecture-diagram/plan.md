# Feature Plan: Repo Architecture Diagram

**Feature dir**: `specs/020-repo-architecture-diagram`
**Date**: 2026-08-27
**Review budget**: Under 100 nonblank lines and about 1,500 words.
**Current tranche**: Add one new, clickable folder-architecture diagram covering the whole repository (governance, R package, bundled app, hosted deployment, verification, docs) next to `pipeline-diagram.html`, and link it from `README.md`; no existing generated, app, or package-behavior file changes.
**Change class**: presentation-only (a `.specify/memory/` documentation artifact plus a README link; no `site/`, package, scientific, or app-behavior change)

## Goal

- Give contributors and reviewers one visual map of how top-level folders relate (governance -> R package -> bundled Shiny app -> hosted Shinylive site -> verification/docs), reachable from `README.md`.
- Each folder box is a real, keyboard-accessible link to that folder's GitHub tree, so the diagram doubles as repo navigation.

## Scope

- **In**: one new standalone HTML diagram page beside `pipeline-diagram.html`; a new `README.md` link to a rendered view of it (implementation-time edit, see Note below); optional short mention in `NEWS.md`.
- **Out**: editing `.specify/memory/pipeline-diagram.html` (unrelated, app-pipeline scope per Principle X); any change to `R/`, `inst/shiny/`, `tests/`, workflow YAML, or generated docs; exhaustive coverage of every repo folder (`data/`, `packages/`, `test-results/`, `pkgdown/`, `.codex-*` scratch dirs are intentionally omitted as non-architectural).
- **Users**: contributors and reviewers browsing GitHub or the hosted site who want a folder-level mental model before editing.
- **Concurrency note**: another agent is editing this repo now. This plan only creates new files (this `plan.md` and the diagram HTML). It intentionally defers the routine `AGENTS.md` active-plan pointer update and `.specify/feature.json` update (both existing tracked files) to implementation time, once no concurrent edit is in flight.

## Requirements

- R1. Add `.specify/memory/repo-architecture-diagram.html`, a self-contained, theme-aware (light/dark) HTML/CSS/SVG page with no build step, reusing `.specify/memory/pipeline-diagram.html`'s shared visual tokens (IBM Plex fonts, base CSS custom properties, legend, pan/zoom viewer) with its own legend/category colors and content, adapted to folder boxes instead of pipeline stages.
- R2. Represent these nodes, grouped by tier, each as one clickable box:
  - Governance: `.agents/`, `.claude/`, `.github/`, `.specify/`
  - R package core: `R/`, `man/`, `tests/`, `inst/` (with `inst/shiny/` nested inside `inst/`)
  - Verification/support: `workflows/`, `benchmarks/`
  - Documentation: `vignettes/`, `docs/`
  - Hosted deployment: `tools/wasm/`, `site/`
- R3. Represent these relationships as labeled edges (grouped/summarized, not one edge per governance folder, to stay legible): governance folders set rules for the rest of the repo; `R/`, `man/`, `tests/`, `inst/` compose the R package; `inst/` contains `inst/shiny/`; `inst/shiny/` plus `tools/wasm/` (driven by `.github/workflows/deploy-shinylive.yml`) generate the hosted Shinylive app served under `site/app/`; `workflows/` and `benchmarks/` support/validate `R/` outside the CRAN test surface; `vignettes/` and `docs/` document both `R/` and `inst/shiny/`; `site/` is published to GitHub Pages by `.github/workflows/`.
- R4. Every box is a real `<a href="https://github.com/wincowgerDEV/OpenSpecy-package/tree/main/<path>">` (not `onclick`/JS-only), opening in a new tab with `rel="noopener"`, with visible focus style and an accessible name (folder path + one-line purpose) so it works with JS disabled and is keyboard-navigable.
- R5. The page must render correctly as a standalone file (`file://`, a raw-HTML preview proxy, or GitHub Pages) without any repo-internal asset dependency beyond the existing Google Fonts pattern already used by `pipeline-diagram.html`.
- R6. `README.md` gains one short link (implementation time) to a rendered view of the diagram, consistent with the existing "Use OpenSpecy online" pattern; README stays iframe-free per Principle V/Hosted Boundary.

## Technical Decisions

- **Location**: `.specify/memory/repo-architecture-diagram.html`, alongside `pipeline-diagram.html`, per explicit user direction — this is a durable governance/reference artifact, not a `site/` deployment asset, so it carries none of the hosted-app boundary's obligations.
- **README rendering without touching `site/`**: GitHub's blob view does not execute HTML/JS for files under `.specify/`, so the README link (implementation time) should point to a raw-HTML preview proxy (e.g. `https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/.specify/memory/repo-architecture-diagram.html`) rather than routing this through `site/`/GitHub Pages. This keeps the change entirely presentation-only with zero hosted-app impact, at the cost of depending on a third-party raw-file CDN for the rendered preview (the file itself stays fully in-repo and self-contained either way).
- **Link targets**: `https://github.com/<owner>/<repo>/tree/main/<path>`, owner/repo taken from the existing README badge URLs (`wincowgerDEV/OpenSpecy-package`); the one external node (published site) links to `https://wincowgerdev.github.io/OpenSpecy-package/`.
- **Visual reuse, not shared code**: copy the proven theme-token/legend/pan-zoom pattern from `pipeline-diagram.html` for consistency (same fonts/background/shadow/tip styling); give this diagram its own legend and category colors since its 5 folder tiers (governance/package/support/docs/hosted) are a different taxonomy than the pipeline's input/function/decision/trigger/output roles. Do not refactor into a shared include: the two diagrams have unrelated content lifecycles, and only the pipeline diagram is bound by Principle X.
- **Generated artifacts**: N/A, hand-authored static HTML, no roxygen/NAMESPACE/man involvement.
- **OpenSpecy contract / bundled Shiny app / pipeline diagram**: N/A, no `R/` or `inst/shiny/` behavior touched; `pipeline-diagram.html` itself is unmodified (new sibling file only).
- **Hosted Shinylive/WebAssembly app**: N/A. Nothing under `site/`, `inst/shiny/`, `tools/wasm/`, or `.github/workflows/` changes in this tranche, so no hosted gate is triggered.

## Package Surfaces

- `R/`, `tests/testthat/`, `benchmarks/`, `DESCRIPTION`: unchanged.
- `workflows/`, `.github/workflows/`: unchanged.
- `inst/`: unchanged.
- `site/`: unchanged (diagram intentionally lives in `.specify/memory/`, not `site/`; no hosted-app surface touched).
- `README.md`/pkgdown: README gets one new link at implementation time (deferred, existing tracked file); pkgdown untouched.
- `NEWS.md`: one short entry at implementation time noting the new architecture diagram.
- Bundled Shiny app: N/A. Hosted Shinylive/WebAssembly app: N/A, nothing hosted-related changes.

## Work Checklist

- [x] Author `.specify/memory/repo-architecture-diagram.html` (self-contained SVG/CSS diagram, nodes/edges per R2-R4).
- [ ] Once the concurrent agent's work has landed: add the README link (R6, raw-HTML preview proxy URL) and a `NEWS.md` line; then update `.specify/feature.json` and the `AGENTS.md` active-plan pointer to this feature dir if still the active plan.
- [ ] Validate the page locally by opening `.specify/memory/repo-architecture-diagram.html` directly in a browser: every box link resolves to the correct GitHub folder or external URL, light/dark themes both render, keyboard tab order reaches every box.

## Verification

- Direct acceptance check: open `.specify/memory/repo-architecture-diagram.html` in a browser (light and dark OS theme) and click every box; confirm the destination URL matches the folder/site it names.
- Focused checks: N/A beyond manual browser open (no build/staging step for a `.specify/memory/` file).
- Toolchain/version preflight: N/A (no roxygen/package regeneration).
- `devtools::document()` / full `devtools::test()` / `devtools::check()`: N/A, no `R/`/`NAMESPACE` change.
- Benchmarks / reference-library staging: N/A.
- Shiny affected states: N/A.
- Shinylive/WebAssembly impact: N/A, no hosted input changed.
- Reusable evidence: none yet; this is a new file with no prior passing gate to reuse.

## Risks And Open Questions

- README/`.specify/feature.json`/`AGENTS.md` pointer updates are intentionally deferred past this plan's creation because another agent is concurrently editing tracked files; implementation must re-check for conflicts before those specific edits.
- Node/edge set favors legibility over completeness (`data/`, `pkgdown/`, `packages/`, `test-results/` omitted); revisit only if a reviewer wants full coverage.
- The README link depends on a third-party raw-HTML preview proxy (e.g. `raw.githack.com`) to render the file's JS/SVG, since GitHub's own blob view only shows source for `.specify/`-hosted HTML; confirm that proxy choice (or an accepted alternative) before adding the README link.

## Approval Notes

- Diagram authored and verified headlessly with the repo's existing Playwright install (`_wasm/mount-prototype-tools/node_modules/playwright`): all 16 boxes resolve to the intended GitHub-tree or external URL, no console/page errors, keyboard Tab reaches link nodes, tooltips render correctly (including `tip-left`/`tip-above` edge cases). Light and dark theme screenshots reviewed directly. One real bug found and fixed this way: the solid `inst/shiny/` -> `tools/wasm/` arrow originally cut through the `vignettes/`/`docs/` boxes; rerouted around them. Ad hoc screenshot scripts used for this check were not committed (`_wasm/` is gitignored) and were deleted after use.
- Approved by:
- Follow-up: land the deferred README/`NEWS.md`/pointer edits once the concurrent agent's changes are merged.
