---
name: openspecy-verify-hosted-app
description: Reproduce and diagnose OpenSpecy's static landing, pkgdown, and Shinylive deployment locally from an action-built wasm artifact. Use for hosted-app routes, wasm package pin, deployment workflow, or action-equivalent preflight work.
---

# Verify The Hosted OpenSpecy App

Use the maintained preflight instead of rebuilding its steps ad hoc.

## Required Inputs

- A wasm artifact ZIP or extracted directory produced by the package-repository
  action.
- The exact 40-character package commit recorded by that artifact.
- Existing staged medoid/model libraries, or explicit permission to download
  them.

Never silently pair an artifact with a different commit just because the
package version is unchanged.

## Workflow

1. Read `DESCRIPTION`, the active plan,
   `.github/workflows/deploy-cran-repo.yml`,
   `.github/workflows/deploy-shinylive.yml`, and the artifact manifest.
   Confirm workflow artifact lookup uses `github.repository` so the same source
   can run in the Moore Institute hosting fork.
2. If shared `inst/shiny/` source changed, first apply
   `openspecy-develop-shiny-app` and retain its local state-matrix, download,
   console, screenshot, and asset evidence. Hosted preflight is additional.
3. Confirm `git status --short`; preserve user changes and keep all generated
   output under ignored `_wasm/` paths.
4. Choose a fresh `-WorkDir`, a reusable ignored `-ToolDir`/`-NodeDir`, and a
   free port. The script removes `-WorkDir`, so verify that path before running.
5. Run from the repository root:

```powershell
powershell.exe -ExecutionPolicy Bypass -File `
  tools/wasm/test-shinylive-action.ps1 `
  -Artifact <artifact-zip-or-directory> `
  -PackageSha <matching-40-character-sha> `
  -Libraries <staged-library-directory> `
  -WorkDir _wasm/hosted-app-preflight `
  -ToolDir _wasm/hosted-app-tools `
  -NodeDir _wasm/hosted-app-tools/node `
  -Port <free-port>
```

6. Use `-StageLibraries` only when the download path is part of the requested
   test. Use `-Bootstrap` only with permission for network installs.
7. Inspect the generated landing root, `/app/`, `/pkgdown/`, SEO metadata,
   robots/sitemap, manifests, package/image checks, Playwright result, and
   landing/loading/desktop/expanded/pkgdown/mobile screenshots.
8. Run `git status --short` and `git check-ignore` on representative `_wasm/`
   outputs before handoff.

## Failure Triage

Classify the first failure before changing source:

- artifact/SHA mismatch or incomplete image/repository;
- package or dependency closure/index failure;
- staged library name, shape, or matching failure;
- landing, pkgdown, or Shinylive export failure;
- startup/readiness timeout;
- upload, identification, download, console, or visual failure.

Fix the canonical `site/`, package, `inst/shiny/`, README/pkgdown source,
workflow, or wasm tool that owns the failure. Never patch generated site output.

## Handoff Evidence

Report artifact ref and SHA, package version, assembled site size, static check
results, browser workflow result, screenshot review, skipped network stages,
and any remaining public-deployment-only verification. Do not push or pull
unless the current request explicitly authorizes it.
