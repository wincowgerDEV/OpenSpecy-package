<!-- SPECKIT START -->
For additional context about technologies to be used, project structure,
shell commands, and other important information, read the current plan
`specs/001-masked-circular-ae/plan.md` and
`.specify/memory/constitution.md`. For this R package, do not edit generated
`NAMESPACE`, `man/*.Rd`, or pkgdown HTML directly; update roxygen or package
metadata and regenerate with the appropriate R tooling. Center `OpenSpecy`
object structure (`wavenumber`, `spectra`, `metadata`) and object attributes in
function flows and examples. For same-output function improvements, keep old
comparison code in `benchmarks/`, add or update a benchmark, and keep `tests/`
focused on current package behavior. Keep long-running tests manual or GitHub
Actions guarded. The Shiny application lives in `wincowgerDEV/OpenSpecy-shiny`;
consider compatibility when relevant, but package functionality takes
precedence and Shiny application code does not belong in this repository.
<!-- SPECKIT END -->

## Local Windows R Tooling

`Rscript.exe` may not be on `PATH` in this workspace shell. Before running R
commands, use the known local install path or look it up under `C:\Program
Files\R`:

- Known working path: `C:\Program Files\R\R-4.3.3\bin\Rscript.exe`
- Lookup command: `Get-ChildItem -Path 'C:\Program Files\R' -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue | Select-Object -First 5 -ExpandProperty FullName`
