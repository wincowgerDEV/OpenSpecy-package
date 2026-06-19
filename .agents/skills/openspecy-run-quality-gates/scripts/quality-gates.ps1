param(
  [string]$Filter,
  [string[]]$Benchmark = @(),
  [switch]$Document,
  [switch]$FullTests,
  [switch]$Check
)

$ErrorActionPreference = "Stop"
$repo = (Resolve-Path (Join-Path $PSScriptRoot "..\..\..\..")).Path
$rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe"

if (-not (Test-Path -LiteralPath $rscript)) {
  $rscript = Get-ChildItem -Path "C:\Program Files\R" -Recurse `
    -Filter Rscript.exe -ErrorAction SilentlyContinue |
    Sort-Object FullName -Descending |
    Select-Object -First 1 -ExpandProperty FullName
}
if (-not $rscript -or -not (Test-Path -LiteralPath $rscript)) {
  throw "No installed Rscript.exe was found."
}

function Invoke-RExpression {
  param([string]$Expression)
  & $rscript -e $Expression
  if ($LASTEXITCODE -ne 0) {
    throw "R verification command failed with exit code $LASTEXITCODE."
  }
}

Push-Location $repo
try {
  git status --short

  if ($Filter) {
    $env:OPENSPECY_TEST_FILTER = $Filter
    Invoke-RExpression "devtools::test(filter = Sys.getenv('OPENSPECY_TEST_FILTER'), reporter = 'summary')"
  }

  foreach ($item in $Benchmark) {
    $path = (Resolve-Path -LiteralPath $item).Path
    & $rscript $path
    if ($LASTEXITCODE -ne 0) {
      throw "Benchmark failed: $item"
    }
  }

  if ($Document) {
    Invoke-RExpression @'
required <- desc::desc_get_field("Config/roxygen2/version", default = NA_character_)
installed <- as.character(utils::packageVersion('roxygen2'))
if (!is.na(required) && !identical(required, installed)) {
  stop('roxygen2 version mismatch: DESCRIPTION requires ', required,
       ' but ', installed, ' is installed')
}
'@
    Invoke-RExpression "devtools::document()"
    git diff -- NAMESPACE man DESCRIPTION
  }

  if ($FullTests) {
    Invoke-RExpression "devtools::test(reporter = 'summary')"
  }

  if ($Check) {
    Invoke-RExpression "devtools::check(document = FALSE, cran = FALSE, error_on = 'never')"
  }

  git diff --check
}
finally {
  Remove-Item Env:OPENSPECY_TEST_FILTER -ErrorAction SilentlyContinue
  Pop-Location
}
