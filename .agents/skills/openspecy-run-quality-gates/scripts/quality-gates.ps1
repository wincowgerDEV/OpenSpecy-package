param(
  [string]$Filter,
  [string[]]$Benchmark = @(),
  [switch]$Document,
  [switch]$BundledAppStatic,
  [switch]$BundledAppBrowser,
  [switch]$HostedAppStatic,
  [string]$BrowserGrep,
  [switch]$FullTests,
  [switch]$Check,
  [switch]$CheckPrepareOnly,
  [string]$CheckIncludeUntracked = "",
  [string]$CheckExcludeUntracked = ""
)

$ErrorActionPreference = "Stop"
$repo = (Resolve-Path (Join-Path $PSScriptRoot "..\..\..\..")).Path
$rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe"
$hadNodePath = Test-Path Env:NODE_PATH
$oldNodePath = $env:NODE_PATH

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

  if ($BrowserGrep -and -not $BundledAppBrowser) {
    throw "-BrowserGrep requires -BundledAppBrowser."
  }
  if ($CheckPrepareOnly -and -not $Check) {
    throw "-CheckPrepareOnly requires -Check."
  }
  if (($CheckIncludeUntracked -or $CheckExcludeUntracked) -and -not $Check) {
    throw "Untracked-file check classifications require -Check."
  }

  if ($Filter) {
    $env:OPENSPECY_TEST_FILTER = $Filter
    Invoke-RExpression "devtools::test(filter = Sys.getenv('OPENSPECY_TEST_FILTER'), reporter = 'check', stop_on_failure = TRUE)"
  }

  if ($BundledAppStatic -or $BundledAppBrowser -or $HostedAppStatic) {
    $appSources = @(
      (Resolve-Path "inst/shiny/global.R").Path,
      (Resolve-Path "inst/shiny/ui.R").Path,
      (Resolve-Path "inst/shiny/server.R").Path
    )
    $env:OPENSPECY_APP_SOURCES = $appSources -join [IO.Path]::PathSeparator
    Invoke-RExpression "paths <- strsplit(Sys.getenv('OPENSPECY_APP_SOURCES'), getElement(.Platform, 'path.sep'), fixed = TRUE)[[1L]]; invisible(lapply(paths, parse)); cat('Bundled Shiny sources parse successfully.\n')"

    $smokeRelative = "tools/shiny-local-smoke.spec.js"
    $smoke = (Resolve-Path $smokeRelative).Path
    $node = Get-Command node.exe -ErrorAction SilentlyContinue
    if (-not $node) {
      throw "A real node.exe is required for app browser tests."
    }
    & $node.Source --check $smoke
    if ($LASTEXITCODE -ne 0) {
      throw "The local app browser test has invalid JavaScript."
    }

    $wwwFiles = Get-ChildItem "inst/shiny/www" -Recurse -File
    $appFiles = Get-ChildItem "inst/shiny" -Recurse -File
    Write-Host ("inst/shiny/www: {0} files, {1} bytes" -f `
      $wwwFiles.Count, ($wwwFiles | Measure-Object Length -Sum).Sum)
    Write-Host ("inst/shiny: {0} files, {1} bytes" -f `
      $appFiles.Count, ($appFiles | Measure-Object Length -Sum).Sum)
  }

  if ($HostedAppStatic) {
    if ($Filter -ne "shinylive_wasm") {
      Invoke-RExpression "devtools::test(filter = 'shinylive_wasm', reporter = 'check', stop_on_failure = TRUE)"
    }

    $hostedSmoke = (Resolve-Path "tools/wasm/shinylive-smoke.spec.js").Path
    $node = Get-Command node.exe -ErrorAction SilentlyContinue
    if (-not $node) {
      throw "A real node.exe is required for hosted source checks."
    }
    & $node.Source --check $hostedSmoke
    if ($LASTEXITCODE -ne 0) {
      throw "The hosted Shinylive browser test has invalid JavaScript."
    }

    $wasmRFiles = Get-ChildItem "tools/wasm" -Recurse -File -Filter *.R
    $env:OPENSPECY_WASM_R_SOURCES = `
      ($wasmRFiles.FullName -join [IO.Path]::PathSeparator)
    Invoke-RExpression "paths <- strsplit(Sys.getenv('OPENSPECY_WASM_R_SOURCES'), getElement(.Platform, 'path.sep'), fixed = TRUE)[[1L]]; invisible(lapply(paths, parse)); cat('Hosted wasm R sources parse successfully.\n')"

    $powerShellErrors = @()
    foreach ($source in Get-ChildItem "tools/wasm" -Recurse -File -Filter *.ps1) {
      $tokens = $null
      $errors = $null
      [void][System.Management.Automation.Language.Parser]::ParseFile(
        $source.FullName, [ref]$tokens, [ref]$errors
      )
      foreach ($error in $errors) {
        $powerShellErrors += "{0}:{1}: {2}" -f `
          $source.FullName, $error.Extent.StartLineNumber, $error.Message
      }
    }
    if ($powerShellErrors.Count) {
      throw "Hosted wasm PowerShell parse failures:`n$($powerShellErrors -join [Environment]::NewLine)"
    }
    Write-Host "Hosted workflow contracts and JS/R/PowerShell sources pass fast checks."
  }

  if ($BundledAppBrowser) {
    $nodeModules = @(
      (Join-Path $repo "_wasm\action-sim\node\node_modules"),
      (Join-Path $repo "node_modules")
    ) | Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
    if (-not $nodeModules) {
      throw "Playwright dependencies were not found in the cached or repository node_modules path."
    }
    $playwright = Join-Path $nodeModules ".bin\playwright.cmd"
    if (-not (Test-Path -LiteralPath $playwright)) {
      throw "playwright.cmd was not found under $nodeModules."
    }

    $env:NODE_PATH = $nodeModules
    $playwrightArguments = @("test", $smokeRelative, "--workers=1")
    if ($BrowserGrep) {
      $playwrightArguments += @("--grep", $BrowserGrep)
    }
    & $playwright @playwrightArguments
    if ($LASTEXITCODE -ne 0) {
      throw "The bundled Shiny Playwright smoke test failed."
    }
  }

  foreach ($item in $Benchmark) {
    $path = (Resolve-Path -LiteralPath $item).Path
    & $rscript $path
    if ($LASTEXITCODE -ne 0) {
      throw "Benchmark failed: $item"
    }
  }

  if ($Document) {
    Invoke-RExpression "required <- desc::desc_get_field('Config/roxygen2/version', default = NA_character_); installed <- as.character(utils::packageVersion('roxygen2')); if (!is.na(required) && !identical(required, installed)) stop('roxygen2 version mismatch: DESCRIPTION requires ', required, ' but ', installed, ' is installed')"
    Invoke-RExpression "devtools::document()"
    git diff -- NAMESPACE man DESCRIPTION
  }

  if ($FullTests) {
    Invoke-RExpression "devtools::test(reporter = 'check', stop_on_failure = TRUE)"
  }

  if ($Check) {
    $stagedCheck = Join-Path $PSScriptRoot "staged-package-check.ps1"
    $stagedCheckArguments = @{}
    if ($CheckIncludeUntracked) {
      $stagedCheckArguments.IncludeUntracked = $CheckIncludeUntracked
    }
    if ($CheckExcludeUntracked) {
      $stagedCheckArguments.ExcludeUntracked = $CheckExcludeUntracked
    }
    if ($CheckPrepareOnly) {
      $stagedCheckArguments.PrepareOnly = $true
    }
    & $stagedCheck @stagedCheckArguments
  }

  git diff --check
}
finally {
  Remove-Item Env:OPENSPECY_TEST_FILTER -ErrorAction SilentlyContinue
  Remove-Item Env:OPENSPECY_APP_SOURCES -ErrorAction SilentlyContinue
  Remove-Item Env:OPENSPECY_WASM_R_SOURCES -ErrorAction SilentlyContinue
  if ($hadNodePath) {
    $env:NODE_PATH = $oldNodePath
  } else {
    Remove-Item Env:NODE_PATH -ErrorAction SilentlyContinue
  }
  Pop-Location
}
