[CmdletBinding()]
param(
  [string]$WorkDir = "_wasm/pages-shell",
  [string]$NodeDir = "_wasm/pages-shell-tools/node",
  [string]$Rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe",
  [int]$Port = 8088,
  [switch]$Bootstrap
)

$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "../..")).Path
. (Join-Path $PSScriptRoot "workspace-path.ps1")

function Invoke-Checked([string]$File, [string[]]$Arguments) {
  & $File @Arguments
  if ($LASTEXITCODE -ne 0) {
    throw "Command failed ($LASTEXITCODE): $File $($Arguments -join ' ')"
  }
}

function Get-RepoRelative([string]$Path) {
  $rootUri = [Uri]($repoRoot.TrimEnd([IO.Path]::DirectorySeparatorChar) + "/")
  $pathUri = [Uri][IO.Path]::GetFullPath($Path)
  [Uri]::UnescapeDataString($rootUri.MakeRelativeUri($pathUri).ToString()).
    Replace("/", [IO.Path]::DirectorySeparatorChar)
}

if (-not (Test-Path -LiteralPath $Rscript -PathType Leaf)) {
  throw "Rscript was not found at $Rscript"
}
$rExe = Join-Path (Split-Path $Rscript) "R.exe"
if (-not (Test-Path -LiteralPath $rExe -PathType Leaf)) {
  throw "R.exe was not found beside $Rscript"
}

$work = Resolve-OpenSpecyScratchPath -RepoRoot $repoRoot -Path $WorkDir
$node = Resolve-OpenSpecyScratchPath -RepoRoot $repoRoot -Path $NodeDir
$siteRoot = Join-Path $work "site"
$pkgdownRoot = Join-Path $siteRoot "pkgdown"
$appRoot = Join-Path $siteRoot "app"
$hostLibrary = Join-Path $work "r-library"
$playwright = Join-Path $node "node_modules/.bin/playwright.cmd"

Push-Location $repoRoot
try {
  if (Test-Path -LiteralPath $work) {
    Remove-Item -LiteralPath $work -Recurse -Force
  }
  New-Item -ItemType Directory -Path $pkgdownRoot -Force | Out-Null
  New-Item -ItemType Directory -Path $hostLibrary -Force | Out-Null

  Invoke-Checked $rExe @(
    "CMD", "INSTALL", "--no-multiarch", "--with-keep.source",
    "-l", $hostLibrary, "."
  )

  $pkgdownRelative = (Get-RepoRelative $pkgdownRoot).Replace("\", "/")
  $hostLibraryR = $hostLibrary.Replace("\", "/")
  Invoke-Checked $Rscript @(
    "-e",
    ".libPaths(c('$hostLibraryR', .libPaths())); options(pkgdown.internet=FALSE); pkgdown::build_site_github_pages(new_process=FALSE, install=FALSE, dest_dir='$pkgdownRelative')"
  )
  Invoke-Checked $Rscript @(
    "tools/wasm/stage-pages-shell.R",
    (Get-RepoRelative $siteRoot)
  )

  New-Item -ItemType Directory -Path $appRoot -Force | Out-Null
  [IO.File]::WriteAllText(
    (Join-Path $appRoot "index.html"),
    '<!doctype html><html><body data-openspecy-app-placeholder><p>Shell-only app route placeholder.</p></body></html>' + [Environment]::NewLine
  )
  Invoke-Checked $Rscript @(
    "tools/wasm/check-pages-site.R",
    (Get-RepoRelative $siteRoot),
    "--shell-only"
  )

  if ($Bootstrap) {
    New-Item -ItemType Directory -Path $node -Force | Out-Null
    Invoke-Checked "npm.cmd" @(
      "install", "--prefix", $node, "--no-save", "--package-lock=false",
      "@playwright/test@1.61.1", "http-server@14.1.1"
    )
    Invoke-Checked $playwright @("install", "chromium")
  } elseif (-not (Test-Path -LiteralPath $playwright -PathType Leaf)) {
    throw "Pinned browser tools are missing under $node; rerun with -Bootstrap or pass a prepared -NodeDir."
  }

  if (Get-NetTCPConnection -LocalPort $Port -State Listen -ErrorAction SilentlyContinue) {
    throw "Port $Port is already in use."
  }
  $httpServer = Join-Path $node "node_modules/http-server/bin/http-server"
  if (-not (Test-Path -LiteralPath $httpServer -PathType Leaf)) {
    throw "Pinned http-server is missing under $node."
  }

  $screenshotDir = Join-Path $work "screenshots"
  New-Item -ItemType Directory -Path $screenshotDir -Force | Out-Null
  $oldNodePath = $env:NODE_PATH
  $oldSmokeUrl = $env:PAGES_SHELL_SMOKE_URL
  $oldScreenshotDir = $env:PAGES_SHELL_SCREENSHOT_DIR
  $env:NODE_PATH = Join-Path $node "node_modules"
  $env:PAGES_SHELL_SMOKE_URL = "http://127.0.0.1:$Port/site/"
  $env:PAGES_SHELL_SCREENSHOT_DIR = $screenshotDir
  $server = $null
  try {
    $server = Start-Process -FilePath "node.exe" -ArgumentList @(
      $httpServer, $work, "-p", $Port
    ) -WindowStyle Hidden -PassThru
    Start-Sleep -Seconds 2
    Invoke-Checked $playwright @(
      "test", "tools/wasm/landing-smoke.spec.js",
      "--output", (Get-RepoRelative (Join-Path $work "playwright-results"))
    )
  } finally {
    if ($server) {
      Stop-Process -Id $server.Id -Force -ErrorAction SilentlyContinue
    }
    $env:NODE_PATH = $oldNodePath
    $env:PAGES_SHELL_SMOKE_URL = $oldSmokeUrl
    $env:PAGES_SHELL_SCREENSHOT_DIR = $oldScreenshotDir
  }

  $report = [ordered]@{
    status = "passed"
    action_equivalent = $false
    current_source_native_install = "passed"
    wasm_build = "not_run"
    hosted_webr_browser = "not_run"
    landing_shell_browser = "passed"
    base_path = "/site/"
    completed_at = [DateTime]::UtcNow.ToString("o")
  }
  $reportPath = Join-Path $work "pages-shell-report.json"
  [IO.File]::WriteAllText(
    $reportPath,
    ($report | ConvertTo-Json -Depth 3) + [Environment]::NewLine
  )
  Write-Host "NON-ACTION-EQUIVALENT Pages shell test passed."
  Write-Host "No wasm package was built and no hosted WebR/Shiny workflow ran."
  Write-Host "Evidence: $reportPath"
} finally {
  Pop-Location
}
