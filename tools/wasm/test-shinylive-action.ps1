[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)]
  [string]$Artifact,

  [Parameter(Mandatory = $true)]
  [ValidatePattern('^[0-9a-fA-F]{40}$')]
  [string]$PackageSha,

  [string]$Libraries = "_wasm/libraries",
  [string]$WorkDir = "_wasm/shinylive-preflight",
  [string]$ToolDir = "_wasm/shinylive-preflight-tools",
  [string]$Rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe",
  [int]$Port = 8087,
  [switch]$StageLibraries,
  [switch]$Bootstrap
)

$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path ".").Path

function Resolve-WorkspacePath([string]$Path) {
  $resolved = [IO.Path]::GetFullPath((Join-Path $repoRoot $Path))
  $prefix = $repoRoot.TrimEnd([IO.Path]::DirectorySeparatorChar) +
    [IO.Path]::DirectorySeparatorChar
  if (-not $resolved.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase)) {
    throw "Path must stay inside the repository: $Path"
  }
  $resolved
}

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
$work = Resolve-WorkspacePath $WorkDir
$tools = Resolve-WorkspacePath $ToolDir
$defaultLibrary = & $Rscript -e "cat(.libPaths()[1])"

if (Test-Path -LiteralPath $work) {
  Remove-Item -LiteralPath $work -Recurse -Force
}
New-Item -ItemType Directory -Path $work | Out-Null
New-Item -ItemType Directory -Force -Path $tools | Out-Null
$env:R_LIBS_USER = "$tools;$defaultLibrary"

$shinyliveVersion = & $Rscript -e `
  "cat(if (requireNamespace('shinylive', quietly=TRUE)) as.character(packageVersion('shinylive')) else '')"
if ($shinyliveVersion -ne "0.5.0") {
  if (-not $Bootstrap) {
    throw "Shinylive 0.5.0 is required in $tools; rerun with -Bootstrap."
  }
  Invoke-Checked $Rscript @(
    "-e",
    "pak::pkg_install('shinylive@0.5.0', lib='$($tools.Replace('\', '/'))', upgrade=FALSE)"
  )
}

Invoke-Checked $rExe @(
  "CMD", "INSTALL", "--no-multiarch", "--with-keep.source",
  "-l", $tools, "."
)

$artifactPath = (Resolve-Path -LiteralPath $Artifact).Path
if ((Get-Item -LiteralPath $artifactPath) -is [IO.DirectoryInfo]) {
  $pinned = $artifactPath
} else {
  $pinned = Join-Path $work "pinned"
  Expand-Archive -LiteralPath $artifactPath -DestinationPath $pinned -Force
}

foreach ($relative in @(
  "image/library.data.gz",
  "image/library.js.metadata",
  "repo/bin/emscripten"
)) {
  if (-not (Test-Path -LiteralPath (Join-Path $pinned $relative))) {
    throw "Pinned artifact is missing $relative"
  }
}

if ($StageLibraries) {
  $libraryPath = Join-Path $work "libraries"
  Invoke-Checked $Rscript @(
    "tools/wasm/stage-shinylive-libraries.R",
    "--out-dir", $libraryPath,
    "--manifest-out", (Join-Path $work "library-manifest.json")
  )
} else {
  $libraryPath = (Resolve-Path -LiteralPath $Libraries).Path
}
Invoke-Checked $Rscript @(
  "tools/wasm/smoke-staged-libraries.R",
  (Get-RepoRelative $libraryPath)
)

$siteRoot = Join-Path $work "site"
$site = Join-Path $siteRoot "openspecy"
$workApp = Join-Path $work "app-source"
$appManifest = Join-Path $work "wasm-app-manifest.json"
$repoUrl = "https://wincowgerDEV.github.io/OpenSpecy-package/wasm/$PackageSha/repo"
Invoke-Checked $Rscript @(
  "tools/wasm/prepare-shinylive-app.R",
  "--repo-url", $repoUrl,
  "--package-sha", $PackageSha,
  "--library-dir", (Get-RepoRelative $libraryPath),
  "--out-dir", (Get-RepoRelative $site),
  "--work-dir", (Get-RepoRelative $workApp),
  "--manifest-out", (Get-RepoRelative $appManifest)
)
Invoke-Checked $Rscript @(
  "tools/wasm/bundle-wasm-library.R",
  "--image-dir", (Get-RepoRelative (Join-Path $pinned "image")),
  "--repo-dir", (Get-RepoRelative (Join-Path $pinned "repo")),
  "--site-dir", (Get-RepoRelative $site),
  "--package-sha", $PackageSha
)
Invoke-Checked $Rscript @(
  "tools/wasm/check-shinylive-export.R",
  (Get-RepoRelative $site)
)

$nodeDir = Join-Path $tools "node"
$playwright = Join-Path $nodeDir "node_modules/.bin/playwright.cmd"
if (-not (Test-Path -LiteralPath $playwright)) {
  if (-not $Bootstrap) {
    throw "Pinned Node smoke tools are missing under $nodeDir; rerun with -Bootstrap."
  }
  Invoke-Checked "npm.cmd" @(
    "install", "--prefix", $nodeDir, "--no-save",
    "@playwright/test@1.61.1", "http-server@14.1.1"
  )
  Invoke-Checked $playwright @("install", "chromium")
}

if (Get-NetTCPConnection -LocalPort $Port -State Listen -ErrorAction SilentlyContinue) {
  throw "Port $Port is already in use."
}

$env:NODE_PATH = Join-Path $nodeDir "node_modules"
$env:SHINYLIVE_SMOKE_URL = "http://127.0.0.1:$Port/openspecy/"
$env:OPENSPECY_EXPECTED_VERSION =
  (& $Rscript -e "cat(read.dcf('DESCRIPTION')[1, 'Version'])")
$server = Start-Process -FilePath "node.exe" -ArgumentList @(
  (Join-Path $nodeDir "node_modules/http-server/bin/http-server"),
  $siteRoot, "-p", $Port
) -WindowStyle Hidden -PassThru

try {
  Start-Sleep -Seconds 2
  Invoke-Checked $playwright @(
    "test", "tools/wasm/shinylive-smoke.spec.js"
  )
} finally {
  Stop-Process -Id $server.Id -Force -ErrorAction SilentlyContinue
}

Write-Host "Shinylive action preflight passed for $PackageSha."
