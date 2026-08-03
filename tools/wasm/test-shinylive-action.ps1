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
  [string]$NodeDir = "",
  [string]$Rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe",
  [int]$Port = 8087,
  [switch]$StageLibraries,
  [switch]$Bootstrap
)

$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path ".").Path
. (Join-Path $PSScriptRoot "workspace-path.ps1")

function Resolve-ScratchPath([string]$Path) {
  Resolve-OpenSpecyScratchPath -RepoRoot $repoRoot -Path $Path
}

function Invoke-Checked([string]$File, [string[]]$Arguments) {
  & $File @Arguments
  if ($LASTEXITCODE -ne 0) {
    throw "Command failed ($LASTEXITCODE): $File $($Arguments -join ' ')"
  }
}

function Get-RepoRelative([string]$Path) {
  Get-OpenSpecyRepoRelativePath -RepoRoot $repoRoot -Path $Path
}

function Assert-Equal([string]$Actual, [string]$Expected, [string]$Label) {
  if (-not [string]::Equals($Actual, $Expected,
      [StringComparison]::Ordinal)) {
    throw "$Label mismatch: found '$Actual'; expected '$Expected'."
  }
}

if (-not (Test-Path -LiteralPath $Rscript -PathType Leaf)) {
  throw "Rscript was not found at $Rscript"
}

$rExe = Join-Path (Split-Path $Rscript) "R.exe"
$work = Resolve-ScratchPath $WorkDir
$tools = Resolve-ScratchPath $ToolDir
$nodeDir = if ($NodeDir) {
  Resolve-ScratchPath $NodeDir
} else {
  Join-Path $tools "node"
}
$separator = [IO.Path]::DirectorySeparatorChar
$workPrefix = $work.TrimEnd($separator) + $separator
$toolsPrefix = $tools.TrimEnd($separator) + $separator
if ($work -eq $tools -or
    $work.StartsWith($toolsPrefix, [StringComparison]::OrdinalIgnoreCase) -or
    $tools.StartsWith($workPrefix, [StringComparison]::OrdinalIgnoreCase)) {
  throw "WorkDir and ToolDir must be separate, non-overlapping directories."
}
if ($nodeDir -eq $work -or
    $nodeDir.StartsWith($workPrefix, [StringComparison]::OrdinalIgnoreCase)) {
  throw "NodeDir must not overlap WorkDir."
}

$headSha = (& git rev-parse HEAD).Trim()
Assert-Equal $headSha $PackageSha "Checked-out Git commit"
$artifactInputPatterns = @(
  '^(DESCRIPTION|NAMESPACE)$',
  '^(R|data|inst|src)/',
  '^(configure|configure\.win|cleanup|cleanup\.win)$'
)
$dirtyArtifactInputs = @(& git status --porcelain=v1 --untracked-files=all) |
  ForEach-Object {
    $path = $_.Substring(3).Trim('"')
    if ($path.Contains(' -> ')) { $path = $path.Split(' -> ')[-1] }
    $path.Replace('\', '/')
  } | Where-Object {
    $candidate = $_
    $artifactInputPatterns | Where-Object { $candidate -match $_ }
  }
if ($dirtyArtifactInputs) {
  throw "The artifact cannot verify dirty package/app inputs: $($dirtyArtifactInputs -join ', '). Commit them and build a fresh matching artifact first."
}

if (Test-Path -LiteralPath $work) {
  Remove-Item -LiteralPath $work -Recurse -Force
}
if ($Bootstrap -and (Test-Path -LiteralPath $tools)) {
  Remove-Item -LiteralPath $tools -Recurse -Force
}
$nodeInsideTools = $nodeDir -eq $tools -or
  $nodeDir.StartsWith($toolsPrefix, [StringComparison]::OrdinalIgnoreCase)
if ($Bootstrap -and -not $nodeInsideTools -and
    (Test-Path -LiteralPath $nodeDir)) {
  Remove-Item -LiteralPath $nodeDir -Recurse -Force
}
New-Item -ItemType Directory -Path $work | Out-Null
New-Item -ItemType Directory -Force -Path $tools | Out-Null

if ($Bootstrap) {
  # pak/pkgcache is otherwise shared across R versions and runs. A stale
  # Windows binary cached under a source-package key can make a clean
  # rehearsal fail while unpacking (for example, raw ZIP bytes presented as a
  # tarball). Keep bootstrap downloads inside the freshly recreated tool tree.
  $env:R_PKG_CACHE_DIR = Join-Path $tools "pkg-cache"
  $packageRoots = Get-Content -LiteralPath `
    "inst/shiny/wasm/app-package-roots.txt" |
    ForEach-Object { ($_ -replace '#.*$', '').Trim() } |
    Where-Object { $_ }
  $bootstrapRefs = @($packageRoots) + @(
    "jsonlite", "pkgdown", "shinylive@0.5.0", "httpuv"
  ) | Select-Object -Unique
  $rRefs = ($bootstrapRefs | ForEach-Object {
    "'" + $_.Replace("'", "\\'") + "'"
  }) -join ","
  Invoke-Checked $Rscript @(
    "-e",
    "pak::pkg_install(c($rRefs), lib='$($tools.Replace('\', '/'))', upgrade=FALSE)"
  )
}

# Deliberately exclude the developer's user library. Otherwise a locally
# installed pkgdown/app dependency can conceal a missing deployment package.
$env:R_LIBS_USER = $tools
$requiredHostPackages = @("jsonlite", "pkgdown", "shinylive", "httpuv")
$missingHostPackages = & $Rscript -e (
  "cat(setdiff(c(" + (($requiredHostPackages | ForEach-Object {
    "'$_'"
  }) -join ",") + "), rownames(installed.packages())), sep='\n')"
)
if ($missingHostPackages) {
  throw "Isolated host library is missing: $($missingHostPackages -join ', '). Rerun with -Bootstrap."
}

$shinyliveVersion = & $Rscript -e `
  "cat(if (requireNamespace('shinylive', quietly=TRUE)) as.character(packageVersion('shinylive')) else '')"
if ($shinyliveVersion -ne "0.5.0") {
  throw "Shinylive 0.5.0 is required in the isolated library $tools."
}

Invoke-Checked $rExe @(
  "CMD", "INSTALL", "--no-multiarch", "--no-lock", "--with-keep.source",
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
  "repo/bin/emscripten",
  "metadata/wasm-app-manifest.json",
  "metadata/resolved-wasm-packages.json"
)) {
  if (-not (Test-Path -LiteralPath (Join-Path $pinned $relative))) {
    throw "Pinned artifact is missing $relative"
  }
}

Invoke-Checked $Rscript @(
  "tools/wasm/check-wasm-artifact.R",
  (Get-RepoRelative $pinned),
  $PackageSha
)

$oldGitHubSha = $env:GITHUB_SHA
$env:GITHUB_SHA = $PackageSha
$verifiedWasmManifest = Join-Path $work "verified-wasm-packages.json"
try {
  Invoke-Checked $Rscript @(
    "tools/wasm/check-wasm-repo.R",
    (Get-RepoRelative (Join-Path $pinned "repo")),
    (Get-RepoRelative (Join-Path $pinned "image")),
    (Get-RepoRelative $verifiedWasmManifest)
  )
} finally {
  $env:GITHUB_SHA = $oldGitHubSha
}
Invoke-Checked $Rscript @(
  "tools/wasm/check-wasm-artifact.R",
  (Get-RepoRelative $pinned),
  $PackageSha,
  (Get-RepoRelative $verifiedWasmManifest)
)

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
$pkgdownSite = Join-Path $siteRoot "pkgdown"
$site = Join-Path $siteRoot "app"
$workApp = Join-Path $work "app-source"
$appManifest = Join-Path $work "wasm-app-manifest.json"
$artifactRef = "openspecy-wasm-$PackageSha"
Invoke-Checked $Rscript @(
  "-e",
  "options(pkgdown.internet=FALSE); pkgdown::build_site_github_pages(new_process=FALSE, install=FALSE, dest_dir='$((Get-RepoRelative $pkgdownSite).Replace('\', '/'))')"
)
Invoke-Checked $Rscript @(
  "tools/wasm/stage-pages-shell.R",
  (Get-RepoRelative $siteRoot)
)
Invoke-Checked $Rscript @(
  "tools/wasm/prepare-shinylive-app.R",
  "--artifact-ref", $artifactRef,
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
Invoke-Checked $Rscript @(
  "tools/wasm/check-pages-site.R",
  (Get-RepoRelative $siteRoot)
)

$playwright = Join-Path $nodeDir "node_modules/.bin/playwright.cmd"
if ($Bootstrap) {
  New-Item -ItemType Directory -Force -Path $nodeDir | Out-Null
  Invoke-Checked "npm.cmd" @(
    "install", "--prefix", $nodeDir, "--no-save", "--package-lock=false",
    "@playwright/test@1.61.1", "http-server@14.1.1"
  )
  Invoke-Checked $playwright @("install", "chromium")
} elseif (-not (Test-Path -LiteralPath $playwright)) {
  throw "Pinned Node smoke tools are missing under $nodeDir; rerun with -Bootstrap."
}
$playwrightManifest = Join-Path $nodeDir `
  "node_modules/@playwright/test/package.json"
$httpManifest = Join-Path $nodeDir "node_modules/http-server/package.json"
if (-not (Test-Path -LiteralPath $playwrightManifest) -or
    -not (Test-Path -LiteralPath $httpManifest)) {
  throw "Pinned Node package manifests are missing under $nodeDir."
}
$playwrightVersion = (Get-Content -Raw -LiteralPath $playwrightManifest |
  ConvertFrom-Json).version
$httpVersion = (Get-Content -Raw -LiteralPath $httpManifest |
  ConvertFrom-Json).version
Assert-Equal $playwrightVersion "1.61.1" "Playwright version"
Assert-Equal $httpVersion "14.1.1" "http-server version"

if (Get-NetTCPConnection -LocalPort $Port -State Listen -ErrorAction SilentlyContinue) {
  throw "Port $Port is already in use."
}

$env:NODE_PATH = Join-Path $nodeDir "node_modules"
$env:SHINYLIVE_SMOKE_URL = "http://127.0.0.1:$Port/"
$env:OPENSPECY_EXPECTED_VERSION =
  (& $Rscript -e "cat(read.dcf('DESCRIPTION')[1, 'Version'])")
$server = Start-Process -FilePath "node.exe" -ArgumentList @(
  (Join-Path $nodeDir "node_modules/http-server/bin/http-server"),
  $siteRoot, "-p", $Port
) -WindowStyle Hidden -PassThru

try {
  Start-Sleep -Seconds 2
  Invoke-Checked $playwright @(
    "test", "tools/wasm/shinylive-smoke.spec.js",
    "--output", (Get-RepoRelative (Join-Path $work "playwright-results"))
  )
} finally {
  Stop-Process -Id $server.Id -Force -ErrorAction SilentlyContinue
}

Write-Host "Shinylive action preflight passed for $PackageSha."
