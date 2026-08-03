[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)]
  [ValidatePattern('^[0-9a-fA-F]{40}$')]
  [string]$PackageSha,

  [string]$OutDir = "_wasm/pinned",
  [string]$Rscript = "Rscript"
)

$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "../..")).Path
. (Join-Path $PSScriptRoot "workspace-path.ps1")
. (Join-Path $PSScriptRoot "docker-preflight.ps1")
Push-Location $repoRoot

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
  (Get-OpenSpecyRepoRelativePath -RepoRoot $repoRoot -Path $Path).
    Replace("\", "/")
}

try {
  try {
    $dockerReadiness = Assert-OpenSpecyDockerEngine
    $docker = $dockerReadiness.DockerCommand
  } catch {
    Write-Host $_.Exception.Message -ForegroundColor Red
    exit 1
  }
  $headSha = (& git rev-parse HEAD).Trim()
  if (-not [string]::Equals(
      $headSha, $PackageSha, [StringComparison]::OrdinalIgnoreCase)) {
    throw "PackageSha $PackageSha does not match checked-out HEAD $headSha."
  }
  $dirty = @(& git status --porcelain=v1 --untracked-files=all)
  if ($dirty) {
    throw "The pinned wasm builder requires a clean committed source tree."
  }
  $rscriptCommand = Get-Command $Rscript -ErrorAction SilentlyContinue
  if (-not $rscriptCommand -and
      (Test-Path -LiteralPath "C:\Program Files\R\R-4.3.3\bin\Rscript.exe")) {
    $Rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe"
  } elseif ($rscriptCommand) {
    $Rscript = $rscriptCommand.Source
  } else {
    throw "Rscript was not found."
  }
  $out = Resolve-ScratchPath $OutDir
  if (Test-Path -LiteralPath $out) {
    Remove-Item -LiteralPath $out -Recurse -Force
  }
  $metadata = Join-Path $out "metadata"
  New-Item -ItemType Directory -Path $metadata -Force | Out-Null

  $resolvedRoots = Join-Path $metadata "resolved-package-roots.txt"
  Invoke-Checked $Rscript @(
    "tools/wasm/resolve-wasm-package-roots.R",
    "inst/shiny/wasm/app-package-roots.txt",
    (Get-RepoRelative $resolvedRoots)
  )
  Invoke-Checked $Rscript @(
    "tools/wasm/write-app-manifest.R",
    "--artifact-ref", "openspecy-wasm-$PackageSha",
    "--package-sha", $PackageSha,
    "--out", (Get-RepoRelative (Join-Path $metadata "wasm-app-manifest.json"))
  )
  Copy-Item -LiteralPath "inst/shiny/wasm/app-package-roots.txt" `
    -Destination (Join-Path $metadata "app-package-roots.txt")
  Copy-Item -LiteralPath "inst/shiny/wasm/library-types.txt" `
    -Destination (Join-Path $metadata "library-types.txt")

  $packages = (Get-Content -LiteralPath $resolvedRoots |
    Where-Object { $_.Trim() }) -join " "
  if (-not $packages) { throw "The resolved wasm package set is empty." }
  # Build local::. from an exact committed snapshot. Mounting the repository
  # itself lets ignored _wasm scratch outputs leak into the package source and
  # makes repeated rehearsals recursively inflate the OpenSpecy archive.
  $sourceArchive = Join-Path $out "candidate-source.zip"
  $sourceSnapshot = Join-Path $out "candidate-source"
  Invoke-Checked "git" @(
    "archive", "--format=zip", "--output=$sourceArchive", $PackageSha
  )
  Expand-Archive -LiteralPath $sourceArchive `
    -DestinationPath $sourceSnapshot -Force
  Remove-Item -LiteralPath $sourceArchive -Force
  $driver = Join-Path $repoRoot "tools/wasm/rwasm-build"
  $webRImage = "ghcr.io/r-wasm/webr@sha256:2bd309d7a4ea1daed82b6fdb8e325b0de715fcd8592c5b6f3b3b88366e70cb76"
  $imageTag = "openspecy-rwasm-prepush:$($PackageSha.Substring(0, 12).ToLowerInvariant())"
  Invoke-Checked $docker @(
    "build",
    "--build-arg", "WEBR_IMAGE=$webRImage",
    "--tag", $imageTag,
    $driver
  )

  $repoPath = Join-Path $out "repo"
  $imagePath = Join-Path $out "image"
  $sourceMount = $sourceSnapshot + ":/github/workspace:ro"
  $outputMount = $out + ":/github/output"
  $strip = "demo,doc,examples,help,html,include,tests,vignette"
  try {
    Invoke-Checked $docker @(
      "run", "--rm",
      "--env", "GITHUB_SHA=$PackageSha",
      "--volume", $sourceMount,
      "--volume", $outputMount,
      "--workdir", "/github/workspace",
      $imageTag,
      "/code.R",
      "/github/output/image",
      "/github/output/repo",
      "true", $packages, $strip
    )
  } finally {
    if (Test-Path -LiteralPath $sourceSnapshot) {
      Remove-Item -LiteralPath $sourceSnapshot -Recurse -Force
    }
  }

  $oldGitHubSha = $env:GITHUB_SHA
  $env:GITHUB_SHA = $PackageSha
  try {
    Invoke-Checked $Rscript @(
      "tools/wasm/check-wasm-repo.R",
      (Get-RepoRelative $repoPath),
      (Get-RepoRelative $imagePath),
      (Get-RepoRelative (Join-Path $metadata "resolved-wasm-packages.json"))
    )
  } finally {
    $env:GITHUB_SHA = $oldGitHubSha
  }
  Invoke-Checked $Rscript @(
    "tools/wasm/check-wasm-artifact.R",
    (Get-RepoRelative $out),
    $PackageSha
  )
  Write-Host "Built and verified pinned wasm repository for $PackageSha at $out."
} finally {
  Pop-Location
}
