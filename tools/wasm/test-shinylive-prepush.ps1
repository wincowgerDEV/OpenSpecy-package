[CmdletBinding()]
param(
  [string]$WorkDir = "_wasm/shinylive-prepush",
  [string]$ToolDir = "_wasm/shinylive-prepush-tools",
  [string]$NodeDir = "_wasm/shinylive-prepush-tools/node",
  [string]$Rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe",
  [int]$Port = 8087
)

$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path ".").Path
. (Join-Path $PSScriptRoot "workspace-path.ps1")
$status = @(& git status --porcelain=v1 --untracked-files=all)
if ($status) {
  throw "The full pre-push rehearsal requires a clean local commit. Commit the candidate locally (do not push yet), then rerun."
}
$packageSha = (& git rev-parse HEAD).Trim()
if ($packageSha -notmatch '^[0-9a-fA-F]{40}$') {
  throw "Unable to resolve the candidate commit SHA."
}

$work = Resolve-OpenSpecyScratchPath -RepoRoot $repoRoot -Path $WorkDir
$pinned = Join-Path $work "pinned"
$siteWork = Join-Path $work "site-preflight"

& powershell.exe -ExecutionPolicy Bypass -File `
  "tools/wasm/build-wasm-repo.ps1" `
  -PackageSha $packageSha `
  -OutDir $pinned `
  -Rscript $Rscript
if ($LASTEXITCODE -ne 0) { throw "The pinned wasm build failed." }

& powershell.exe -ExecutionPolicy Bypass -File `
  "tools/wasm/test-shinylive-action.ps1" `
  -Artifact $pinned `
  -PackageSha $packageSha `
  -WorkDir $siteWork `
  -ToolDir $ToolDir `
  -NodeDir $NodeDir `
  -Rscript $Rscript `
  -Port $Port `
  -StageLibraries `
  -Bootstrap
if ($LASTEXITCODE -ne 0) { throw "The assembled Shinylive rehearsal failed." }

$siteRoot = Join-Path $siteWork "site"
$siteBytes = (Get-ChildItem -LiteralPath $siteRoot -Recurse -File |
  Measure-Object -Property Length -Sum).Sum
$report = [ordered]@{
  status = "passed"
  package_sha = $packageSha
  webr_image = "ghcr.io/r-wasm/webr@sha256:2bd309d7a4ea1daed82b6fdb8e325b0de715fcd8592c5b6f3b3b88366e70cb76"
  shinylive = "0.5.0"
  playwright = "1.61.1"
  site_bytes = [long]$siteBytes
  completed_at = [DateTime]::UtcNow.ToString("o")
}
$reportPath = Join-Path $work "prepush-report.json"
[IO.File]::WriteAllText(
  $reportPath,
  ($report | ConvertTo-Json -Depth 4) + [Environment]::NewLine
)
Write-Host "FULL SHINYLIVE PRE-PUSH REHEARSAL PASSED for $packageSha."
Write-Host "Evidence: $reportPath"
