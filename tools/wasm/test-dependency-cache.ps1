$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "../..")).Path
. (Join-Path $PSScriptRoot "workspace-path.ps1")
. (Join-Path $PSScriptRoot "dependency-cache.ps1")

$fixture = Resolve-OpenSpecyScratchPath -RepoRoot $repoRoot `
  -Path ("_wasm/dependency-cache-fixture-" + $PID)
try {
  $older = Join-Path $fixture "older"
  $newer = Join-Path $fixture "newer"
  foreach ($candidate in @($older, $newer)) {
    $contrib = Join-Path $candidate "repo/bin/emscripten/contrib/4.6"
    New-Item -ItemType Directory -Path $contrib -Force | Out-Null
    New-Item -ItemType File -Path (Join-Path $contrib "PACKAGES") `
      -Force | Out-Null
  }
  [IO.Directory]::SetLastWriteTimeUtc($older, [DateTime]::UtcNow.AddMinutes(-2))
  [IO.Directory]::SetLastWriteTimeUtc($newer, [DateTime]::UtcNow.AddMinutes(-1))
  New-Item -ItemType Directory -Path (Join-Path $fixture "current") `
    -Force | Out-Null

  $selected = Get-OpenSpecyCompatibleWasmCacheSeed `
    -CacheRoot $fixture -CurrentKey "current"
  $expected = Join-Path $newer "repo"
  if (-not [string]::Equals(
      $selected, $expected, [StringComparison]::OrdinalIgnoreCase)) {
    throw "Expected compatible cache seed $expected; found $selected"
  }
  Write-Host "Compatible wasm dependency cache fallback passed."
} finally {
  if (Test-Path -LiteralPath $fixture) {
    Remove-Item -LiteralPath $fixture -Recurse -Force
  }
}
