[CmdletBinding()]
param()

$ErrorActionPreference = "Stop"
. (Join-Path $PSScriptRoot "docker-preflight.ps1")

try {
  $readiness = Assert-OpenSpecyDockerEngine
} catch {
  Write-Host $_.Exception.Message -ForegroundColor Red
  exit 1
}

Write-Host "DOCKER ENGINE PREFLIGHT PASSED"
Write-Host "  Command: $($readiness.DockerCommand)"
Write-Host "  Server: $($readiness.ServerVersion)"
Write-Host "  Engine: $($readiness.OSType)"
Write-Host "This environment-only check did not build or reuse a wasm artifact."
