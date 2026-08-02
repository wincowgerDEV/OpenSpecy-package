[CmdletBinding()]
param()

$ErrorActionPreference = "Stop"
. (Join-Path $PSScriptRoot "workspace-path.ps1")
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "../..")).Path
$relativeExpected = [IO.Path]::GetFullPath((Join-Path $repoRoot "_wasm/path #% test"))
$relative = Resolve-OpenSpecyWorkspacePath `
  -RepoRoot $repoRoot -Path "_wasm/path #% test"
$absolute = Resolve-OpenSpecyWorkspacePath `
  -RepoRoot $repoRoot -Path $relativeExpected
if ($relative -ne $relativeExpected -or $absolute -ne $relativeExpected) {
  throw "Relative and absolute workspace paths did not resolve identically."
}
$repoRelative = Get-OpenSpecyRepoRelativePath `
  -RepoRoot $repoRoot -Path $relativeExpected
$repoRelativeExpected = Join-Path "_wasm" "path #% test"
if ($repoRelative -ne $repoRelativeExpected -or
    [IO.Path]::IsPathRooted($repoRelative)) {
  throw "Repository-relative paths were not computed portably."
}

$repoParent = [IO.Directory]::GetParent([IO.Path]::GetFullPath($repoRoot))
if (-not $repoParent) { throw "Repository root must have a parent directory." }
$outside = Join-Path $repoParent.FullName "openspecy-outside-test"
$rejected = $false
try {
  Resolve-OpenSpecyWorkspacePath -RepoRoot $repoRoot -Path $outside |
    Out-Null
} catch {
  $rejected = $true
}
if (-not $rejected) { throw "An out-of-workspace path was accepted." }

$scratchRelative = Resolve-OpenSpecyScratchPath `
  -RepoRoot $repoRoot -Path "_wasm/path #% test"
$scratchAbsolute = Resolve-OpenSpecyScratchPath `
  -RepoRoot $repoRoot -Path $relativeExpected
if ($scratchRelative -ne $relativeExpected -or
    $scratchAbsolute -ne $relativeExpected) {
  throw "Relative and absolute scratch paths did not resolve identically."
}
foreach ($unsafe in @("_wasm", "inst", "R")) {
  $rejected = $false
  try {
    Resolve-OpenSpecyScratchPath -RepoRoot $repoRoot -Path $unsafe |
      Out-Null
  } catch {
    $rejected = $true
  }
  if (-not $rejected) { throw "Unsafe scratch path was accepted: $unsafe" }
}
if ([IO.Path]::DirectorySeparatorChar -ne "\") {
  $caseMismatch = Join-Path $repoRoot "_WASM/path-test"
  $rejected = $false
  try {
    Resolve-OpenSpecyScratchPath -RepoRoot $repoRoot -Path $caseMismatch |
      Out-Null
  } catch {
    $rejected = $true
  }
  if (-not $rejected) {
    throw "A case-mismatched Linux scratch path was accepted."
  }
}
Write-Host "Workspace path resolver accepts safe relative/absolute paths, emits portable repository-relative paths, rejects escapes, and constrains scratch cleanup to _wasm children."
