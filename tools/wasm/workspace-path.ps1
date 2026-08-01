function Resolve-OpenSpecyWorkspacePath {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory = $true)]
    [string]$RepoRoot,
    [Parameter(Mandatory = $true)]
    [string]$Path
  )

  if (-not $Path.Trim()) { throw "Workspace path cannot be empty." }
  $candidate = if ([IO.Path]::IsPathRooted($Path)) {
    $Path
  } else {
    Join-Path $RepoRoot $Path
  }
  $resolved = [IO.Path]::GetFullPath($candidate)
  $root = [IO.Path]::GetFullPath($RepoRoot)
  $prefix = $root.TrimEnd(
    [IO.Path]::DirectorySeparatorChar,
    [IO.Path]::AltDirectorySeparatorChar
  ) + [IO.Path]::DirectorySeparatorChar
  if (-not $resolved.StartsWith(
      $prefix, [StringComparison]::OrdinalIgnoreCase)) {
    throw "Path must stay inside the repository: $Path"
  }
  $resolved
}

function Resolve-OpenSpecyScratchPath {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory = $true)]
    [string]$RepoRoot,
    [Parameter(Mandatory = $true)]
    [string]$Path
  )

  $resolved = Resolve-OpenSpecyWorkspacePath `
    -RepoRoot $RepoRoot -Path $Path
  $scratchRoot = [IO.Path]::GetFullPath((Join-Path $RepoRoot "_wasm"))
  $scratchPrefix = $scratchRoot.TrimEnd(
    [IO.Path]::DirectorySeparatorChar,
    [IO.Path]::AltDirectorySeparatorChar
  ) + [IO.Path]::DirectorySeparatorChar
  if (-not $resolved.StartsWith(
      $scratchPrefix, [StringComparison]::OrdinalIgnoreCase)) {
    throw "Scratch path must be a child of the repository's _wasm directory: $Path"
  }
  $resolved
}
