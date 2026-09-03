function Get-OpenSpecyCompatibleWasmCacheSeed {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory = $true)]
    [string]$CacheRoot,

    [Parameter(Mandatory = $true)]
    [string]$CurrentKey
  )

  if (-not (Test-Path -LiteralPath $CacheRoot -PathType Container)) {
    return $null
  }

  $candidates = foreach ($directory in Get-ChildItem -LiteralPath $CacheRoot `
      -Directory -ErrorAction SilentlyContinue) {
    if ($directory.Name -eq $CurrentKey) { continue }
    $repo = Join-Path $directory.FullName "repo"
    if (-not (Test-Path -LiteralPath $repo -PathType Container)) { continue }
    $binaryIndex = Get-ChildItem -LiteralPath $repo -Recurse -Filter PACKAGES `
      -File -ErrorAction SilentlyContinue | Where-Object {
        $_.FullName -match '[\\/]bin[\\/]emscripten[\\/]contrib[\\/]'
      } | Select-Object -First 1
    if ($null -eq $binaryIndex) { continue }
    [PSCustomObject]@{
      Repo = $repo
      LastWriteTimeUtc = $directory.LastWriteTimeUtc
      Key = $directory.Name
    }
  }

  $selected = $candidates | Sort-Object LastWriteTimeUtc, Key -Descending |
    Select-Object -First 1
  if ($null -eq $selected) { return $null }
  $selected.Repo
}
