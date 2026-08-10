param(
  [string]$IncludeUntracked = "",
  [string]$ExcludeUntracked = "",
  [switch]$PrepareOnly
)

$ErrorActionPreference = "Stop"
$repo = (Resolve-Path (Join-Path $PSScriptRoot "..\..\..\..")).Path
$rscript = "C:\Program Files\R\R-4.3.3\bin\Rscript.exe"

if (-not (Test-Path -LiteralPath $rscript)) {
  $rscript = Get-ChildItem -Path "C:\Program Files\R" -Recurse `
    -Filter Rscript.exe -ErrorAction SilentlyContinue |
    Sort-Object FullName -Descending |
    Select-Object -First 1 -ExpandProperty FullName
}
if (-not $rscript -or -not (Test-Path -LiteralPath $rscript)) {
  throw "No installed Rscript.exe was found."
}
$rExecutable = Join-Path (Split-Path -Parent $rscript) "R.exe"
if (-not (Test-Path -LiteralPath $rExecutable)) {
  throw "R.exe was not found beside $rscript."
}

function ConvertTo-SafeRelativePath {
  param([string]$Path)
  $normalized = $Path.Replace("\", "/")
  while ($normalized.StartsWith("./", [StringComparison]::Ordinal)) {
    $normalized = $normalized.Substring(2)
  }
  if (-not $normalized -or [IO.Path]::IsPathRooted($normalized) -or
      $normalized -match '(^|/)\.\.(/|$)') {
    throw "Unsafe candidate path: $Path"
  }
  $normalized
}

function Copy-CandidateFile {
  param([string]$RelativePath, [string]$DestinationRoot)
  $safePath = ConvertTo-SafeRelativePath $RelativePath
  $fileSystemPath = $safePath.Replace(
    "/", [IO.Path]::DirectorySeparatorChar
  )
  $source = Join-Path $repo $fileSystemPath
  if (-not (Test-Path -LiteralPath $source -PathType Leaf)) {
    throw "Candidate file is missing or is not a regular file: $safePath"
  }
  $destination = Join-Path $DestinationRoot $fileSystemPath
  $parent = Split-Path -Parent $destination
  if (-not (Test-Path -LiteralPath $parent)) {
    New-Item -ItemType Directory -Path $parent -Force | Out-Null
  }
  Copy-Item -LiteralPath $source -Destination $destination
}

function Split-PathList {
  param([string]$Value)
  if ([string]::IsNullOrWhiteSpace($Value)) {
    return @()
  }
  @($Value -split ';' | ForEach-Object { $_.Trim() } |
    Where-Object { $_ })
}

Push-Location $repo
try {
  $tracked = @(git ls-files --cached | ForEach-Object {
    ConvertTo-SafeRelativePath $_
  })
  if ($LASTEXITCODE -ne 0) {
    throw "git ls-files failed while preparing the package candidate."
  }
  $untracked = @(git ls-files --others --exclude-standard | ForEach-Object {
    ConvertTo-SafeRelativePath $_
  })
  if ($LASTEXITCODE -ne 0) {
    throw "git ls-files failed while classifying untracked files."
  }

  $included = @(Split-PathList $IncludeUntracked | ForEach-Object {
    ConvertTo-SafeRelativePath $_
  } | Sort-Object -Unique)
  $excluded = @(Split-PathList $ExcludeUntracked | ForEach-Object {
    ConvertTo-SafeRelativePath $_
  } | Sort-Object -Unique)
  $overlap = @($included | Where-Object { $excluded -contains $_ })
  if ($overlap.Count) {
    throw "Untracked paths cannot be both included and excluded: $($overlap -join ', ')"
  }

  $knownUntracked = @($included + $excluded | Sort-Object -Unique)
  $unknownClassification = @($knownUntracked | Where-Object {
    $untracked -notcontains $_
  })
  if ($unknownClassification.Count) {
    throw "Classified paths are not currently untracked: $($unknownClassification -join ', ')"
  }
  $unclassified = @($untracked | Where-Object {
    $knownUntracked -notcontains $_
  })
  if ($unclassified.Count) {
    throw @"
Every untracked file must be explicitly included in or excluded from the package candidate.
Unclassified paths:
$($unclassified -join "`n")
Use semicolon-delimited -IncludeUntracked for intended package source and -ExcludeUntracked for unrelated work or artifacts.
"@
  }

  $work = Join-Path ([IO.Path]::GetTempPath()) (
    "openspecy-staged-check-" + [guid]::NewGuid().ToString("N")
  )
  $stage = Join-Path $work "OpenSpecy"
  New-Item -ItemType Directory -Path $stage -Force | Out-Null

  $trackedExisting = @($tracked | Where-Object {
    $candidatePath = Join-Path $repo (
      $_.Replace("/", [IO.Path]::DirectorySeparatorChar)
    )
    Test-Path -LiteralPath $candidatePath -PathType Leaf
  })
  foreach ($path in $trackedExisting) {
    Copy-CandidateFile -RelativePath $path -DestinationRoot $stage
  }
  foreach ($path in $included) {
    Copy-CandidateFile -RelativePath $path -DestinationRoot $stage
  }

  $expectedPaths = @($trackedExisting + $included | Sort-Object -Unique)
  $stagePrefixLength = $stage.Length + 1
  $stagedFiles = @(Get-ChildItem -LiteralPath $stage -Recurse -File)
  $stagedPaths = @($stagedFiles | ForEach-Object {
    $_.FullName.Substring($stagePrefixLength).Replace("\", "/")
  } | Sort-Object -Unique)
  $manifestDifference = @(
    $expectedPaths | Where-Object { $stagedPaths -notcontains $_ }
    $stagedPaths | Where-Object { $expectedPaths -notcontains $_ }
  )
  if ($manifestDifference.Count) {
    throw "Staged candidate manifest differs from the intended file set."
  }

  $forbidden = @($stagedPaths | Where-Object {
    $_ -match '(^|/)(\.git|_wasm|node_modules|test-results|playwright-report|\.codex-release-check)(/|$)'
  })
  if ($forbidden.Count) {
    throw "Forbidden generated or nested paths entered the candidate: $($forbidden -join ', ')"
  }

  $manifestPath = Join-Path $work "candidate-manifest.tsv"
  $manifest = @($stagedFiles | ForEach-Object {
    $relative = $_.FullName.Substring($stagePrefixLength).Replace("\", "/")
    $hash = (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash
    "{0}`t{1}`t{2}" -f $relative, $_.Length, $hash
  } | Sort-Object)
  $manifest | Set-Content -LiteralPath $manifestPath -Encoding utf8
  $stageBytes = ($stagedFiles | Measure-Object Length -Sum).Sum
  Write-Host ("Staged candidate: {0} files, {1} bytes" -f `
    $stagedFiles.Count, $stageBytes)
  Write-Host "Candidate manifest: $manifestPath"
  Write-Host "Staged source: $stage"

  if ($PrepareOnly) {
    return
  }

  $buildLog = Join-Path $work "build-console.log"
  Push-Location $work
  try {
    $previousErrorPreference = $ErrorActionPreference
    $ErrorActionPreference = "Continue"
    try {
      & $rExecutable CMD build $stage *> $buildLog
      $buildExit = $LASTEXITCODE
    }
    finally {
      $ErrorActionPreference = $previousErrorPreference
    }
  }
  finally {
    Pop-Location
  }
  if ($buildExit -ne 0) {
    Get-Content -LiteralPath $buildLog -Tail 80
    throw "R CMD build failed; complete log: $buildLog"
  }
  $tarball = Get-ChildItem -LiteralPath $work -Filter "*.tar.gz" -File |
    Sort-Object LastWriteTime -Descending | Select-Object -First 1
  if (-not $tarball) {
    throw "R CMD build succeeded but produced no source tarball; log: $buildLog"
  }

  $checkConsole = Join-Path $work "check-console.log"
  Push-Location $work
  try {
    $previousErrorPreference = $ErrorActionPreference
    $ErrorActionPreference = "Continue"
    try {
      & $rExecutable CMD check --no-manual $tarball.FullName *> $checkConsole
      $checkExit = $LASTEXITCODE
    }
    finally {
      $ErrorActionPreference = $previousErrorPreference
    }
  }
  finally {
    Pop-Location
  }
  $checkDirectory = Get-ChildItem -LiteralPath $work -Directory `
    -Filter "*.Rcheck" | Select-Object -First 1
  $checkLog = if ($checkDirectory) {
    Join-Path $checkDirectory.FullName "00check.log"
  } else {
    $checkConsole
  }
  if ($checkExit -ne 0) {
    Get-Content -LiteralPath $checkLog -Tail 100
    throw "R CMD check failed; complete log: $checkLog"
  }
  $status = Select-String -LiteralPath $checkLog -Pattern '^Status:' |
    Select-Object -Last 1 -ExpandProperty Line
  if (-not $status) {
    $status = "Status: OK"
  }
  Write-Host $status
  Write-Host "R CMD check log: $checkLog"
}
finally {
  Pop-Location
}
