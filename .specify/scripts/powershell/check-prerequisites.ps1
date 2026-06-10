#!/usr/bin/env pwsh
# Prerequisite checks for the concise plan workflow.

[CmdletBinding()]
param(
    [switch]$Json,
    [switch]$RequireTasks,
    [switch]$IncludeTasks,
    [switch]$PathsOnly,
    [switch]$Help
)

$ErrorActionPreference = 'Stop'

if ($Help) {
    Write-Output @"
Usage: check-prerequisites.ps1 [OPTIONS]

OPTIONS:
  -Json          Output JSON
  -PathsOnly     Return resolved paths without validation
  -IncludeTasks  Include legacy tasks.md in AVAILABLE_DOCS if it exists
  -RequireTasks  Deprecated no-op; tasks are embedded in plan.md
  -Help          Show this help
"@
    exit 0
}

. "$PSScriptRoot/common.ps1"

$paths = Get-FeaturePathsEnv

if (-not (Test-FeatureJsonMatchesFeatureDir -RepoRoot $paths.REPO_ROOT -ActiveFeatureDir $paths.FEATURE_DIR)) {
    if (-not (Test-FeatureBranch -Branch $paths.CURRENT_BRANCH -HasGit:$paths.HAS_GIT)) {
        exit 1
    }
}

if ($PathsOnly) {
    if ($Json) {
        [PSCustomObject]@{
            REPO_ROOT    = $paths.REPO_ROOT
            BRANCH       = $paths.CURRENT_BRANCH
            FEATURE_DIR  = $paths.FEATURE_DIR
            IMPL_PLAN    = $paths.IMPL_PLAN
            FEATURE_SPEC = $paths.FEATURE_SPEC
            TASKS        = $paths.TASKS
        } | ConvertTo-Json -Compress
    } else {
        Write-Output "REPO_ROOT: $($paths.REPO_ROOT)"
        Write-Output "BRANCH: $($paths.CURRENT_BRANCH)"
        Write-Output "FEATURE_DIR: $($paths.FEATURE_DIR)"
        Write-Output "IMPL_PLAN: $($paths.IMPL_PLAN)"
        Write-Output "FEATURE_SPEC: $($paths.FEATURE_SPEC)"
        Write-Output "TASKS: $($paths.TASKS)"
    }
    exit 0
}

if (-not (Test-Path -LiteralPath $paths.FEATURE_DIR -PathType Container)) {
    [Console]::Error.WriteLine("ERROR: Feature directory not found: $($paths.FEATURE_DIR)")
    [Console]::Error.WriteLine("Run the concise plan workflow first.")
    exit 1
}

if (-not (Test-Path -LiteralPath $paths.IMPL_PLAN -PathType Leaf)) {
    [Console]::Error.WriteLine("ERROR: plan.md not found in $($paths.FEATURE_DIR)")
    [Console]::Error.WriteLine("Run the concise plan workflow first.")
    exit 1
}

$docs = @('plan.md')
if (Test-Path -LiteralPath $paths.FEATURE_SPEC -PathType Leaf) { $docs += 'spec.md (legacy)' }
if (Test-Path -LiteralPath $paths.RESEARCH -PathType Leaf) { $docs += 'research.md (legacy)' }
if (Test-Path -LiteralPath $paths.DATA_MODEL -PathType Leaf) { $docs += 'data-model.md (legacy)' }
if ((Test-Path -LiteralPath $paths.CONTRACTS_DIR -PathType Container) -and (Get-ChildItem -LiteralPath $paths.CONTRACTS_DIR -ErrorAction SilentlyContinue | Select-Object -First 1)) {
    $docs += 'contracts/ (legacy)'
}
if (Test-Path -LiteralPath $paths.QUICKSTART -PathType Leaf) { $docs += 'quickstart.md (legacy)' }
if ($IncludeTasks -and (Test-Path -LiteralPath $paths.TASKS -PathType Leaf)) { $docs += 'tasks.md (legacy)' }

if ($Json) {
    [PSCustomObject]@{
        FEATURE_DIR    = $paths.FEATURE_DIR
        IMPL_PLAN      = $paths.IMPL_PLAN
        AVAILABLE_DOCS = $docs
    } | ConvertTo-Json -Compress
} else {
    Write-Output "FEATURE_DIR: $($paths.FEATURE_DIR)"
    Write-Output "IMPL_PLAN: $($paths.IMPL_PLAN)"
    Write-Output "AVAILABLE_DOCS:"
    foreach ($doc in $docs) {
        Write-Output "  - $doc"
    }
}
