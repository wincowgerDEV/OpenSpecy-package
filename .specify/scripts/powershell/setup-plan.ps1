#!/usr/bin/env pwsh
# Create or report the concise feature plan for the active feature.

[CmdletBinding()]
param(
    [switch]$Json,
    [switch]$Force,
    [switch]$Help
)

$ErrorActionPreference = 'Stop'

if ($Help) {
    Write-Output "Usage: ./setup-plan.ps1 [-Json] [-Force] [-Help]"
    Write-Output "  -Json   Output results in JSON format"
    Write-Output "  -Force  Re-copy the plan template over an existing plan.md"
    Write-Output "  -Help   Show this help message"
    exit 0
}

. "$PSScriptRoot/common.ps1"

$paths = Get-FeaturePathsEnv

if (-not (Test-FeatureJsonMatchesFeatureDir -RepoRoot $paths.REPO_ROOT -ActiveFeatureDir $paths.FEATURE_DIR)) {
    if (-not (Test-FeatureBranch -Branch $paths.CURRENT_BRANCH -HasGit $paths.HAS_GIT)) {
        exit 1
    }
}

New-Item -ItemType Directory -Path $paths.FEATURE_DIR -Force | Out-Null

$created = $false
if ($Force -or -not (Test-Path -LiteralPath $paths.IMPL_PLAN -PathType Leaf)) {
    $template = Resolve-Template -TemplateName 'plan-template' -RepoRoot $paths.REPO_ROOT
    if ($template -and (Test-Path -LiteralPath $template -PathType Leaf)) {
        $content = [System.IO.File]::ReadAllText($template)
        $utf8NoBom = New-Object System.Text.UTF8Encoding($false)
        [System.IO.File]::WriteAllText($paths.IMPL_PLAN, $content, $utf8NoBom)
        $created = $true
    } else {
        New-Item -ItemType File -Path $paths.IMPL_PLAN -Force | Out-Null
        $created = $true
    }
}

if ($Json) {
    [PSCustomObject]@{
        FEATURE_DIR = $paths.FEATURE_DIR
        IMPL_PLAN   = $paths.IMPL_PLAN
        BRANCH      = $paths.CURRENT_BRANCH
        HAS_GIT     = $paths.HAS_GIT
        CREATED     = $created
    } | ConvertTo-Json -Compress
} else {
    Write-Output "FEATURE_DIR: $($paths.FEATURE_DIR)"
    Write-Output "IMPL_PLAN: $($paths.IMPL_PLAN)"
    Write-Output "BRANCH: $($paths.CURRENT_BRANCH)"
    Write-Output "HAS_GIT: $($paths.HAS_GIT)"
    Write-Output "CREATED: $created"
}
