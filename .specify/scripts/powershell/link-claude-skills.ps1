<#
Recreate the local NTFS directory junctions that let Claude Code's Skill tool
discover the shared `openspecy-*` skills authored under `.agents/skills/`.
These junctions are git-ignored (git cannot track a junction as a link; it
would silently duplicate the files instead), so run this once after cloning
and again only if a new `openspecy-*` skill directory is added.
#>

$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..\..\..")).Path
$sourceRoot = Join-Path $repoRoot ".agents\skills"
$linkRoot = Join-Path $repoRoot ".claude\skills"

New-Item -ItemType Directory -Force -Path $linkRoot | Out-Null

Get-ChildItem -Path $sourceRoot -Directory -Filter "openspecy-*" | ForEach-Object {
    $link = Join-Path $linkRoot $_.Name
    $target = $_.FullName
    if (Test-Path $link) {
        $existing = Get-Item $link
        if ($existing.LinkType -eq "Junction" -and $existing.Target -contains $target) {
            Write-Output "ok      $($_.Name)"
            return
        }
        Remove-Item $link -Recurse -Force
    }
    New-Item -ItemType Junction -Path $link -Target $target | Out-Null
    Write-Output "linked  $($_.Name)"
}
