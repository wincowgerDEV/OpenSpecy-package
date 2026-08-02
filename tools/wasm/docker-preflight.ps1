function ConvertTo-OpenSpecyDiagnosticText {
  param([AllowNull()][string]$Text)

  if ([string]::IsNullOrWhiteSpace($Text)) { return "" }
  (($Text -replace [char]0, "") -replace "`r", "").Trim()
}

function Test-OpenSpecyWindowsHost {
  $env:OS -eq "Windows_NT"
}

function Invoke-OpenSpecyDiagnosticCommand {
  param(
    [Parameter(Mandatory = $true)][string]$File,
    [string[]]$Arguments = @(),
    [int]$TimeoutSeconds = 10
  )

  $start = New-Object System.Diagnostics.ProcessStartInfo
  $start.FileName = $File
  $start.Arguments = (@($Arguments) | ForEach-Object {
    '"' + ([string]$_).Replace('"', '\"') + '"'
  }) -join " "
  $start.UseShellExecute = $false
  $start.CreateNoWindow = $true
  $start.RedirectStandardOutput = $true
  $start.RedirectStandardError = $true

  $process = New-Object System.Diagnostics.Process
  $process.StartInfo = $start
  try {
    [void]$process.Start()
    if (-not $process.WaitForExit($TimeoutSeconds * 1000)) {
      try { $process.Kill() } catch { }
      return [pscustomobject]@{
        ExitCode = $null
        Stdout = ""
        Stderr = "Timed out after $TimeoutSeconds seconds."
        TimedOut = $true
      }
    }
    $stdout = ConvertTo-OpenSpecyDiagnosticText $process.StandardOutput.ReadToEnd()
    $stderr = ConvertTo-OpenSpecyDiagnosticText $process.StandardError.ReadToEnd()
    [pscustomobject]@{
      ExitCode = $process.ExitCode
      Stdout = $stdout
      Stderr = $stderr
      TimedOut = $false
    }
  } catch {
    [pscustomobject]@{
      ExitCode = $null
      Stdout = ""
      Stderr = $_.Exception.Message
      TimedOut = $false
    }
  } finally {
    $process.Dispose()
  }
}

function Get-OpenSpecyDockerDesktopVersion {
  if (-not (Test-OpenSpecyWindowsHost)) { return "not applicable" }
  if (-not $env:ProgramFiles) { return "not detected" }
  $candidates = @(
    (Join-Path $env:ProgramFiles "Docker\Docker\Docker Desktop.exe"),
    (Join-Path $env:ProgramFiles "Docker\Docker\resources\bin\docker.exe")
  ) | Where-Object { $_ -and (Test-Path -LiteralPath $_) }
  foreach ($candidate in $candidates) {
    $version = (Get-Item -LiteralPath $candidate).VersionInfo.ProductVersion
    if (-not [string]::IsNullOrWhiteSpace($version)) { return $version }
  }
  "not detected"
}

function Get-OpenSpecyDockerDiagnostics {
  param([AllowNull()][string]$DockerCommand)

  $details = New-Object System.Collections.Generic.List[string]
  if ($DockerCommand) {
    $client = Invoke-OpenSpecyDiagnosticCommand $DockerCommand @("--version")
    if ($client.ExitCode -eq 0 -and $client.Stdout) {
      $details.Add("Docker CLI: $($client.Stdout)")
    } else {
      $details.Add("Docker CLI: $DockerCommand")
    }
    $context = Invoke-OpenSpecyDiagnosticCommand $DockerCommand @("context", "show")
    if ($context.ExitCode -eq 0 -and $context.Stdout) {
      $details.Add("Docker context: $($context.Stdout)")
    } else {
      $details.Add("Docker context: unavailable")
    }
  } else {
    $details.Add("Docker CLI: not found on PATH")
  }

  if (Test-OpenSpecyWindowsHost) {
    $desktopVersion = Get-OpenSpecyDockerDesktopVersion
    $details.Add("Docker Desktop: $desktopVersion")
    if ($desktopVersion -match '^4\.19\.') {
      $details.Add(
        "Docker Desktop update: 4.19 is an old release line; update it before deeper recovery."
      )
    }
    $desktopProcesses = @(Get-Process -Name "Docker Desktop", `
      "com.docker.backend", "com.docker.proxy" -ErrorAction SilentlyContinue |
      Select-Object -ExpandProperty ProcessName -Unique)
    if ($desktopProcesses.Count) {
      $details.Add("Docker Desktop processes: $($desktopProcesses -join ', ')")
    } else {
      $details.Add("Docker Desktop processes: not running")
    }

    $serviceCommand = Get-Command Get-Service -ErrorAction SilentlyContinue
    $service = if ($serviceCommand) {
      Get-Service -Name "com.docker.service" -ErrorAction SilentlyContinue
    } else { $null }
    if ($service) {
      $details.Add(
        "Docker Desktop service: $($service.Status) ($($service.StartType))"
      )
    } else {
      $details.Add("Docker Desktop service: not detected")
    }

    $settingsPath = Join-Path `
      ([Environment]::GetFolderPath("ApplicationData")) `
      "Docker\settings-store.json"
    if (Test-Path -LiteralPath $settingsPath) {
      try {
        $settings = Get-Content -LiteralPath $settingsPath -Raw |
          ConvertFrom-Json
        $details.Add(
          "Docker settings: WSL2 engine=$($settings.wslEngineEnabled); " +
          "auto-start=$($settings.autoStart)"
        )
      } catch {
        $details.Add("Docker settings: present but unreadable")
      }
    }

    $wsl = Get-Command wsl.exe -ErrorAction SilentlyContinue
  } else {
    $wsl = $null
    $details.Add("Host: $([Runtime.InteropServices.RuntimeInformation]::OSDescription)")
  }
  if ($wsl) {
    $wslVersion = Invoke-OpenSpecyDiagnosticCommand $wsl.Source @("--version")
    if ($wslVersion.ExitCode -eq 0 -and $wslVersion.Stdout) {
      $versionLines = @($wslVersion.Stdout -split "`n" |
        Where-Object { $_.Trim() } | Select-Object -First 2)
      $details.Add("WSL: $($versionLines -join '; ')")
    } else {
      $wslReason = ConvertTo-OpenSpecyDiagnosticText `
        ($wslVersion.Stderr + "`n" + $wslVersion.Stdout)
      $reason = if ($wslReason) {
        @($wslReason -split "`n" | Where-Object { $_.Trim() } |
          Select-Object -First 1)[0]
      } else { "diagnostic unavailable" }
      $details.Add("WSL: $reason")
    }
    $wslStatus = Invoke-OpenSpecyDiagnosticCommand $wsl.Source @("--status")
    if ($wslStatus.ExitCode -ne 0) {
      $reason = ConvertTo-OpenSpecyDiagnosticText `
        ($wslStatus.Stderr + "`n" + $wslStatus.Stdout)
      if ($reason) {
        $firstLine = @($reason -split "`n" |
          Where-Object { $_.Trim() } | Select-Object -First 1)[0]
        $details.Add("WSL status: $firstLine")
      }
    }
  } elseif (Test-OpenSpecyWindowsHost) {
    $details.Add("WSL: wsl.exe not found")
  }
  @($details)
}

function Get-OpenSpecyDockerReadiness {
  $docker = Get-Command docker -ErrorAction SilentlyContinue
  if (-not $docker) {
    return [pscustomobject]@{
      Ready = $false
      Reason = "Docker CLI was not found."
      DockerCommand = $null
      ServerVersion = $null
      OSType = $null
      Details = @(Get-OpenSpecyDockerDiagnostics $null)
    }
  }

  $server = Invoke-OpenSpecyDiagnosticCommand $docker.Source @(
    "version", "--format", "{{.Server.Version}}"
  )
  if ($server.ExitCode -ne 0 -or -not $server.Stdout) {
    $reason = ConvertTo-OpenSpecyDiagnosticText `
      ($server.Stderr + "`n" + $server.Stdout)
    if (-not $reason) { $reason = "Docker did not return a server version." }
    return [pscustomobject]@{
      Ready = $false
      Reason = "Docker's engine is unreachable: $reason"
      DockerCommand = $docker.Source
      ServerVersion = $null
      OSType = $null
      Details = @(Get-OpenSpecyDockerDiagnostics $docker.Source)
    }
  }

  $info = Invoke-OpenSpecyDiagnosticCommand $docker.Source @(
    "info", "--format", "{{.OSType}}"
  )
  $osType = $info.Stdout.Trim().ToLowerInvariant()
  if ($info.ExitCode -ne 0 -or $osType -ne "linux") {
    $reason = if ($info.ExitCode -ne 0) {
      ConvertTo-OpenSpecyDiagnosticText ($info.Stderr + "`n" + $info.Stdout)
    } else {
      "the active engine reports '$osType' instead of 'linux'"
    }
    return [pscustomobject]@{
      Ready = $false
      Reason = "A reachable Linux Docker engine is required: $reason"
      DockerCommand = $docker.Source
      ServerVersion = $server.Stdout.Trim()
      OSType = $osType
      Details = @(Get-OpenSpecyDockerDiagnostics $docker.Source)
    }
  }

  [pscustomobject]@{
    Ready = $true
    Reason = ""
    DockerCommand = $docker.Source
    ServerVersion = $server.Stdout.Trim()
    OSType = $osType
    Details = @()
  }
}

function Format-OpenSpecyDockerFailure {
  param([Parameter(Mandatory = $true)]$Readiness)

  $lines = @(
    "DOCKER ENGINE PREFLIGHT FAILED",
    $Readiness.Reason
  )
  $lines += @($Readiness.Details | ForEach-Object { "  $_" })
  if (Test-OpenSpecyWindowsHost) {
    $lines += @(
      "",
      "Recovery (does not delete Docker or WSL data):",
      "  1. Start or reopen Docker Desktop and wait for its Linux engine to be ready.",
      "  2. Update Docker Desktop if the detected installation is old, and run: wsl --update",
      "  3. If it still fails, close Docker Desktop, run: wsl --shutdown, then reopen Docker Desktop.",
      "  4. Rerun: powershell -NoProfile -ExecutionPolicy Bypass -File tools/wasm/test-docker-engine.ps1",
      "Do not unregister WSL distributions, delete VHDX files, or factory-reset Docker as a first repair step."
    )
  } else {
    $lines += @(
      "",
      "Recovery:",
      "  1. Start the Docker daemon and confirm this account can access it.",
      "  2. Select a Docker context backed by a Linux engine.",
      "  3. Rerun: pwsh -File tools/wasm/test-docker-engine.ps1"
    )
  }
  $lines += "No wasm build or cached/native fallback was used."
  $lines -join [Environment]::NewLine
}

function Assert-OpenSpecyDockerEngine {
  $readiness = Get-OpenSpecyDockerReadiness
  if (-not $readiness.Ready) {
    throw (Format-OpenSpecyDockerFailure $readiness)
  }
  $readiness
}
