# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Download n2c into the project root, prepare a project-local Python
# environment, and run the one-time NDC11-to-ATC4 mapping needed between
# Code/2_variable construction/5.2_export_polypharmacy_unique_ndc11.R and
# Code/2_variable construction/5.3_stage_polypharmacy_ndc11_atc_crosswalk.R.
#
# This script only needs to be run once per exported unique-NDC list and mapping
# version. After the year-specific
# Outputs/5.x_annual_polypharmacy_<year>/5.3... crosswalk exists, do not rerun
# this script for the same export unless the NDC input file changed, the
# RxNav/n2c mapping version is being refreshed, or -Force is intentionally used.
#
# The n2c mapping run can take several hours because it queries RxNav for each
# unique NDC. n2c writes a cache beside the input file, so an interrupted run can
# resume without repeating completed lookups.

[CmdletBinding()]
param(
  [string]$InputPath = "Outputs\5.x_annual_polypharmacy_2016\5.2_polypharmacy_unique_ndc11_2016.txt",
  [string]$CrosswalkOutputPath = "Outputs\5.x_annual_polypharmacy_2016\5.3_polypharmacy_ndc11_atc_crosswalk_2016.csv",
  [string]$N2cDir = "n2c",
  [string]$N2cGitUrl = "https://github.com/fabkury/n2c.git",
  [ValidateSet("atc4")]
  [string]$Mapping = "atc4",
  [switch]$Force,
  [switch]$SkipDownload
)

$ErrorActionPreference = "Stop"

$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
Set-Location $repoRoot

$inputFullPath = [System.IO.Path]::GetFullPath((Join-Path $repoRoot $InputPath))
$crosswalkFullPath = [System.IO.Path]::GetFullPath((Join-Path $repoRoot $CrosswalkOutputPath))
$n2cFullPath = [System.IO.Path]::GetFullPath((Join-Path $repoRoot $N2cDir))
$venvPython = Join-Path $n2cFullPath ".venv\Scripts\python.exe"
$n2cScript = Join-Path $n2cFullPath "n2c.py"
$n2cNoInterruptScript = Join-Path $n2cFullPath "n2c_no_interrupt.py"

if (-not (Test-Path -LiteralPath $inputFullPath)) {
  throw "Missing n2c input file: $inputFullPath. Run Code/2_variable construction/5.2_export_polypharmacy_unique_ndc11.R first."
}

if ((Test-Path -LiteralPath $crosswalkFullPath) -and -not $Force) {
  Write-Host "Crosswalk already exists: $crosswalkFullPath"
  Write-Host "This one-time n2c mapping step does not need to be rerun for the same NDC export."
  Write-Host "Use -Force only if the unique NDC input or mapping version intentionally changed."
  exit 0
}

if (-not (Test-Path -LiteralPath $n2cFullPath)) {
  if ($SkipDownload) {
    throw "n2c directory is missing and -SkipDownload was supplied: $n2cFullPath"
  }

  $gitCommand = Get-Command git -ErrorAction SilentlyContinue
  if ($null -eq $gitCommand) {
    $gitExe = "C:\Program Files\Git\cmd\git.exe"
    if (-not (Test-Path -LiteralPath $gitExe)) {
      throw "Could not find git on PATH or at $gitExe. Install Git or clone n2c manually into $n2cFullPath."
    }
  } else {
    $gitExe = $gitCommand.Source
  }

  Write-Host "Downloading n2c into $n2cFullPath"
  & $gitExe clone $N2cGitUrl $n2cFullPath
}

if (-not (Test-Path -LiteralPath $n2cScript)) {
  throw "Could not find n2c.py after setup: $n2cScript"
}

if (-not (Test-Path -LiteralPath $venvPython)) {
  Write-Host "Creating project-local Python environment under $n2cFullPath\.venv"
  & py -3 -m venv (Join-Path $n2cFullPath ".venv")
}

Write-Host "Installing n2c Python dependencies into the project-local environment."
& $venvPython -m pip install requests keyboard

$sourceText = [System.IO.File]::ReadAllText($n2cScript)
$sourceText = $sourceText -replace "(?m)^import keyboard\r?\n", ""
$sourceText = $sourceText -replace "\r?\n\s+if keyboard\.is_pressed\('p'\):\r?\n\s+raise KeyboardInterrupt\r?\n", "`r`n"
[System.IO.File]::WriteAllText($n2cNoInterruptScript, $sourceText)

Write-Host "Running n2c $Mapping mapping. This may take several hours and only needs to complete once."
& $venvPython $n2cNoInterruptScript $inputFullPath --mapping $Mapping

$generatedFileName = (
  [System.IO.Path]::GetFileNameWithoutExtension($inputFullPath) +
  "_" +
  $Mapping.ToUpperInvariant() +
  "_classes.csv"
)
$generatedPath = Join-Path (Split-Path $inputFullPath -Parent) $generatedFileName

if (-not (Test-Path -LiteralPath $generatedPath)) {
  throw "n2c did not create the expected output file: $generatedPath"
}

$generatedItem = Get-Item -LiteralPath $generatedPath
if ($generatedItem.Length -le 16) {
  throw "n2c output appears to contain only the CSV header: $generatedPath. Do not stage it as a production crosswalk."
}

$crosswalkDir = Split-Path $crosswalkFullPath -Parent
if (-not (Test-Path -LiteralPath $crosswalkDir)) {
  New-Item -ItemType Directory -Path $crosswalkDir | Out-Null
}

Copy-Item -LiteralPath $generatedPath -Destination $crosswalkFullPath -Force

Write-Host "Completed one-time n2c mapping output:"
Write-Host "  Source: $generatedPath"
Write-Host "  Crosswalk for Code/2_variable construction/5.3: $crosswalkFullPath"
Write-Host "Next run Code/2_variable construction/5.3_stage_polypharmacy_ndc11_atc_crosswalk.R."
