$bbjHome = (Resolve-Path (Join-Path $PSScriptRoot "..\..")).Path
& (Join-Path $bbjHome "bin\bbj.exe") -tIO -q (Join-Path $PSScriptRoot "bbjship.bbj") - @args | Out-Host
if ($null -eq $LASTEXITCODE) { exit 1 }
exit $LASTEXITCODE
