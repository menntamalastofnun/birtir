$ErrorActionPreference = "Stop"

$staged = & 'C:\Git\cmd\git.exe' diff --cached --name-only |
  Where-Object { $_ -match '^README\.[R]?md$' }

$msg = "use 'git commit --no-verify' to override this check"

if ($staged.Count -eq 0) {
  exit 0
}

$readmeRmd = Join-Path (Get-Location) 'README.Rmd'
$readmeMd = Join-Path (Get-Location) 'README.md'

if ((Test-Path $readmeRmd) -and (Test-Path $readmeMd) -and ((Get-Item $readmeRmd).LastWriteTime -gt (Get-Item $readmeMd).LastWriteTime)) {
  Write-Host "README.md is out of date; please re-knit README.Rmd"
  Write-Host $msg
  exit 1
}

$hasReadmeRmd = $staged -contains 'README.Rmd'
$hasReadmeMd = $staged -contains 'README.md'

if (-not ($hasReadmeRmd -and $hasReadmeMd)) {
  Write-Host "README.Rmd and README.md should be both staged"
  Write-Host $msg
  exit 1
}

exit 0
