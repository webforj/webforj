@echo off
setlocal
set "plugin_dir=%~dp0"
powershell -NoProfile -ExecutionPolicy Bypass -File "%plugin_dir%bbjship.ps1" %*
exit /b %ERRORLEVEL%
