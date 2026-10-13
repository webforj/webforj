@echo off
setlocal
set "plugin_dir=%~dp0"
for %%H in ("%plugin_dir%..\..") do set "home=%%~fH"
"%home%\bin\bbj.exe" -tIO -q "%plugin_dir%bbjship.bbj" - %*
exit /b %ERRORLEVEL%
