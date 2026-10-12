@echo off
setlocal
set "plugin_dir=%~dp0"
for %%H in ("%plugin_dir%..\..") do set "home=%%~fH"
if not defined JAVA_HOME (
  echo Set JAVA_HOME to the Java runtime BBjServices uses, then run the command again.
  exit /b 1
)
"%JAVA_HOME%\bin\java.exe" "-Dbasis.BBjHome=%home%" -cp "%home%\.lib\*;%plugin_dir%lib\*" com.webforj.bbjship.cli.BBjShipCli %*
exit /b %ERRORLEVEL%
