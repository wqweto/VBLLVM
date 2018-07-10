@echo off
setlocal
echo EXPORTS
for /f "tokens=2,3 delims=@ " %%i in ('dumpbin /linkermember:1 %~dp0*.lib ^| findstr /r "^..........[_].*[^@]@[0-9]$ [^@]@[0-9][0-9]$"') do call :process %%i %%j
goto :eof

:process
set fn=%1
echo   %fn:~1%=%1@%2
