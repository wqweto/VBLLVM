@echo off
setlocal
set vbpeg_exe=%~dp0vbpeg.exe
set peg_dir=%~dp0

%vbpeg_exe% %peg_dir%\grammar.peg -o %peg_dir%\..\src\cParser.cls
if errorlevel 1 pause
