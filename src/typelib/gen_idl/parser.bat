@echo off
setlocal
set vbpeg=%~dp0..\..\..\..\vbpeg\vbpeg.exe
set infile=%~dpn0.peg
set outfile=%~dp0mdParser.bas

%vbpeg% %infile% -o %outfile%
