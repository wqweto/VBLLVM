@echo off
setlocal
set vbpeg=%~dp0..\..\..\vbpeg\vbpeg.exe
set infile=%~dp0gen_idl.peg
set outfile=%~dp0mdParser.bas

%vbpeg% %infile% -o %outfile%
