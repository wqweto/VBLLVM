@echo off
setlocal
set dlltool_exe="%~dp0..\..\..\..\lib\llvm-build-release\Release\bin\llvm-dlltool.exe"
set cc_exe="%ProgramFiles(x86)%\LLVM\bin\clang.exe"
set out_dir=%~dp0..\..\bin\lib
for %%i in ("%out_dir%\.") do set out_dir=%%~dpnxi

echo Building %out_dir%\coff\i386
%dlltool_exe% -m i386 -d "%~dp0kernel32.def" -l "%out_dir%\coff\i386\kernel32.lib" -k
%cc_exe% -target i686-pc-windows-msvc -c "%~dp0startup.c" -o "%out_dir%\coff\i386\startup.obj"

echo Building %out_dir%\coff\x86_64
%dlltool_exe% -m i386:x86-64 -d "%~dp0kernel32_x64.def" -l "%out_dir%\coff\x86_64\kernel32.lib"
%cc_exe% -target x86_64-pc-windows-msvc -c "%~dp0startup.c" -o "%out_dir%\coff\x86_64\startup.obj"

echo Done.
