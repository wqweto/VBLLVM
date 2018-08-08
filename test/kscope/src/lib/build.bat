@echo off
setlocal
set dlltool_exe="%~dp0..\..\..\..\lib\build-release\Release\bin\llvm-dlltool.exe"
set cc_exe="%ProgramFiles(x86)%\LLVM\bin\clang.exe"
set out_dir=%~dp0..\..\bin\lib
for %%i in ("%out_dir%\.") do set out_dir=%%~dpnxi

echo Building %out_dir%\win32
%dlltool_exe% -m i386 -d "%~dp0kernel32.def" -l "%out_dir%\win32\kernel32.lib" -k
%cc_exe% -march=i386 -c "%~dp0startup.c" -o "%out_dir%\win32\startup.o"

echo Building %out_dir%\x64
%dlltool_exe% -m i386:x86-64 -d "%~dp0kernel32_x64.def" -l "%out_dir%\x64\kernel32.lib"
%cc_exe% -target x86_64-pc-windows-msvc -c "%~dp0startup.c" -o "%out_dir%\x64\startup.o"

echo Done.