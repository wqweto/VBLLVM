@echo off
setlocal
set lib_arch=%1
set out_file="%~2"

set kernel_lib="%~dp0%lib_arch%\kernel32.lib"
if not exist %kernel_lib% echo No %lib_arch% support files found& exit /b 1

set startup_obj="%~dp0%lib_arch%\startup.o"
if not exist %startup_obj% echo No %lib_arch% support files found& exit /b 1

set link_exe="%ProgramFiles(x86)%\LLVM\bin\lld-link.exe"
if not exist %link_exe% set link_exe="%ProgramFiles(x86)%\Microsoft Visual Studio\VB98\link.exe"
if not exist %link_exe% for /f "delims=" %%i in ('where lld-link.exe') do set link_exe="%%i"
if not exist %link_exe% for /f "delims=" %%i in ('where link.exe') do set link_exe="%%i"
if not exist %link_exe% echo No linker executable found& exit /b 1

echo on
%link_exe% -nologo -subsystem:console,4.0 -nodefaultlib -entry:__runtime_main -out:%out_file% %kernel_lib% %startup_obj% %3 %4 %5 %6 %7 %8 %9
