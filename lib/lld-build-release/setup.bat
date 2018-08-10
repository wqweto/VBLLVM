@echo off
setlocal
call "%VS140COMNTOOLS%..\..\VC\bin\vcvars32.bat"
set path=%ProgramFiles%\CMake\bin;%LOCALAPPDATA%\Programs\Python\Python36-32;%PATH%
set libroot=%~dp0..

md "%libroot%\llvm-build-release\bin" >nul 2>&1
copy "%libroot%\llvm-build-release\Release\bin\llvm-config.exe" "%libroot%\llvm-build-release\bin\llvm-config.exe" >nul 2>&1

if exist lld.sln goto :skip_gen
cmake -G "Visual Studio 14" -D LLVM_TARGETS_TO_BUILD=X86 ^
    -D LLVM_USE_CRT_DEBUG=MTd -D LLVM_USE_CRT_RELEASE=MT ^
    -D LLVM_CONFIG_PATH="%libroot%\llvm-build-release\bin\llvm-config.exe" ^
    -D LLVM_TABLEGEN_EXE="%libroot%\llvm-build-release\Release\bin\llvm-tblgen.exe" ^
    -D CMAKE_INSTALL_PREFIX="%libroot%\lld-install-release" ^
    "%libroot%\lld-6.0.1.src" || exit /b 1

:skip_gen
cmake --build . --config Release --target install || exit /b 1
