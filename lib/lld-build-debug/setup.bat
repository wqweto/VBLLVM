@echo off
setlocal
call "%VS140COMNTOOLS%..\..\VC\bin\vcvars32.bat"
set path=%ProgramFiles%\CMake\bin;%LOCALAPPDATA%\Programs\Python\Python36-32;%PATH%
set libroot=%~dp0..

md "%libroot%\llvm-build-debug\bin" >nul 2>&1
copy "%libroot%\llvm-build-debug\Debug\bin\llvm-config.exe" "%libroot%\llvm-build-debug\bin\llvm-config.exe" >nul 2>&1

if exist lld.sln goto :skip_gen
cmake -G "Visual Studio 14" -D LLVM_TARGETS_TO_BUILD=X86 ^
    -D LLVM_USE_CRT_DEBUG=MTd -D LLVM_USE_CRT_RELEASE=MT ^
    -D LLVM_CONFIG_PATH="%libroot%\llvm-build-debug\bin\llvm-config.exe" ^
    -D LLVM_TABLEGEN_EXE="%libroot%\llvm-build-debug\Debug\bin\llvm-tblgen.exe" ^
    -D CMAKE_INSTALL_PREFIX="%libroot%\lld-install-debug" ^
    "%libroot%\lld-6.0.1.src" || exit /b 1

:skip_gen
cmake --build . --config Debug --target install || exit /b 1
