@echo off
setlocal
call "%VS140COMNTOOLS%..\..\VC\bin\vcvars32.bat"
set path=%ProgramFiles%\CMake\bin;%LOCALAPPDATA%\Programs\Python\Python36-32;%PATH%
set libroot=%~dp0..

if exist LLVM.sln goto :skip_gen
cmake -G "Visual Studio 14" -D LLVM_TARGETS_TO_BUILD=X86 ^
    -D LLVM_USE_CRT_DEBUG=MTd -D LLVM_USE_CRT_RELEASE=MT ^
    -D LLVM_ENABLE_RTTI=ON -D LLVM_ENABLE_TERMINFO=OFF -D LLVM_ENABLE_DUMP=ON ^
    -D CMAKE_INSTALL_PREFIX="%libroot%\llvm-install-release" ^
    "%libroot%\llvm-6.0.0.src" || exit /b 1

:skip_gen
cmake --build . --config Release --target install || exit /b 1
