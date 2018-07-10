# VBLLVM
LLVM 6.0 bindings for VB6

### Description

This repo contains a `stdcall` port of LLVM C API exports to be accessible from VB6 projects. This build uses `/MT` and `/MTd` (static) runtimes and `Windows XP (v140_xp)` platform toolset in Visual Studio 2015 for the produced DLLs to be portable and self-contained.

### LLVM preparation

LLVM sources are extracted in `lib/llvm-6.0.0.src` and `stdcall` patch is applied in commit [`cd1946d`](https://github.com/wqweto/VBLLVM/commit/cd1946dfd8e83cc7ddc7e84d277cffd01f716712).

LLVM build uses [cmake](https://cmake.org/download/), [python3.x](https://www.python.org/downloads/) and Visual Studio 2015 so make sure you have these pre-installed.

Use `lib/build-release/setup.bat` to compile LLVM release build in `lib/install-release`.

Use `lib/build-debug/setup.bat` to compile LLVM debug build in `lib/install-debug`.

Use `lib/install-release/lib/dump-def.bat` to collect exports for `src/VBLLVM.def`

### Source usage

Use `src/VBLLVM.sln` to build `bin/Release/VBLLVM.dll` and `bin/Debug/VBLLVM.dll` from `.lib` files in `lib/install-release` and `lib/install-debug`.
