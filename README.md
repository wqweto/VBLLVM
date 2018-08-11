# VBLLVM
LLVM 6.0.0 and lld 6.0.1 bindings for VB6

### Description

This repo contains a `stdcall` port of LLVM-C API exports to be accessible from VB6 projects. This build uses `/MT` and `/MTd` (static) runtimes and `Windows XP (v140_xp)` platform toolset in Visual Studio 2015 for the produced DLLs to be portable and self-contained.

### LLVM and lld preparation

LLVM sources are extracted in `lib/llvm-6.0.0.src` and `stdcall` patch is applied in commits [`cd1946d`](https://github.com/wqweto/VBLLVM/commit/cd1946dfd8e83cc7ddc7e84d277cffd01f716712) and [`e1e1356`](https://github.com/wqweto/VBLLVM/commit/e1e1356a6ac7592398937ac000fdde7e5a7d8670). LLVM's lld sources are extracted in `lib/lld-6.0.1.src` with no modifications applied.

LLVM build uses [cmake](https://cmake.org/download/), [python3.x](https://www.python.org/downloads/) and Visual Studio 2015 so make sure you have these pre-installed.

Use `lib/llvm-build-release/setup.bat` to compile LLVM release build in `lib/llvm-install-release`.

Use `lib/llvm-build-debug/setup.bat` to compile LLVM debug build in `lib/llvm-install-debug`.

Use `lib/lld-build-release/setup.bat` to compile LLVM's lld release build in `lib/lld-install-release`.

Use `lib/lld-build-debug/setup.bat` to compile LLVM's lld debug build in `lib/lld-install-debug`.

Finally use `lib/llvm-install-release/lib/dump-def.bat` to collect exports for `src/VBLLVM.def`

### Source usage

Use `src/VBLLVM.sln` to build `bin/release/VBLLVM.dll` and `bin/debug/VBLLVM.dll` from `.lib` files in `lib/llvm-install-release`;`lib/lld-install-release` and `lib/llvm-install-debug`;`lib/lld-install-debug`.

Finally use `src/typelib/build.bat` to build `bin/typelib/VBLLVM.tlb` from `lib/llvm-6.0.0.src/include/llvm-c` and `src/VBLLVM.def`

### Samples

 - LLVM's Kaleidoscope toy language VB6 port is in `test/kscope`
