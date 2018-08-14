### How to compile kscope runtimes

The `build.bat` depends on clang being installed in `%ProgramFiles(x86)%\LLVM` beforehand but you can adjust the path to the compiler if different.

The `build.bat` tries to compile runtimes for all targets (windows/linux + i386/x86_64) to their resepective `bin/lib/<os>/<arch>` output folders.

### Hot to compile runtimes for linux targets under WSL

This is an alternative build for the linux runtime. Note that this step is not needed anymore.

    $ gcc -m32 -c cbits.c -o ../../bin/lib/linux/i386/cbits.o
    $ gcc -c cbits.c -o ../../bin/lib/linux/x86_64/cbits.o
    