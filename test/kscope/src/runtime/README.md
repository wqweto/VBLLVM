### Compile runtime

The batch `build.bat` depends on `%ProgramFiles(x86)%\LLVM\bin\clang.exe` being installed beforehand. You can adjust the path in the batch if needed.

Runtime build compiles all runties (windows/linux and i386/x86_64) to `bin/lib/<os>/<arch>` output folders.

### Compile linux runtime under WSL

This is an alternative build for the linux runtime. Note: this step is not needed anymore

    $ gcc -m32 -c cbits.c -o ../../bin/lib/linux/i386/cbits.o
    $ gcc -c cbits.c -o ../../bin/lib/linux/x86_64/cbits.o
    