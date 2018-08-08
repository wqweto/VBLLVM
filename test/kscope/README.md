## LLVM's Kaleidoscope toy language VB6 port

This test project ports to VBLLVM the [Kaleidoscope toy language](https://llvm.org/docs/tutorial/index.html) as implemented in the original LLVM's tutorial.

Final `kscope.exe` implements both a cross-compiler (x64 and x86) and MCJIT-ed interpreter.

### How to compile grammar

Use `compile_parser.bat` to compile Kaleidoscope's parser from `kscope.peg` to `src/cParser.cls`.

This batch file needs `vbpeg.exe` from [VbPeg project](https://github.com/wqweto/VbPeg/releases) version 0.3.12 at least.

### Command line

    C:> kscope.exe
    kscope VB6 port 0.1, i686-pc-windows-msvc (c) 2018 by wqweto@gmail.com (7.8.2018 12:25:28)

    Usage: kscope.exe [options] <in_file.ks>

    Options:
      -o OUTFILE      write result to OUTFILE [default: stdout]
      -c, -emit-obj   compile to COFF .obj file only [default: exe]
      -m32            compile for win32 target [default: x64]
      -emit-tree      output parse tree
      -emit-llvm      output intermediate represetation
      -q              in quiet operation outputs only errors
      -nologo         suppress startup banner
      -version        dump available targets
    If no -emit-xxx is used emits executable. If no -o is used writes result to console.