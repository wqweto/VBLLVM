# GenIdl
VBLLVM Project Typelib Generator

### Description

This folder contains a C/C++ headers PEG parser that extracts function declarations along with enums, structs and typedes from LLVM C API sources.

### Compilation

You'll need [VbPeg.exe](https://github.com/wqweto/VbPeg) parser generator to compile this parser's grammar -- just use `parser.bat` to compile `parser.peg` to `mdParser.bas`.

Once the parser `.bas` file is generated you can compile `gen_idl.vbp` project in VB6 IDE as usual.

### Usage

This tool is used by `build_tlb.bat` in parent folder to compile `VBLLVM.idl` (in parent folder).

    GenIdl 0.1 (c) 2018 by wqweto@gmail.com (10.7.2018 19:20:19)

    Usage: gen_idl.exe [options] <include_files> <include_dirs> ...

    Options:
      -o OUTFILE      output .idl file [default: stdout]
      -def DEFFILE    input .def file
      -json           dump includes parser result
      -types          dump types in use
