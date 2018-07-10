@echo off
setlocal
set prj_root=%~dp0..\..
set gen_idl=%~dp0gen_idl\gen_idl.exe
set idl_file=%~dp0VBLLVM.idl

%gen_idl% %prj_root%\lib\llvm-6.0.0.src\include\llvm-c -def %prj_root%\src\VBLLVM.def -o %idl_file%
mktyplib %idl_file%
