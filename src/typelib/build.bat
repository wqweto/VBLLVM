@echo off
setlocal
set prj_root=%~dp0..\..
set gen_idl=%prj_root%\lib\gen_idl\gen_idl.exe
set idl_file=%~dp0VBLLVM.idl

if [%1]==[tlb] goto make_tlb
%gen_idl% %prj_root%\lib\llvm-6.0.0.src\include\llvm-c -def %prj_root%\src\VBLLVM.def -o %idl_file%
:make_tlb
mktyplib %idl_file% /tlb %prj_root%\bin\typelib\VBLLVM.tlb
