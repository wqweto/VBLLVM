#include <llvm-c/Linker.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/ADT/Triple.h>

#include <lld/Common/Driver.h>

using namespace llvm;

class MyOStream: public raw_ostream {
public:
    MyOStream(LLVMLLDLinkDiagnosticHandler _append_diagnostic, void *_context) :
        raw_ostream(true), 
        append_diagnostic(_append_diagnostic), 
        context(_context), 
        pos(0) { }

    void write_impl(const char *ptr, size_t len) override
    {
        if (append_diagnostic)
            append_diagnostic(context, ptr, len);
        pos += len;
    }

    uint64_t current_pos() const override
    {
        return pos;
    }

    LLVMLLDLinkDiagnosticHandler append_diagnostic;
    void *context;
    size_t pos;
};


LLVMBool LLVM_STDCALL LLVMLLDLink(LLVMObjectFormatType ObjFormat, const char *ArgsMultiSz, 
                                  LLVMLLDLinkDiagnosticHandler Handler, void *DiagnosticContext)
{
    const char *args[1024];
    size_t count = 0;

    while (*ArgsMultiSz) {
        if (count >= _countof(args))
            llvm_unreachable("too many args");
        args[count++] = ArgsMultiSz;
        ArgsMultiSz += strlen(ArgsMultiSz) + 1;
    }
    ArrayRef<const char *> array_ref_args(args, count);
    MyOStream diag(Handler, DiagnosticContext);
    switch (ObjFormat) {
    case Triple::UnknownObjectFormat:
        return 0;
    case Triple::COFF:
        return lld::coff::link(array_ref_args, false, diag);
    case Triple::ELF:
        return lld::elf::link(array_ref_args, false, diag);
    case Triple::MachO:
        return lld::mach_o::link(array_ref_args, diag);
    case Triple::Wasm:
        return lld::wasm::link(array_ref_args, false, diag);
    }
    llvm_unreachable("unknown object format");
}

LLVMObjectFormatType LLVM_STDCALL LLVMGetObjectFormatFromTriple(const char *Triple)
{
    llvm::Triple triple(Triple);
    return (LLVMObjectFormatType)triple.getObjectFormat();
}
