/*=========================================================================
 *
 * VBLLVM Project
 * kscope (c) 2018 by wqweto@gmail.com
 *
 * Kaleidoscope toy language for VBLLVM
 *
 * startup.c - Startup and runtime functions
 *
 *=========================================================================*/

// #pragma comment(lib, "kernel32")
extern void * __stdcall GetStdHandle(unsigned int nStdHandle);
extern int __stdcall WriteFile(void *HANDLE, const void *lpBuffer, unsigned int nNumberOfBytesToWrite, unsigned int *lpNumberOfBytesWritten, void *lpOverlapped);
extern void __stdcall ExitProcess(unsigned int exit_code);

extern double __anon_expr();
char *dtoa(double x, char *out);

#define STD_ERROR_HANDLE -12

double putchard(double x) {
    char ch;
    
    ch = x;
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), &ch, 1, 0, 0);
    return x;
}

double printd(double x) {
    char buf[32], *p;
    
    p = dtoa(x, buf);
    *p++ = '\r';
    *p++ = '\n';
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), buf, p-buf, 0, 0);
    return x;
}

char *dtoa(double x, char *out) {
    char buf[32], *p, *b;
    int i;
    
    if (x < 0) {
        *out++ = '-';
        x = -x;
    }
    /* whole part */
    p = buf;
    i = x;
    if (i == 0)
        *out++ = '0';
    else {
        for(; i != 0; i /= 10) {
            *p++ = i%10 + '0';
        }
        while(p > buf)
            *out++ = *(--p);
    }
    /* fraction part if any */
    p = buf;
    i = (x - (int)x) * 1000000;
    for(; i != 0; i /= 10) {
        *p++ = i%10 + '0';
    }
    for(b = buf; b < p; b++)
        if (*b != '0') {
            *out++ = '.';
            while(p > b)
                *out++ = *(--p);
            break;
        }
    *out = 0;
    return out;
}

void __runtime_main(void) {
    double res = 0;
    #ifndef TEST
        res = __anon_expr();
    #endif
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), "Evaluated to ", 13, 0, 0);
    printd(res);
    ExitProcess(res);
}

#ifdef TEST
int main() {
    printd(123.456);
    printd(-7890.456);
    printd(-0.123);
    return 0;
}
#endif
