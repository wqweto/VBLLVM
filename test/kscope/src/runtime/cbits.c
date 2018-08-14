/* Minimum needed from stdio.h */
struct _IO_FILE { };
typedef struct _IO_FILE FILE;
/* Standard streams.  */
extern struct _IO_FILE *stdin;		/* Standard input stream.  */
extern struct _IO_FILE *stdout;		/* Standard output stream.  */
extern struct _IO_FILE *stderr;		/* Standard error output stream.  */
/* C89/C99 say they're macros.  Make them happy.  */
#define stdin stdin
#define stdout stdout
#define stderr stderr
extern int putchar(int ch);
extern int printf (const char *__format, ...);
extern int fflush(FILE *file);

extern double __anon_expr();

double 
putchard(double X) {
  putchar((char)X);
  fflush(stdout);
  return 0;
}

double 
printd(double X) {
  printf("%g\n", X);
  return 0;
}

int 
main(int ac, char **av) {
    double res = 0;
    
    res = __anon_expr();
    printf("Evaluated to %g\n", res);
    return res;
}

/* silence Scrt1.o startup/tear down */
void
__libc_csu_init(void) {}

void
__libc_csu_fini(void) {}
