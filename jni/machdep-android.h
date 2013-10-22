#if !defined(S_IREAD) && !defined(S_IWRITE) && !defined(S_IEXEC)
#define S_IREAD S_IRUSR
#define S_IWRITE S_IWUSR
#define S_IEXEC S_IXUSR
#endif
