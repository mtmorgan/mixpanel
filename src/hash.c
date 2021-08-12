#include <limits.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* 'djb2' hash, e.g., http://www.cse.yorku.ca/~oz/hash.html */

SEXP hash_impl(SEXP txt) {
    const unsigned char *str = (unsigned char *) CHAR(STRING_ELT(txt, 0));
    unsigned long hash = 5381;
    int c;
    char hex[9];

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    sprintf(hex, "%06x", (unsigned int) (hash % 16777216L));

    return Rf_ScalarString(mkChar(hex));
}

/* make available to R */

static const R_CallMethodDef callMethods[] = {
    { ".hash_impl", (DL_FUNC) & hash_impl, 1 },
    { NULL, NULL, 0 }
};

void R_init_mixpanel(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}

void R_unload_mixpanel(DllInfo *info) {
    (void) info;
}
