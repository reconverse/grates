#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP seven_day_week_in_year(SEXP, SEXP);
extern SEXP as_grouped_week_from_date(SEXP, SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"seven_day_week_in_year",    (DL_FUNC) &seven_day_week_in_year,    2},
    {"as_grouped_week_from_date", (DL_FUNC) &as_grouped_week_from_date, 3},
    {NULL,                        NULL,                                 0}
};

void R_init_grates(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

