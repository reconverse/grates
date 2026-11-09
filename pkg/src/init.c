#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP as_grouped_week_from_date(SEXP, SEXP, SEXP);
extern SEXP as_date_from_isoweek(SEXP);
extern SEXP as_date_from_epiweek(SEXP);
extern SEXP as_date_from_yearweek(SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"as_grouped_week_from_date", (DL_FUNC) &as_grouped_week_from_date, 3},
    {"as_date_from_isoweek",      (DL_FUNC) &as_date_from_isoweek,      1},
    {"as_date_from_epiweek",      (DL_FUNC) &as_date_from_epiweek,      1},
    {"as_date_from_yearweek",     (DL_FUNC) &as_date_from_yearweek,     2},
    {NULL,                        NULL,                                 0}
};

void R_init_grates(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
