#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

// both inputs known integer and same length
SEXP seven_day_week_in_year(SEXP date, SEXP jan1)
{
    R_xlen_t size = XLENGTH(date);
    SEXP out      = PROTECT(Rf_allocVector(INTSXP, size));
    const int* p_date = INTEGER_RO(date);
    const int* p_jan1 = INTEGER_RO(jan1);
    int* p_out = INTEGER(out);

    for (R_xlen_t i = 0; i < size; i++)
    {
        if (p_date[i] == NA_INTEGER || p_jan1[i] == NA_INTEGER)
        {
            p_out[i] = NA_INTEGER;
        }
        else
        {
            int diff = p_date[i] - p_jan1[i];
            p_out[i] = 1 + floor((double) diff / 7);
        }
    }

    UNPROTECT(1);
    return out;
}


