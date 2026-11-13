#define INTDIV(x,y) ((int) floor(((double) (x)) / ((double) (y))))

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

static SEXP make_yearweek_class(int firstday);
static SEXP _as_date_from_yearweek(SEXP x, const int first);

SEXP as_grouped_week_from_date(SEXP date, SEXP class, SEXP firstday)
{
    if (!Rf_inherits(date, "Date"))
        Rf_error("INTERNAL ERROR: C_as_grouped_week_from_date called on non-Date object.");

    if (!Rf_isNumeric(date))
        Rf_error("Non-numeric Date object.");

    int protected = 0;

    const int first = INTEGER_RO(firstday)[0];
    R_xlen_t size = XLENGTH(date);

    /* Note: If double we want to floor the input before coercing to integer
             This utilises the R API to handle invalid coercion */
    if (Rf_isReal(date)) {
        date = PROTECT(Rf_duplicate(date)); protected ++;
        double* xdate = REAL(date);
        for (R_xlen_t i = 0; i < size; i ++)
            xdate[i] = floor(xdate[i]);
        date = PROTECT(Rf_coerceVector(date, INTSXP)); protected ++;
    }

    SEXP out      = PROTECT(Rf_allocVector(INTSXP, size)); protected++;
    int* p_out = INTEGER(out);

    const int* p_date = INTEGER_RO(date);
    for (R_xlen_t i = 0; i < size; i++)
        p_out[i] = p_date[i] == NA_INTEGER ? NA_INTEGER : INTDIV(p_date[i] + 4 - first, 7);


    /* set class before returning */
    SEXP newClass;
    const char* classString = CHAR(STRING_ELT(class, 0));
    if (first == 1 && strcmp(classString, "grates_isoweek") == 0)
    {
        newClass = PROTECT(Rf_allocVector(STRSXP, 1)); protected++;
        SET_STRING_ELT(newClass, 0, Rf_mkChar("grates_isoweek"));
    }
    else if (first == 7 && strcmp(classString, "grates_epiweek") == 0)
    {
        newClass = PROTECT(Rf_allocVector(STRSXP, 1)); protected++;
        SET_STRING_ELT(newClass, 0, Rf_mkChar("grates_epiweek"));
    }
    else
    {
        newClass = PROTECT(make_yearweek_class(first)); protected++;
    }
    Rf_classgets(out, newClass);
    UNPROTECT(protected);
    return out;
}

SEXP as_date_from_yearweek(SEXP x, SEXP firstday)
{
    int first = INTEGER_RO(firstday)[0];
    return _as_date_from_yearweek(x, first);
}

SEXP as_date_from_epiweek(SEXP x)
{
    return _as_date_from_yearweek(x, 7);
}

SEXP as_date_from_isoweek(SEXP x)
{
    return _as_date_from_yearweek(x, 1);
}


/* ----------------------------------------------------------------------
 * Internals
 * ---------------------------------------------------------------------- */

static SEXP make_yearweek_class(int firstday)
{
    SEXP class = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 1, Rf_mkChar("grates_yearweek"));
    switch(firstday)
    {
    case 1:
        SET_STRING_ELT(class, 0, Rf_mkChar("grates_yearweek_monday"));
        break;

    case 2:
        SET_STRING_ELT(class, 0, Rf_mkChar("grates_yearweek_tuesday"));
        break;

    case 3:
        SET_STRING_ELT(class, 0, Rf_mkChar("grates_yearweek_wednesday"));
        break;

    case 4:
        SET_STRING_ELT(class, 0, Rf_mkChar("grates_yearweek_thursday"));
        break;

    case 5:
        SET_STRING_ELT(class, 0, Rf_mkChar("grates_yearweek_friday"));
        break;

    case 6:
        SET_STRING_ELT(class, 0, Rf_mkChar("grates_yearweek_saturday"));
        break;

    case 7:
        SET_STRING_ELT(class, 0, Rf_mkChar("grates_yearweek_sunday"));
        break;

    default:
        Rf_error("INTERNAL ERROR: Invalid firstday for grates_yearweek object.");
    }
    UNPROTECT(1);
    return class;
}

static SEXP _as_date_from_yearweek(SEXP x, const int first)
{
    R_xlen_t size = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
    int* p_out = INTEGER(out);
    const int* p_x = INTEGER_RO(x);
    for (R_xlen_t i = 0; i < size; i++)
        p_out[i] = p_x[i] == NA_INTEGER ? NA_INTEGER : p_x[i] * 7 + (first - 4);

    /* set class to "Date" before returning */
    Rf_classgets(out, Rf_mkString("Date"));
    UNPROTECT(1);
    return out;
}
