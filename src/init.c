#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "vadr.h"

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

static const R_CallMethodDef CallEntries[] = {
  {"_string_reps", (DL_FUNC) &_string_reps, 2},
  {NULL, NULL, 0}
};

void R_init_msgpack(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
