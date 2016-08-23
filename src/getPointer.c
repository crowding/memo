#include "vadr.h"

SEXP stringify_item(SEXP, char *);
SEXP _string_reps(SEXP);

/* Return some canonical identifying strings for each of a list of objects.
 * For scalars, the values are directly represented.
 * For scalar strings, we use the pointer to the interned CHARSXP.
 * For other objects, we use the pointer.
 * If the list has names, we represent store STRSXP pointers to the names
 * Because it is possible for objects-pointed-to to be GCed and replaced with 
 * different objects, the calling code is responsible for holding on to
 * references to the objects (see test-cache.R).
 */
SEXP _string_reps(SEXP list) {
  assert_type(list, VECSXP); 

  int length = LENGTH(list);
  SEXP in_names = getAttrib(list, R_NamesSymbol);
  SEXP out_reps = PROTECT(allocVector(STRSXP, length));
  SEXP out_names;
  
  if (in_names != R_NilValue) {
    assert_type3(in_names, STRSXP, "names attribute should be a character vector");
    if (LENGTH(in_names) < length) {
      in_names = R_NilValue;
      out_names = R_NilValue;
    } else {
      PROTECT(out_names = allocVector(STRSXP, length));
    }
  } else {
    out_names = R_NilValue;
  }

  for (int i = 0; i < length; i++) {
    SEXP item = VECTOR_ELT(list, i);
    char buf[99]; /* worst case, element has a name and is a CLOSXP;
                     "c01234567=c_01234567/01234567/01234567/01234567" */
    char *bufptr = buf;

    if (in_names != R_NilValue) {
      SEXP name = STRING_ELT(in_names, i);
      if (name != R_BlankString) {
        bufptr += sprintf(bufptr, "c%p=", R_CHAR(name));
      }
    }

    stringify_item(item, bufptr);
    SET_STRING_ELT(out_reps, i, mkChar(buf));
  }

  if (in_names != R_NilValue) {
    setAttrib(out_names, R_NamesSymbol, in_names);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return(out_reps);
}

/* Construct a string identifying some SEXP, either as a scalar value or as a pointer. */
SEXP stringify_item(SEXP item, char *bufptr) {
  int done = 0;
  SEXP result = R_NilValue;
  while(!done) {
    switch (TYPEOF(item)) {
    case PROMSXP:
      /* if we have a promise, drill down. */
      item=PRCODE(item);
      break;
    case CHARSXP:
      /* interned string, represent its pointer */
      bufptr += sprintf(bufptr, "c%p", CHAR(item)); break;
      result = item;
    case REALSXP:
    case INTSXP:
    case STRSXP:
    case LGLSXP:
      /* we have a code literal. represent it canonically,
         and don't hold a ref to a scalar. */
      result = R_NilValue;
      if (LENGTH(item) == 0) {
        switch(TYPEOF(item)) {
        case REALSXP: bufptr += sprintf(bufptr, "r0"); break;
        case INTSXP: bufptr +=  sprintf(bufptr, "i0"); break;
        case LGLSXP: bufptr += sprintf(bufptr, "l0"); break;
        case STRSXP: bufptr += sprintf(bufptr, "s0"); break;
        default: error("Unexpected type %s (this shouldn't happen)", TYPEOF(item));
        }
      } else if (LENGTH(item) == 1) {
        switch(TYPEOF(item)) {
        case REALSXP: bufptr += sprintf(bufptr, "r%la", REAL(item)[0]); break;
        case INTSXP: bufptr += sprintf(bufptr, "i%x", INTEGER(item)[0]); break;
        case LGLSXP: bufptr += sprintf(bufptr, "l%x", LOGICAL(item)[0]); break;
        case STRSXP:
          bufptr += sprintf(bufptr, "s%p", CHAR(STRING_ELT(item, 0))); break;
          result = STRING_ELT(item, 0);
        default: error("Unexpected type %s (this shouldn't happen)", TYPEOF(item));
        }
      } else {
        /* for longer values, represent the pointer */
        bufptr += sprintf(bufptr, "v%p", (void *)item);
        result = item;
      }
      done = 1;
      break;
    case VECSXP:
      bufptr += sprintf(bufptr, "l%p", (void *)item);
      result = item;
      done = 1;
      break;
    case CLOSXP:
      bufptr += sprintf(bufptr, "c_%p/%p/%p",
                        (void *) FORMALS(item),
                        (void *) BODY(item),
                        (void *) CLOENV(item));
      result=item;
      done = 1;
      break;
    case SYMSXP:
    case LANGSXP:
    case EXPRSXP:
    case BCODESXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case NILSXP:
      /* We have an expression-ish, represent its pointer. */
      bufptr += sprintf(bufptr, "e%p", (void *)item);
      result = item;
      done = 1;
      break;
    default:
      error("Unexpected type %s", type2char(TYPEOF(item)));
    }
  }
  SET_NAMED(result, 2);
  return result;
}

/*
-*- previewing-build-command: '(previewing-run-R-unit-tests)
 */
