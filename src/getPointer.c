#include "vadr.h"

SEXP stringify_item(SEXP, char *);
int sprintdouble(char *, double);

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L && !defined(__MINGW32__)

int sprintdouble(char *buf, double arg) {
  return sprintf(buf, "%la", arg);
}

#else

/* hack for stdlibs that don't support "%a" printf conversion */
int sprintdouble(char *buf, double arg) {
  int written = 0;
  union {
    double f;
    unsigned char ch[sizeof(double) / sizeof(char)];
  } data;
  data.f = arg;

  for (int i = 0; i < sizeof(double) / sizeof(char); i++) {
    int chars = sprintf(buf, "%02x", data.ch[i]);
    written += chars;
    buf += chars;
  }
  return written;
}

#endif



/* Return some canonical identifying strings for each of a list of objects.
 * For numeric scalars, the values are directly represented.
 * For scalar strings, we use the pointer to the interned CHARSXP.
 * For other objects, we use the pointer, after setting NAMED=2.
 * If the list has names, we represent the pointer values on the names.
 * Because it is possible for objects-pointed-to to be GCed and replaced with 
 * different objects, the calling code is responsible for holding on to
 * references to the objects (see test-cache.R).
 */
SEXP _string_reps(SEXP list) {
  assert_type(list, VECSXP); 

  int length = LENGTH(list);
  SEXP in_names = PROTECT(getAttrib(list, R_NamesSymbol));
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
    /* worst case, element has a tag and is a CLOSXP, so 4 pointers and change:
       "c0x0123456789abcdef=c_0x0123456789abcdef/0x0123456789abcdef/0x01234567abcdef/0x01234567abcdef" = 97 chars*/
    char buf[128];
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

  UNPROTECT(2);
  return(out_reps);
}

/* Construct a string identifying some SEXP, either as a scalar value or as a pointer.
   If we use its pointer, mark the item immutable.
   Return that pointer, or R_NilValue. */
SEXP stringify_item(SEXP item, char *bufptr) {
  int done = 0;
  PROTECT_INDEX ix;
  SEXP item_ptr;
  PROTECT_WITH_INDEX(item_ptr = R_NilValue, &ix);
  while(!done) {
    switch (TYPEOF(item)) {
    case PROMSXP:
      /* if we have a promise, drill down. */
      item = PRCODE(item);
      break;
    case CHARSXP:
      /* interned string, represent its pointer */
      REPROTECT(item_ptr = item, ix);
      bufptr += sprintf(bufptr, "c%p", CHAR(item_ptr));
      done = 1;
      break;
    case REALSXP:
    case INTSXP:
    case STRSXP:
    case LGLSXP:
      /* we have a code literal. represent it canonically,
         and don't hold a ref to a scalar. */
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
        case REALSXP:
          bufptr += sprintf(bufptr, "r");
          bufptr += sprintdouble(bufptr, REAL(item)[0]);
          break;
        case INTSXP: bufptr += sprintf(bufptr, "i%x", INTEGER(item)[0]); break;
        case LGLSXP: bufptr += sprintf(bufptr, "l%x", LOGICAL(item)[0]); break;
        case STRSXP:
          REPROTECT(item_ptr = STRING_ELT(item, 0), ix);
          bufptr += sprintf(bufptr, "s%p", CHAR(item_ptr)); break;
        default: error("Unexpected type %s (this shouldn't happen)", TYPEOF(item));
        }
      } else {
        /* for non-scalar vectors, represent the pointer */
        REPROTECT(item_ptr = item, ix);
        bufptr += sprintf(bufptr, "v%p", (void *)item_ptr);
      }
      done = 1;
      break;
    case VECSXP:
      REPROTECT(item_ptr = item, ix);
      bufptr += sprintf(bufptr, "l%p", (void *)item_ptr);
      done = 1;
      break;
    case CLOSXP:
      REPROTECT(item_ptr = item, ix);
      bufptr += sprintf(bufptr, "c_%p/%p/%p",
                        (void *) FORMALS(item),
                        (void *) BODY(item),
                        (void *) CLOENV(item));
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
      REPROTECT(item_ptr = item, ix);
      bufptr += sprintf(bufptr, "e%p", (void *)item_ptr);
      done = 1;
      break;
    default:
      error("Unexpected type %s", type2char(TYPEOF(item)));
    }
  }
  if (item_ptr != R_NilValue) {
    MARK_NOT_MUTABLE(item_ptr);
  }
  UNPROTECT(1);
  return item_ptr;
}

/*
-*- previewing-build-command: '(previewing-run-R-unit-tests)
 */
