#' In-memory caching of repeated computations, by pointer equivalence.
#'
#' The `memo` package implements a cache that can be used to avoid repeated
#' computations of functions. The cache lookup is based on object
#' identity (i.e.  pointer equivalence) which is suited for functions
#' like accessors or other functions that are called repeatedly on the
#' same object.
#'
#' @aliases memo-package
#' @author Peter Meilstrup
"_PACKAGE"
