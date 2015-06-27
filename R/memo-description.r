#' In-memory caching of repeated computations, by pointer equivalence.
#'
#' This package implements a cache that can be used to avoid repeated
#' computations of functions. The cache lookup is based on object
#' identity (i.e.  pointer equivalence) which is suited for functions
#' like accessors or other functions that are called repeatedly on the
#' same object.  Description of memo goes here.
#'
#' @name memo
#' @docType package
#' @author Peter Meilstrup
NULL
