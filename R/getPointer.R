# Return some canonical identifying strings for each of a list of objects.
# For scalars, the values are directly represented.
# For scalar strings, we use the pointer to the interned CHARSXP.
# For other objects, we use the pointer.
# If the list has names, we represent store STRSXP pointers to the names
# Because it is possible for objects-pointed-to to be GCed and replaced with
# different objects, the calling code is responsible for holding on to
# references to the objects (see test-cache.R).
#' @useDynLib memo _string_reps
string_reps <- function(list) {
  .Call(`_string_reps`, list)
}
