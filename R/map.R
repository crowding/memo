cached_digest <- function(...) digest(list(...), algo="md5")

.onLoad <- function(libname, pkgname) {
  cached_digest <<- memo(cached_digest, key=pointer_key)
}

#' A reference-valued, key-value store.
#'
#' [hashmap()] constructs a hashmap, which is an object that behaves
#' like an [environment] but can key on arbitrary objects rather than
#' just characters.
#'
#' You can use multiple indices in a hashmap; the effect is similar to
#' indexing on a list containing all keys.
#'
#' Type is significant; for instance, float `1` and integer `1L` are
#' considered distinct indices. It is also permitted to index on NULL,
#' NA, or the empty string.
#'
#' The `memo` package hashmap has a performance optimization over
#' other implementations of this concept, in that the md5 digest is
#' memoized on scalar and pointer values. That means that if you
#' lookup using keys that are pointer-identical to previously seen
#' keys, it will skip computing the digest a second time. Indexing
#' using scalar values will also bypass the md5 hash.
#'
#' @return `hashmap()` returns a newly constructed hashmap.
#' @author Peter Meilstrup
#' @export
hashmap <- function() {
  structure(
    list(
      keys=new.env(parent=emptyenv()),
      vals=new.env(parent=emptyenv()),
      digest=cached_digest),
    class="hashmap")
}

#' @exportS3Method "[" hashmap
#' @rdname hashmap
#' @param x a hashmap object.
`[.hashmap` <- function(x, ...) {
  mapply(`[[.hashmap`, ..., MoreArgs=list(x=x), SIMPLIFY=FALSE)
}

#'  The `[` and `[<-` methods work in terms of a list formed by
#'   iterating over the given indices in parallel; for instance
#'   `x[c(2, 8), c(3, 9)]` will be equivalent to `list(x[[2, 3]],
#'   x[[3, 9]])`.
#' @param value A replacement value for `[[`; for '[', a
#'   sequence of replacement values.
#' @rdname hashmap
#' @exportS3Method "[<-" hashmap
`[<-.hashmap` <- function(x, ..., value) {
  mapply(`[[<-.hashmap`, ..., value=value, MoreArgs=list(x=x), SIMPLIFY=FALSE)
  x
}

#' @exportS3Method "[[" hashmap
#' @rdname hashmap
`[[.hashmap` <- function(x, ...) {
  digestfn <- x$digest
  dig <- digestfn(...) # just writing x$digest(...) makes CRAN check complain???
  if (exists(dig, envir=x$keys)) {
    stopifnot(identical(x$keys[[dig]], list(...)))
    x$vals[[dig]]
  } else NULL
}

#' @exportS3Method "[[<-" hashmap
#' @rdname hashmap
`[[<-.hashmap` <- function(x, ..., value) {
  digestfn <- x$digest
  dig <- digestfn(...) # just writing x$digest(...) makes CRAN check complain???
  x$keys[[dig]] <- list(...)
  x$vals[[dig]] <- value
  x
}

#' @export
#' @rdname hashmap
keys <- function(x, ...) UseMethod("keys")

#' @exportS3Method
keys.hashmap <- function(x, ...) {
  lapply(sort(names(x$keys)), function(k) x$keys[[k]])
}

#' @export
#' @rdname hashmap
values <- function(x, ...) UseMethod("values")

#' @exportS3Method
values.hashmap <- function(x, ...) {
  lapply(sort(names(x$keys)), function(k) x$vals[[k]])
}

#' @export
#' @return `pairs(x)` extracts from a hashmap a list of pairs, each
#'   pair being of the form `list(key=, val=)`.
#' @rdname hashmap
to_pairs <- function(x, ...) UseMethod("to_pairs")

#' @exportS3Method
to_pairs.hashmap <- function(x, ...) {
  lapply(sort(names(x$keys)), function(k) list(key=x$keys[[k]], value=x$vals[[k]]))
}

#' @export
#' @param pairs A list of pairs, the first element is treated as key
#'   and the second as value.
#' @rdname hashmap
from_pairs <- function(pairs) {
  hm <- hashmap()
  lapply(pairs, function(x)  {
    dig <- hm$digest(x[[1]])
    hm$keys[[dig]] <- x[[1]]
    hm$vals[[dig]] <- x[[2]]
  })
  hm
}

#' @export
#' @rdname hashmap
#' @param ... Any number of indices.
#' @return `hasKey(x)` returns TRUE if there is a key with the same
#'   digest as `...` that compares [identical()]
hasKey <- function(x, ...) UseMethod("hasKey")

#' @exportS3Method
hasKey.hashmap <- function(x, ...) {
  digest <- x$digest # CRAN complains about x$digest(...)
  exists(digest(...), envir=x$keys)
}

#' The base R behavior of deleting keys using `x[[key]] <- NULL` is
#' explicitly _not_ supported. Instead, use `dropKey(x, ...)`.
#' @rdname hashmap
#' @export
dropKey <- function(x, ...) UseMethod("dropKey")

#' @exportS3Method
dropKey.hashmap <- function(x, ...) {
  digest <- x$digest # why does CRAN complain about x$digest(...)
  dig <- digest(...)
  if (exists(dig, envir=x$keys)) {
    rm(list=dig, envir=x$keys)
    rm(list=dig, envir=x$vals)
  }
  invisible(hashmap)
}
