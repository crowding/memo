#' Memoize a function based on the raw pointers of its arguments.
#' @export
#' @param fn A function to wrap. It should be a pure function (i.e. it
#' should not cause side effects, and should not depend on any
#' variables that may change.)
#' @param cache A cache to use. Defaults to a new instance of \code{\link{lru_cache}}.
#' Caches may be shared between memoized functions.
memo <- function(fn, cache=lru_cache(1000), key=digest_key) {
  force(fn)

  # is this delayedAssign necessary? Because package is loaded before DLL?
  delayedAssign("fn_digest", key(list(fn)))

  function(...) {
    digest <- key(list(...))
    digest <- paste(c(fn_digest, digest), collapse=";")
    cache(digest, fn(...))
  }
}

#' Report statistics on cache utilization.
#'
#' @param fn A memoized function (such as created by \code{\link{memo}})
#' @return A list with entries "hits", "misses", "expired" and "entries"
#' counting how many times the cache has found a previously saved result,
#' computed a new result, and dropped an old result from memory, and the number
#' of entries currently in the cache.
#' @export
cache_stats <- function(fn) {
    hitdata <- mget(c("size", "used", "hits", "misses", "expired"),
                    environment(environment(fn)$cache))
  as.list(hitdata)
  }

#' Memoize based on pointer equivalence.
#'
#' This may be used as an argument to 'memo' to use pointer
#' equivalence instead of a computed hash to index the cache.
#' @param x A list of objects.
#' @return A string encoding the memory locations of each item in the
#' list.
pointer_key <- function(x) {
  object_pointers(x)
}

#' Memoize based on object serialization.
#'
#' This is the default key type for \code{\link{memo}}.
#' @param x A list of objects.
#' @return A string based on a hash of the object contents.
digest_key <- function(x) {
  digest(x)
}
