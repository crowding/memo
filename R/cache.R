#Cache for macro expansions
global_cache <- NULL

#' Memoize a function based on the raw pointers of its arguments.
#' @export
#' @param fn A function to wrap. It should be a pure function (i.e. it
#' should not cause side effects, and should not depend on any
#' variables that may change.)
#' @param cache A cache to use. Defaults to a new instance of \code{\link{lru_cache}}.
memo <- function(fn, cache=lru_cache(1000)) {
  force(fn)

  # is this delayedAssign necessary? Because package is loaded before DLL?
  delayedAssign("fn_pointer", object_pointers(list(fn)))

  function(...) {
    digest <- object_pointers(list(...))
    key <- paste(c(fn_pointer, digest), collapse=";")
    cache(key, fn(...))
  }
}

#' Report statistics on cache utilization.
#'
#' @param cache A cache function (such as created by \code{\link{lru_cache}})
#' @return A list with entries "hits", "misses", "expired" and "entries"
#' counting how many times the cache has found a previously saved result,
#' computed a new result, and dropped an old result from memory, and the number
#' of entries currently in the cache.
#' @export
cache_stats <- function(cache=global_cache) {
  hitdata <- mget(c("hits", "misses", "expired", "entries"), environment(cache))
  as.list(hitdata)
}

.onLoad <- function(...) {
  global_cache <<- lru_cache(size=10000)
}
