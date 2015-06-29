#' Memoize a function based on the raw pointers of its arguments.
#' @export
#' @param fn A function to wrap. It should be a pure function (i.e. it
#' should not cause side effects, and should not depend on any
#' variables that may change.)
#' @param cache A cache to use. Defaults to a new instance of \code{\link{lru_cache}}.
#' Caches may be shared between memoized functions.
memo <- function(fn, cache=lru_cache(5000), key=digest_key) {
  force(fn)

  # is this delayedAssign necessary? Because package is loaded before DLL?
  key(fn, cache)
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

#' Strategies for caching items.
#'
#' The function \code{\link{memo}} accepts an argument `key` which
#' specifies the keying strategy.
#'
#' @param fn A function whose results should be cached.
#' @param cache A cache object.
#' @return A memoized function.
#' @name strategies
NULL

#' \code{digest_key} is the default key strategy. It computes a key by
#' hashing the contents of the object using the digest package. No
#' attempt is made to avoid MD5 hash collisions.
#' @rdname strategies
#' @export
digest_key <- function(fn, cache) {
  delayedAssign("fn_digest", digest(fn))
  function(...) {
    digest <- paste0(c(fn_digest, digest(list(...))), collapse=";")
    cache(digest, fn(...))
  }
}

#' The \code{pointer_key} strategy bases its key on object identity,
#' (that is, pointer equivalence.)  This can be faster because hte
#' entire object need not be hashed. However, this strategy is only
#' useful when the function is called on the same object repeatedly
#' and that object is not copied.
#' @rdname strategies
#' @export
pointer_key <- function(fn, cache) {
  delayedAssign("fn_digest", object_pointers(list(fn)))
  function(...) {
    digest <- paste0(c(fn_digest, object_pointers(list(...))), collapse=";")
    cache(digest, fn(...))
  }
}

#' The \code{hybrid_key} strategy first tries to key on object
#' identity and then falls back on computing the md5 digest. Note that
#' this method uses twice as many cache slots, since the MD5 digest
#' results are cached.
#' @rdname strategies
hybrid_key <- function(fn, cache) {
  delayedAssign("fn_digest", digest(fn))
  function(...) {
    l = list(...)
    predigest <- paste0(fn_digest, object_pointers(l), collapse=";")
    digest <- cache(predigest, paste0(fn_digest, digest(l), collapse=";"))
    cache(digest, fn(...))
  }
}
