#' Memoize a function.
#' @param fn A function to wrap. It should be a pure function (i.e. it should
#'   not cause side effects, and should not depend on any variables that may
#'   change.) It should not be a nonstandard-evaluating function. All arguments
#'   will be forced by the wrapper.
#' @param cache A cache to use. Defaults to a new instance of
#'   \code{\link{lru_cache}}.  Caches may be shared between memoized functions.
#' @param key A hashing strategy. The default "\code{\link{hybrid_key}}"
#' first checks for pointer equivalence and then falls back to using a
#' hash of the arguments. `pointer_key` uses just pointer equivalence,
#' and `digest_key` always performs a hash.
#' @param ... Further arguments passed on to key.
#' @export
memo <- function(fn, cache=lru_cache(5000), key=hybrid_key, ...) {
  force(fn)
  key <- match.fun(key)
  key(fn, cache, ...)
}

#' Report cache statistics.
#'
#' @param fn A memoized function that was created by \code{\link{memo}}.
#' @return A list with labels "size", "used", "hits", "misses", "expired"
#' counting the number of slots in the cache, the number of slots currently
#' used, the number of times a previous result was recalled, a new result was
#' recorded, and a result was dropped.
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

#' \code{digest_key} computes a key by hashing the contents of the object using
#' the digest package. No attempt is made to avoid MD5 hash collisions.
#' @rdname strategies
#' @import digest
#' @export
digest_key <- function(fn, cache, digest=digest::digest) {
  delayedAssign("fn_digest", digest(fn))
  function(...) {
    key <- paste0(c(fn_digest, digest(list(...))), collapse=";")
    cache(key, fn(...))
  }
}

#' The \code{pointer_key} strategy uses object identity,
#' that is, pointer equivalence.  This can be faster because hte
#' entire object need not be hashed. However, this strategy is only
#' useful when the function is called repeatedly on the same
#' object rather than merely identical objects. Also be aware that
#' the cache will hold on to the values of the arguments, to prevent
#' them being garbage collected.
#' @rdname strategies
#' @export
pointer_key <- function(fn, cache) {
  delayedAssign("fn_digest", string_reps(list(fn)))
  function(...) {
    l <- list(...)
    key <- paste0(c(fn_digest, string_reps(list(...))), collapse=";")
    # hold onto the argument list while the cache remembers the pointer value
    cache(key, list(fn(...), l))[[1]]
  }
}

#' The \code{hybrid_key} strategy first tries to key on object
#' identity and then falls back on computing the md5 digest.
#' This may use two cache slots per result.
#' @param digest A digest function to use.
#' @rdname strategies
hybrid_key <- function(fn, cache, digest=digest::digest) {
  delayedAssign("fn_digest", digest(fn))
  function(...) {
    l = list(...)
    predigest <- paste0(c(fn_digest, string_reps(l)), collapse=";")
    # also hold on to the argument list
    digest <- cache(predigest,
                    list(paste0(fn_digest, digest(l), collapse=";"),
                         l)
                    )[[1]]
    cache(digest, fn(...))
  }
}
