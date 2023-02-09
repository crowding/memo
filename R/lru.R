#' `basic_cache` makes a cache that does not expire old entries.
#' It should be used in situations where you know the number of
#' things to remember is bounded.
#' @rdname lru_cache
#' @export
permanent_cache <- function() {
  cache <- new.env(parent=emptyenv())

  hits <- 0L
  misses <- 0L
  expired <- 0L
  used <- 0L
  size <- Inf

  function(key, value, action="cache", ifnotfound=NULL) {
    switch(action,
           exists=exists(key, cache),
           cache=if(exists(key, cache)) {
             hits <<- hits+1L
             cache[[key]]
           } else {
             misses <<- misses+1
             used <<- used+1L
             cache[[key]] <<- value
           },
           get=if(exists(key, cache)) {
             hits <<- hits+1L
           } else {
             misses <- misses+1L
             ifnotfound
           },
           set={
             if(!exists(key, cache)) {used <- used+1L}
             cache[[key]] <<- value
           },
           rm=if(exists(key, cache)) {
             used <<- used-1L
             expired <<- expired+1L
             rm(list=key, envir=cache)
           })
  }
}

#' Construct a cache with least-recently-used policy.
#' @param size The maximum number of results to keep.
#' @return A function f(key, value) which takes a string in the first
#' parameter and a lazily evaluated value in the second. `f`
#' will use the string key to retrieve a value from the cache, or
#' return the matching item from the cache, or force the second
#' argument and return that, remembering the result on future calls.
#'
#' When the number of entries in the cache exceeds \code{size}, the least
#' recently accessed entries are removed.
#' @export
lru_cache <- function(size = 10000) {
  lru <- new.env(hash=TRUE, parent=emptyenv(), size=size)
  pred <- new.env(hash=TRUE, parent=emptyenv(), size=size)
  succ <- new.env(hash=TRUE, parent=emptyenv(), size=size)

  hits <- 0
  misses <- 0
  expired <- 0
  used <- 0

  pred$TAIL <- "HEAD"
  succ$HEAD <- "TAIL"

  function(key, value) {
    #value lazily forced if not found
    if (exists(key, lru)) {
      hits <<- hits+1
      #move accessed value to front
      new_succ <- succ[[key]]
      new_pred <- pred[[key]]
      succ[[new_pred]] <<- new_succ
      pred[[new_succ]] <<- new_pred
      pred[[succ$HEAD]] <<- key
      pred[[key]] <<- "HEAD"
      succ[[key]] <<- succ$HEAD
      succ$HEAD <<- key
      lru[[key]]
    } else {
      misses <<- misses+1
      lru[[key]] <<- value
      #drop if entries exceeded
      while (used >= size) {
        last <- pred$TAIL
        succ[[pred[[last]]]] <<- "TAIL"
        pred$TAIL <<- pred[[last]]
        rm(list=last, envir=lru)
        rm(list=last, envir=pred)
        rm(list=last, envir=succ)
        expired <<- expired + 1
        used <<- used - 1
      }
      succ[[key]] <<- succ$HEAD
      pred[[succ$HEAD]] <<- key
      succ$HEAD <<- key
      pred[[key]] <<- "HEAD"
      used <<- used + 1
      value
    }
  }
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
