context("cache")

`%is%` <- expect_equal

signals <- c()
signal <- function(x=".") {
  signals <<- c(signals, x)
  x
}

try_finally <- function(setup = NULL, code = NULL, teardown = NULL) {
  force(setup)
  on.exit(teardown)
  result <- code
}

with_clean_signals <- function(code = NULL) {
  try_finally(
    setup = {
      old_signals <- signals
      signals <<- c()},
    teardown = {
      signals <<- old_signals
    },
    code = code)
}

expect_signal <- function (code, pattern=".+") {
  with_clean_signals({
    force(code)
    expect_that(paste(signals, collapse=""), matches(pattern),
                "No signal seen when one was expected")
  })
}

expect_no_signal <- function(code, pattern=".+") {
  with_clean_signals({
    force(code)
    expect_that(paste(signals, collapse=""), not(matches(pattern)),
                "Signal called when none was expected")
  })
}

test_that("memo() memoizes a function.", {
  f <- memo(function(x) {signal(); x*2})
  a <- 1:5
  b <- a
  expect_signal(f(a) %is% seq(2, 10, 2))
  expect_no_signal(f(b) %is% seq(2, 10, 2))
})

test_that("Memoization is based on memory address, not value", {
  f <- memo(function(x) {signal(); x*2}, key=pointer_key)
  a <- 1:5
  f(a)
  #A sufficiently clever implementation of R may make the following fail.
  c <- a + 1 - 1 # i.e. identical object but a new copy.
  expect_signal(f(c) %is% seq(2, 10, 2))
})

test_that("Pointer Memoization does compare scalars by value.", {
  a <- 123481233783240
  b <- 123481233700000 + 83240
  f <- memo(function(x) {signal(); x*2}, key=pointer_key)
  expect_signal(f(a) %is% 246962467566480)
  expect_no_signal(f(b) %is% 246962467566480)
})

test_that("Digest-based memoisation memoises on content", {
  f <- memo(function(x) {signal(); x*2}, key=digest_key)
  a <- 1:5 + 0 #R now has range objects????
  expect_signal(f(a))
  c <- a + 1 - 1 # i.e. identical object but a new copy.
  expect_no_signal(f(c) %is% seq(2, 10, 2))
})
