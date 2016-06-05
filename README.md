---
title: "The 'memo' package"
author: "Peter Meilstrup"
date: "2016-06-05"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---
#memo
======

The `memo` package implements a simple in-memory cache for the results
of repeated computations.

## Fibonnacci example

Consider this terrible way to compute the Fibonnacci sequence:


```r
fib <- function(n) if (n <= 1) 1 else fib(n-1) + fib(n-2)
```

This function is very slow to compute larger values. Each call to `fib(n)` with
`n > 1` generates two more calls to `fib`, leading to a runtime complexity of
`O(2^n)`. Let's count how many recursive calls to `fib` are involved in computing
each `fib(n)`:


```r
count.calls <- function(f) {
  force(f)
  function(...) {
    count <<- count+1;
    f(...)
  }
}

with_count <- function(f) {
  force(f)
  function(x) {
    count <<- 0
    c(n=x, result=f(x), calls=count)
  }
}

fib <- count.calls(fib)

t(sapply(1:16, with_count(fib)))
```

```
##        n result calls
##  [1,]  1      1     1
##  [2,]  2      2     3
##  [3,]  3      3     5
##  [4,]  4      5     9
##  [5,]  5      8    15
##  [6,]  6     13    25
##  [7,]  7     21    41
##  [8,]  8     34    67
##  [9,]  9     55   109
## [10,] 10     89   177
## [11,] 11    144   287
## [12,] 12    233   465
## [13,] 13    377   753
## [14,] 14    610  1219
## [15,] 15    987  1973
## [16,] 16   1597  3193
```

The number of calls increases unreasonably. This is because, say, `fib(6)` calls
both `fib(5)` and `fib(4)`, but `fib(5)` also calls `fib(4)`, so the second
computation is wasted work, and this gets worse the higher `n` you have. We
would be in better shape if later invocations of `fib` could access the values
that were previously computed.

By wrapping `fib` using `memo`, fewer calls are made:


```r
library(memo)
fib <- memo(fib)
t(sapply(1:16, with_count(fib)))
```

```
##        n result calls
##  [1,]  1      1     1
##  [2,]  2      2     3
##  [3,]  3      3     2
##  [4,]  4      5     2
##  [5,]  5      8     2
##  [6,]  6     13     2
##  [7,]  7     21     2
##  [8,]  8     34     2
##  [9,]  9     55     2
## [10,] 10     89     2
## [11,] 11    144     2
## [12,] 12    233     2
## [13,] 13    377     2
## [14,] 14    610     2
## [15,] 15    987     2
## [16,] 16   1597     2
```

Here is only called to compute new values. Note that `fib(16)` only took two
calls to compute, because `fib(15)` was previously computed. To compute `fib(16)`
_de novo_ takes 17 calls:


```r
fib <- function(n) if (n <= 1) 1 else fib(n-1) + fib(n-2)
fib <- memo(count.calls(fib))
with_count(fib)(16)
```

```
##      n result  calls 
##     16   1597     17
```
 
