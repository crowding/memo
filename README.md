#memo
======  

[![](https://www.r-pkg.org/badges/version/async?color=purple)](https://cran.r-project.org/package=memo)
[![pkgdown](https://github.com/crowding/memo/workflows/pkgdown/badge.svg)](https://github.com/crowding/memo/actions)
[![R-CMD-check](https://github.com/crowding/memo/workflows/R-CMD-check/badge.svg)](https://github.com/crowding/memo/actions)
[![test-coverage](https://github.com/crowding/memo/workflows/test-coverage/badge.svg)](https://github.com/crowding/memo/actions)

The `memo` package implements a simple in-memory cache for the results of a function. If you have an expensive function that is being called repeatedly with the same inputs, `memo` can help.

## Fibonnacci example


```r
fib <- function(n) if (n <= 1) 1 else fib(n-1) + fib(n-2)
sapply(0:9, fib)
```

```
##  [1]  1  1  2  3  5  8 13 21 34 55
```

This recursive implementation corresponds closely to the way the sequence is defined in math texts, but it has a performance problem. The problem is that as you ask for values further down the sequence, the computation becomes inordinately slow due to recursion. To demonstrate the issue, we can try counting every time `fib` is
called:


```r
count <- 0
fib <- function(n) {
  count <<- count+1
  if (n <= 1) 1 else fib(n-1) + fib(n-2)
}

counted_fib <- function(n) {
  count <<- 0
  c(n=n, result=fib(n), calls=count)
}

t(sapply(0:16, counted_fib))
```

```
##        n result calls
##  [1,]  0      1     1
##  [2,]  1      1     1
##  [3,]  2      2     3
##  [4,]  3      3     5
##  [5,]  4      5     9
##  [6,]  5      8    15
##  [7,]  6     13    25
##  [8,]  7     21    41
##  [9,]  8     34    67
## [10,]  9     55   109
## [11,] 10     89   177
## [12,] 11    144   287
## [13,] 12    233   465
## [14,] 13    377   753
## [15,] 14    610  1219
## [16,] 15    987  1973
## [17,] 16   1597  3193
```

The number of calls increases unreasonably. This is because, for instance, `fib(6)` calls both `fib(5)` and `fib(4)`, but `fib(5)` also calls `fib(4)`. The second call to `fib(4)` is wasted work. And this pattern goes on -- the two calls to `fib(4)` lead to _four_ calls to `fib(2)`.  Every time you increment `n` by one, the number of calls roughly doubles.  (Clearly, there are more efficient algorithms for computing the Fibbonacci sequence, but this is a toy example, where `fib` stands in for some expensive function that is being called repeatedly.)

One way to cut down on wasted effort would be to check whether `fib(n)` has already been computed for a given `n`. If it has, `fib` can just return that value instead of starting over. This is called "memoizing." The `memo` package can [automatically]() create a memoized version of a given function, just by wrapping the function definition in `memo()`:

[automatically]:  https://en.wikipedia.org/wiki/Memoization#Automatic_memoization


```r
library(memo)

count <- 0
fib <- memo(function(n) {
  count <<- count+1
  if (n <= 1) 1 else fib(n-1) + fib(n-2)
})

counted_fib(16)
```

```
##      n result  calls 
##     16   1597     17
```
Now, computing `fib(16)` only takes 17 calls. And if we call again, it remembers the previous answer and doesn't make any new calls:

```r
counted_fib(16)
```

```
##      n result  calls 
##     16   1597      0
```
Each successive value then only takes two calls:

```r
t(sapply(17:30, counted_fib))
```

```
##        n  result calls
##  [1,] 17    2584     1
##  [2,] 18    4181     2
##  [3,] 19    6765     2
##  [4,] 20   10946     2
##  [5,] 21   17711     2
##  [6,] 22   28657     2
##  [7,] 23   46368     2
##  [8,] 24   75025     2
##  [9,] 25  121393     2
## [10,] 26  196418     2
## [11,] 27  317811     2
## [12,] 28  514229     2
## [13,] 29  832040     2
## [14,] 30 1346269     2
```

The tradeoff for this speedup is the memory used to store previous results. By default `memo` will remember the 5000 most recently used results; to adjust that limit you can change the `cache` option:


```r
fib <- memo(cache=lru_cache(5000), function () {...})
```

The Fibonacci sequence being kind of a toy example, memoization has a variety of uses, such as:  
* Caching the results of expensive database queries, for instance in Shiny apps where many users may make identical queries.
* Algorithms for path finding (dynamic programming) and parsing.
* Simulations such as [Cellular automata](https://en.wikipedia.org/wiki/Hashlife).
