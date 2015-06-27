#memo
======

In-memory caching of repeated computations, by pointer equivalence.

This package implements a cache that can be used to avoid repeated
computations of functions. The cache lookup is based on object identity (i.e.
pointer equivalence) which is suited for functions like accessors or other
functions that are called repeatedly on the same object.

Because it uses pointer equivalence, this package has a fairly limited
application. If you are looking to memoize the results of computations
lasting more than several milliseconds, computations with non-scalar
inputs, or to build a disk cache for results too big for memory, you
may be better off with one of the following packages:

* [memoise](https://github.com/hadley/memoise)
* [diskmemoiser](http://cran.r-project.org/web/packages/diskmemoiser/index.html)
* [R.cache](http://cran.r-project.org/web/packages/R.cache/index.html)
