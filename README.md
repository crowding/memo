#memo
======

In-memory caching of repeated computations, by pointer equivalence.

This package wraps an R function you provide with an in-memory cache
that caches the function results for repeated access. The cache lookup
is based on object identity (i.e. pointer equivalence) which is suited
to accessors or computations that are called repeatedly on the same
object. The cache follows LRU semantics.
