context("hashmap")

`%is%` <- expect_equal

test_that("hash uses arbitrary keys", {

  x <- hashmap()
  x[[ "1" ]] <- "foo"
  x[[ quote(`1`) ]] <- "bar"
  x[[ 1 ]] <- "baz"
  x[[ 1L ]] <- "qux"
  x[[ NULL ]] <- "qux"
  x[[ NA ]] <- "quux"
  x[[ NA_character_ ]] <- "quuux"

  x[[ "1" ]] %is% "foo"
  x[[ quote(`1`) ]] %is% "bar"
  x[[ 1 ]] %is% "baz"
  x[[ 1L ]] %is% "qux"
  x[[ NULL ]] %is% "qux"
  x[[ NA ]] %is% "quux"
  x[[ NA_character_ ]] %is% "quuux"

  x[list("1", NULL)] %is% list("foo", "qux")

  length(keys(x)) %is% 7
  expect_setequal(keys(x),
                  list(list("1"), list(quote(`1`)), list(1),
                       list(1L), list(NULL), list(NA), list(NA_character_)))
  expect_setequal(values(x),
                  list("foo", "bar", "baz", "qux", "qux", "quux", "quuux"))

})

test_that("hash [] and []<- and pairs()", {
  x <- hashmap()

  x[[ 1, "three" ]] <- "first"
  x[[ 4, "seven" ]] <- "second"
  x[c(1, 4), list("three", "seven")] %is% list("first", "second")

  x[list(1, 5), c("three", "seven")] <- c("refirst", "third")
  x[c(1, 4), list("three", "seven")] %is% list("refirst", "second")

  expect_true(hasKey(x, 1, "three"))
  expect_false(hasKey(x, 4, "three"))

  x[list(1, 5), c("three", "seven")] <- c("refirst", "third")

  expect_setequal(to_pairs(x),
                  list(list(key = list(4, "seven"), value = "second"),
                       list(key = list(1, "three"), value = "refirst"),
                       list(key = list(5, "seven"), value = "third")))

  y <- from_pairs(to_pairs(x))
  expect_setequal(to_pairs(x), to_pairs(y))

  dropKey(x, 1, "three")
  expect_equal(length(keys(x)), 2)

})
