context("Impure queue")

`%is%` <- expect_equal

test_that("refqueue operates as a queue", {
  q <- refqueue()
  q$append("Hello")
  q$push("Oh")
  q$append("World")
  q$pop() %is% "Oh"
  q$pop() %is% "Hello"
  q$pop() %is% "World"
})

test_that("refqueue is reference-like", {
  q <- refqueue()
  r <- q
  q$append("one")
  r$append("two")
  r$pop() %is% "one"
  q$pop() %is% "two"
})

test_that("can check if queue has elements", {
  q <- refqueue()
  q$has() %is% FALSE
  q$append("a")
  q$has() %is% TRUE
  q$pop()
  q$has() %is% FALSE
  q$push("b")
  q$push("c")
  q$has() %is% TRUE
  q$pop()
  q$pop()
  q$has() %is% FALSE
  q$append("b")
  q$append("c")
  q$has() %is% TRUE
  q$pop()
  q$pop()
  q$has() %is% FALSE
})

test_that("popping empty queue is error but still functions afterwards", {
  q <- refqueue()
  expect_error(q$pop())
  expect_warning(q$append("hello")) #warning because of restarted promise
  q$pop() %is% "hello"
  expect_error(q$pop())
})
