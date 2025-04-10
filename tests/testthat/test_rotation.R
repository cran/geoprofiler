a <- c(2, 0, 0)
b <- c(0, 2, 0)
vec1 <- vec2mat(a)
vec2 <- vec2mat(b)

test_that("type of object returned is as expected", {
  expect_length(vec2mat(a), 3)
  expect_true(is.matrix(vec2mat(a)))
})

# vlength(vec1)
# vnorm(vec1)
# vcross(vec1, vec2)
# vrotate(vec1, vec2, pi/2)

test_that("Output of functions is as expected", {
  expect_equal(vlength(vec1), 2L)
  expect_equal(vnorm(vec1), t(c(1, 0, 0)))
  expect_equal(vcross(vec1, vec2), t(c(0, 0, 4)))
  expect_equal(vrotate(vec1, vec2, pi / 2), t(c(0, 0, -2)))
})
