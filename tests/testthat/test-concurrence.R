test_that("concurrence works", {
  expect_equal(concurrence(toydata, gen, env),
               matrix(c(3, 2, 2, 2, 2, 1, 2, 1, 2), nrow = 3, ncol = 3),
               ignore_attr = TRUE)
})
