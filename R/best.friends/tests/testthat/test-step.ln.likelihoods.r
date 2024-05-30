test_that("step.ln.likelihoods returns expected values for known input", {
  ranks <- c(1, 97, 98, 99, 100)
  tags.no <- 100
  result <- step.ln.likelihoods(ranks, tags.no)
  expect_equal(result$population.on.left, 1)
  expect_equal(result$col.on.left, 1)
  expect_equal(result$col.on.right, c(2, 3, 4, 5))
  expect_equal(result$col.order, c(1, 2, 3, 4, 5))
})