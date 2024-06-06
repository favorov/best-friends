test_that("step.ln.likelihoods returns expected values for known input", {
  ranks <- c(1, 97, 98, 99, 100)
  tags.no <- 100
  result <- best.step.fit(ranks, tags.no)
  expect_equal(result$best.step.rank, 96)
  expect_equal(result$population.on.left, 1)
  expect_equal(result$col.on.left, c(1))
  expect_equal(result$col.on.right, c(2, 3, 4, 5))
})

