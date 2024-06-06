test_that("step.ln.likelihoods returns expected values for known input", {
  ranks <- c(1, 97, 98, 99, 100)
  tags.no <- 100
  result <- step.fit.ln.likelihoods(ranks, tags.no)
  expect_equal(length(result$collectons.order), length(ranks))
  expect_equal(length(result$ln.likelihoods), tags.no)
  expect_equal(length(result$k1.by.l1), tags.no)
  #test for min of result$k1.by.l1
  #test for max of result$k1.by.l1
  
  
})

test_that("returns error when a rank is higher than tag.no", {
  ranks <- c(1, 2, 3, 4, 6)
  tags.no <- 5
  expect_error(step.fit.ln.likelihoods(ranks, tags.no))
})