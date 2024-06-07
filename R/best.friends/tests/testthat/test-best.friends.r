test_that("no errors in simplest case",{
    mat <- diag(nrow=5, ncol=5)
    rownames(mat) <- paste0("tag",1:5)
    colnames(mat) <- paste0("coll",1:5)
    expect_no_error(best.friends(mat))
})