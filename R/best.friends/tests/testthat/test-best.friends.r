test_that("no errors in simplest case",{
    mat <- diag(nrow=5, ncol=5)
    rownames(mat) <- paste0("tag",1:5)
    colnames(mat) <- paste0("coll",1:5)
    expect_no_error(best.friends(mat))
})

test_that("friends are determined correctly",{
    mat <- matrix(c(10,6,7,8,9,
                    9,10,6,7,8,
                    8,9,10,6,7,
                    7,8,9,10,6,
                    6,7,8,9,10), nrow=5, ncol=5)
    rownames(mat) <- paste0("tag",1:5)
    colnames(mat) <- paste0("coll",1:5)
    res <- best.friends(mat, threshold = 1)
    expect_equal(res,
                data.frame(tag=paste0("tag",1:5),
                            collection=paste0("coll",1:5)))

})
