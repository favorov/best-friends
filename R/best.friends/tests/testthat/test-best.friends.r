test_that("no errors in simplest case",{
    mat <- diag(nrow=5, ncol=5)
    rownames(mat) <- paste0("tag",1:5)
    colnames(mat) <- paste0("coll",1:5)
    expect_no_error(best.friends(mat))
})

test_that("best friend is determined correctly",{
    mat <- matrix(c(10,6,7,8,9,
                    9,10,6,7,8,
                    8,9,10,6,7,
                    7,8,9,10,6,
                    6,7,8,9,10), nrow=5, ncol=5)
    rownames(mat) <- paste0("tag",1:5)
    colnames(mat) <- paste0("coll",1:5)
    res <- best.friends(mat, threshold = 1)
    expect_equivalent(res,
                data.frame(tag=paste0("tag",1:5),
                           collection=paste0("coll",1:5)))

})

test_that("two friends are determined correctly",{
    mat <- matrix(c(10,6,7,8,9,
                    9,10,6,7,8,
                    8,9,10,6,7,
                    7,8,9,100,6,
                    6,7,8,100,10), nrow=5, ncol=5)
    rownames(mat) <- paste0("tag",1:5)
    colnames(mat) <- paste0("coll",1:5)
    expect_no_warning(res <- best.friends(mat, threshold = 1, best.no = 2))
    expect_equivalent(res[res["tag"]=="tag4",],
                data.frame(tag=paste0("tag",c(4,4)),
                           collection=paste0("coll",4:5)))

})
