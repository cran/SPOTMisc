context("FunExternal")

test_that("test external functions (smoof/BBOB) are evaluated without errors", {
    fn = makeBBOBFunction(dimension = 2L, fid = 1L, iid = 1L)
    y1 <- fn(c(0,0))
    x <- matrix(c(0,0), 1, 2)
    y2 <- as.double(funBBOBCall(x, opt = list(dimensions = 2L, fid = 1L, iid = 1L)) )
    expect_equal( y1 , y2)
})