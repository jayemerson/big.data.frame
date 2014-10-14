context("Creation and characteristics")

test_that("Creating and characteristics 1", {
  x <- big.data.frame(10, c("double", "character"),
                      names=c("first", "second"),
                      init=list(-1.23, "A"),
                      maxchar=c(NA, 10))
  y <- data.frame(first=rep(-1.23, 10), second=rep("A", 10))
  if (FALSE) {
    expect_that(x[], equals(y))
    expect_that(x[,1:2], equals(y[,1:2]))
    expect_that(x[,1], equals(y[,1]))
    expect_that(x[,1,drop=FALSE], equals(y[,1,drop=FALSE]))
    expect_that(x[,2], equals(y[,2]))
    expect_that(x[,2,drop=FALSE], equals(y[,2,drop=FALSE]))
    expect_that(nrow(x), equals(nrow(y)))
    expect_that(ncol(x), equals(ncol(y)))
    expect_that(length(x), equals(length(y)))
    expect_that(dim(x), equals(dim(y)))
  }
  y <- data.frame(first=rep(-1.23, 10), second=rep("A", 10),
                  stringsAsFactors=FALSE)
  expect_that(nrow(x), equals(nrow(y)))
  expect_that(ncol(x), equals(ncol(y)))
  expect_that(length(x), equals(length(y)))
  expect_that(dim(x), equals(dim(y)))
  expect_that(x[], equals(y))
  expect_that(x[,1:2], equals(y[,1:2]))
  expect_that(x[,1], equals(y[,1]))
  expect_that(x[,1,drop=FALSE], equals(y[,1,drop=FALSE]))
  expect_that(x[,2], equals(y[,2]))
  expect_that(x[,2,drop=FALSE], equals(y[,2,drop=FALSE]))
})

test_that("Creating and characteristics 2", {
  z <- big.data.frame(10, c("double", "character"),
                      names=c("first", "second"),
                      init=list(-1.23, "A"),
                      maxchar=c(NA, 10),
                      location="testdir")
  y <- data.frame(first=rep(-1.23, 10), second=rep("A", 10),
                  stringsAsFactors=FALSE)
  x <- attach.big.data.frame("testdir")
  expect_that(x[], equals(y))
  expect_that(x[,1:2], equals(y[,1:2]))
  expect_that(x[,1], equals(y[,1]))
  expect_that(x[,1,drop=FALSE], equals(y[,1,drop=FALSE]))
  expect_that(x[,2], equals(y[,2]))
  expect_that(x[,2,drop=FALSE], equals(y[,2,drop=FALSE]))
  expect_that(nrow(x), equals(nrow(y)))
  expect_that(ncol(x), equals(ncol(y)))
  expect_that(length(x), equals(length(y)))
  expect_that(dim(x), equals(dim(y)))
  expect_that(dim(x), equals(dim(y)))
})