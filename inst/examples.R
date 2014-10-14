


if (FALSE) {
  
  # One possible behavior, because we don't (currently) maintain
  # formal factors in the big.data.frame.
  y <- x[stringsAsFactors=FALSE]
  str(y)
  y <- x[]
  str(y)
  
  x <- big.data.frame(10, c("double", "character"))
  x@data[[1]][]
  x@data[[2]][]   # Should be a vector of NAs, fix up big.char
  
  x <- big.data.frame(10, c("double", "character"), init=list(-1, "A"),
                      maxchar=c(NA, 10))
  x@data[[1]][]
  x@data[[2]][]
  
  x <- big.data.frame(10, c("double", "character"), location="dftest")
  x@data[[1]][]
  x@data[[2]][]
  
  y <- attach.big.data.frame("dftest")
}

