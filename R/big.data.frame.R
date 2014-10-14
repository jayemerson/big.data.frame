#
# Development for package big.data.frame
#
# Jay Emerson
# October 2014
#
# - Could be in-RAM or filebacked
#
# - Fundamentally: a list of big* vectors of the same length
#
# - We can nicely support column addition/deletion, but
#   the number of rows must remain fixed.
#
# - Column names are simply the names of the list; row
#   names can be supported by big.strings, eventually?
#   Or perhaps this would be a future change for
#   bigmemory itself?  I have mixed feelings, frankly.
#
# - Not really supporting factors, though we appear to in
#   the extractions.  But I'm not thrilled with this.
#
###############################################################################
########################################################################## 80 #
###################################################### 60 #

#' S4 class big.data.frame is essentially a list of big vectors
#' provided by packages bigmemory and big.char.
#' @exportClass big.data.frame
setClass('big.data.frame', representation(desc='list',
                                          data='list'))

#' @title Create a big.data.frame
#'
#' @description
#' Create a \code{big.data.frame}
#'
#' @details
#' This is the full set of details for documentation.
#' 
#' @param nrow the number of rows
#' @param classes a vector of values from 'double', 'character', ... 
#' @param location folder to contain the object backingfiles, or NULL
#' if the object will be in-RAM.
#' @param names a vector of names for the columns (variables)
#' @param maxchar a vector with NA for numeric columns, but integers
#' specifying the maximum number of characters for string columns (see
#' \code{\link[big.char]{big.char}} for more information).
#' @param init a vector of values for initialization; note that for
#' large objects this will slow down the creation substantially.
#' @return Returns a \code{big.data.frame} object
#' @author Jay Emerson
#' @export
big.data.frame <- function(nrow, classes,
                           location=NULL,
                           names=NULL,
                           maxchar=NULL,
                           init=NULL) {

  if (!is.null(location)) {
    if (file.exists(location)) {
      warning(paste("Location", location, "exists; using it..."))
    } else {
      warning(paste("Creating", location))
      if (!dir.create(location)) stop("Directory creation failed")
    }
  }
  if (is.null(names)) names <- paste("V", 1:length(classes), sep=".")
  if (length(names) != length(unique(names))) {
    stop("names must be unique") 
  }
  if (any(!(classes %in%
              c("double", "integer", "short", "char", "character"))))
    stop("Invalid class")
  if (nrow < 1) stop("No rows?  Really?")
  if (is.null(maxchar)) {
    maxchar <- rep(NA, length(classes))
    if (any(classes=="character")) maxchar[classes=="character"] <- 8
  }
  # If maxchar is provided, we don't currently check sanity with classes
  
  x <- new('big.data.frame',
           desc=list(dim=c(nrow, length(classes)), 
                     classes=classes, maxchar=maxchar, names=names),
           data=list())
  
  if (!is.null(location)) {
    print("We will be creating filebackings in this location.")
    print("And we need some overall descriptor perhaps?")
    backingfile <- paste(names, ".bin", sep="")
    descriptorfile <- paste(names, ".desc", sep="")
    dput(list(dim=c(nrow, length(classes)),
              maxchar=maxchar, names=names, classes=classes),
         file.path(location, "info.txt"))
  } else {
    backingfile <- NULL
    descriptorfile <- NULL
  }
  
  i <- 0 # To shut up a warning with foreach...
  x@data <- foreach(i=1:length(classes)) %do% {
    if (classes[i] == "character") {
      ans <- big.char::big.char(nrow, maxchar=maxchar[i],
                                init=init[[i]],
                                backingfile=backingfile[i],
                                descriptorfile=descriptorfile[i],
                                backingpath=location)
    } else {
      ans <- bigmemory::big.matrix(nrow, 1, type=classes[i],
                                   init=init[[i]],
                                   backingfile=backingfile[i],
                                   descriptorfile=descriptorfile[i],
                                   backingpath=location)
    }
    return(ans)
  }
  names(x@data) <- names
  
  return(x)
}

#
# attach functionality
#
#' @title Attach an existing big.data.frame from its backingfile
#' @description The expected usage is for shared-memory parallel computing
#' or for persistence via memory-mapped files of the (column) variables.
#' @return a \code{\link{big.data.frame}} object
#' @param location the folder containing the backingfiles.
#' @export
attach.big.data.frame <- function(location) {
  info <- dget(file.path(location, "info.txt"))
  x <- new('big.data.frame',
           desc=list(dim=info$dim, 
                     classes=info$classes,
                     maxchar=info$maxchar,
                     names=info$names),
           data=vector(mode="list", length=length(info$names)))
  names(x@data) <- info$names
  for (i in 1:length(info$names)) {
    if (info$classes[i] == "character") {
      x@data[[i]] <- big.char::attach.big.char(
        paste(info$names[i], ".desc", sep=""), path=location)
    } else {
      x@data[[i]] <- bigmemory::attach.big.matrix(
        paste(info$names[i], ".desc", sep=""), path=location)
    }
  }
  return(x)
}


#
# Need is, as, names... functionality

#' @title ncol functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @exportMethod ncol
setMethod('ncol', signature(x="big.data.frame"),
  function(x) return(x@desc$dim[2]))

#' @title nrow functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @exportMethod nrow
setMethod('nrow', signature(x="big.data.frame"), 
  function(x) return(x@desc$dim[1]))

#' @title dim functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @exportMethod dim
setMethod('dim', signature(x="big.data.frame"),
  function(x) return(c(nrow(x), ncol(x))))

#' @title length functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @param x a big.data.frame
#' @exportMethod length
setMethod('length', signature(x="big.data.frame"),
  function(x) return(ncol(x)))

#
# Get/set signatures!
#

setMethod("[",
          signature(x = "big.data.frame", i="ANY", j="ANY", drop="missing"),
          function(x, i, j, ..., drop) {
            #stop("Not yet BDF get:(ANY, ANY)")
            cat("BDF get:(ANY,ANY,missing) row subset extraction.\n")
            if (length(j)==1) return(as.data.frame(x@data[[j]][i],...)[[1]])
            return(as.data.frame(lapply(x@data[j], function(a) a[i]), ...))
          })

setMethod("[",
          signature(x = "big.data.frame", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
            #stop("Not yet BDF get:(ANY, missing)")
            cat("BDF get:(ANY,missing,missing) row subset extraction.\n")
            # Here, current default is drop=TRUE
            if (ncol(x)==1) return(as.data.frame(x@data[[1]][i],...)[[1]])
            # Otherwise, have multiple columns to extract
            return(as.data.frame(lapply(x@data, function(a) a[i]), ...))
          })

setMethod("[",
          signature(x = "big.data.frame", i="missing", j="ANY", drop="missing"),
          function(x, i, j, ..., drop) {
            cat("BDF get:(missing,ANY,missing)\n")
            if (length(j)==1) return(as.data.frame(x@data[[j]][],...)[[1]])
            # Otherwise, multiple column extraction:
            return(as.data.frame(lapply(x@data[j], function(a) a[]), ...))
          })

setMethod("[",
          signature(x = "big.data.frame", i="missing", j="ANY", drop="logical"),
          function(x, i, j, ..., drop) {
            cat("BDF get:(missing,ANY,ANY)\n")
            if (length(j)==1) {
              if (!drop) {
                ans <- as.data.frame(x@data[[j]][],...)
                names(ans) <- names(x@data)[j]
                return(ans)
              } # else drop==TRUE next with one column:
              return(as.data.frame(x@data[[j]][],...)[[1]])
            } # and otherwise we have multiple columns to extract:
            return(as.data.frame(lapply(x@data[j], function(a) a[]), ...))
          })

setMethod("[",
          signature(x = "big.data.frame",
                    i="missing", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
            cat("BDF get:(missing,missing,missing)\n")
            return(as.data.frame(lapply(x@data, function(a) a[]), ...))
          })
