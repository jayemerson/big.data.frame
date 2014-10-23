########################################################################### 80 #

#' Provides very large data.frame-like objects
#'
#' This package extends bigmemory's big.matrix objects along with
#' new big.char objects to support larger-than-RAM data.frame-like
#' objects in R.
#' @docType package
#' @name big.data.frame-package
#' @author Jay Emerson
#' @import methods
#' @importClassesFrom bigmemory big.matrix
#' @importClassesFrom big.char big.char
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
#' @importFrom iterators iread.table
#' @importFrom iterators ireadLines
#' @importFrom iterators nextElem
NULL
