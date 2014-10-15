big.data.frame
==============

Extend big.char and bigmemory's big.matrix to support
larger-than-RAM data.frame-like objects in R.
Current package build status via use of
[Travis CI](https://travis-ci.org/jayemerson/big.data.frame):
<a href="https://travis-ci.org/jayemerson/big.data.frame"><img src="https://travis-ci.org/jayemerson/big.data.frame.svg?branch=master"></a>

The Travis CI build is failing because I haven't yet configured it to
install `big.char` from GitHub (it's probably only looking at CRAN).

Getting Started
===============

You'll need big.char, first:

---
    > require(devtools)
    > install_github('big.char', 'jayemerson')
    > install_github('big.data.frame', 'jayemerson')
    > library(big.data.frame)
---

Development notes (10/14/2014)
==============================

For the moment, don't do anything with factors.  That is, the
`big.data.frame` is actually only storing columns of numeric or
character data (via `big.matrix` and `big.char`). 
Rivisit this once `big.factor` is available.

Note that data frames always have row and column names, with
subsetting preserving them (whether you like it or not).  Need
to consider this.  I suppose a subset needs to be in-RAM, anyway.

