#
# Chicago R Meetup 10/23/2014
# Jay Emerson
#
# This script doesn't contain all points of discussion
# on the material covered.  And there is no Powerpoint.
# And I'm going to fly through it.  Questions?  Feel free
# to drop me an email:
#
# john.emerson@yale.edu
#
# It's great to be back in Chicago -- thanks
# for having me visit on such short notice!
#
########################################################################### 80 #
####################################################### 60 #

mygc <- function(reset=FALSE) {
  paste(gc(reset=reset)[2,6], "MB")
}

### PART I
###
### Some performance and memory management
### quirks and toy examples
###

N <- 1000000      # One million

##################################################
## Example A: integers may not be integers
##
## ( subtext: R's friendliness can be unfriendly )

mygc(TRUE)

x <- rep(0L, N)
is.integer(x)
object.size(x)

x <- x + 1      # Very important work done here...

is.integer(x)
is.integer(1)
object.size(x)

mygc()

######################################################
## Example B: Loops can be slow, but don't have to be.
##
## ( subtext: a slow loop may be your fault,
##   so don't automatically blame it on R )

x <- matrix(1:N, nrow=4)

# Let's call this the "baseline"
system.time({ 
  ans <- apply(x, 2, sum)
})

# If you write loops like this,
# you deserve what you get.  In fact,
# it is so annoying, I'm not going
# to run it ( ~ 80 seconds )
if (FALSE) { # DON'T RUN AT JAKS
  system.time({ 
    ans <- NULL
    for (i in 1:ncol(x)) {
      ans <- c(ans, sum(x[,i])) # Bad
    }
  })
}

# This is a whole lot better, and without
# some of the (polite) overhead of apply().
system.time({
  ans <- rep(0, ncol(x))
  for (i in 1:ncol(x)) {
    ans[i] <- sum(x[,i]) # Good
  }
})

# This being Chicago, many folks will recognize
# the ultimate coolness of the following:
library(Rcpp)
sourceCpp(code='
  #include <Rcpp.h>
  using namespace Rcpp;
  // [[Rcpp::export]]
  NumericVector mycolsum(NumericMatrix x) {
    NumericVector ans(x.ncol());
    int i, j;
    for (j=0; j<x.ncol(); j++) {
      ans[j] = 0;
      for (i=0; i<x.nrow(); i++) {
        ans[j] += x(i,j);
      }
    }
    return ans;
  }')

# And the result?
system.time({
  ans <- mycolsum(x)
})

# And yes, of course, you could do this,
# but that misses the point of the examples:
system.time({
  ans <- colSums(x) # Comes with R, but...
})

###############################################
## Example C: Some surprising things can happen
## in monitoring memory consumption

N <- 1000000         # Still one million here

# Run the following in R Studio line-by-line or
# as a chunk; run in plain old R; we don't expect
# a copy, but sometimes we get one!  R Studio has
# some references to objects in the global environment
# which can trigger copies that are unexpected.  This
# is necessary to provide some cool features, it seems;
# but I wanted to make you aware of the issue.
# MEETUP NOTE: investigate local() relating to this!
rm(x)
mygc(TRUE)
x <- rnorm(N)
mygc()
x[1] <- 1.234
mygc()

# Run any way you like, this is what is expected,
# no copy should be incurred:
myfunc <- function() {
  print(mygc(TRUE))
  x <- rnorm(N)
  print(mygc())
  x[1] <- 1.234
  print(mygc())
}
myfunc()

# Keep your eyes open:

rm(list=ls()) # Clear the entire environment.
              # Note also that the R Studio
              # "Session > Restart R" option is
              # available...

N <- 1000000      # Yes, still one million
mygc <- function(reset=FALSE) {
  paste(gc(reset=reset)[2,6], "MB")
}

mygc(TRUE)

x <- rnorm(N)
mygc()            # Because x is about 8 MB

y <- x
mygc()            # What?  No memory overhead?

rm(x)
mygc(TRUE)  # Interesting

y2 <- y
mygc()            # Same point as above

y[1] <- 0
mygc()            # Eureka!This is worth knowing...

# Some related points not covered here: working
# with a matrix rather than a data.frame is
# recommended when possible.  The differences
# can be substantial.

########################################################################### 80 #
####################################################### 60 #

### PART II
###
### Parallel programming via foreach and do*,
### examples using doMC (MacOS/Linux, not
### Windows, multicore only) and doSNOW (all
### platforms, could be distributed over a cluster,
### but with copies of objects possibly impacting
### performance)
###

N <- 1000000       # Yup... still one million

# Again, our baseline:
x <- matrix(1:N, nrow=4)
system.time({
  ans <- apply(x, 2, sum)
})

# My parallel programming examples will use
# foreach with doMC (thanks to Steve Weston
# -- foreach and doMC -- and Simon Urbanek
# -- multicore.
library(doMC)
registerDoMC(2) # 2 processor cores

# Our first attempt at parallel programming
# with foreach and doMC.  If I ran this, it
# would take about 2 minutes.  So let's
# not waste the time...
if (FALSE) {  # DON'T RUN AT JAKS
  system.time({
    ans <- foreach(i=1:ncol(x),
                   .combine=c) %dopar%
           {
             return(sum(x[,i]))
           }
  })
}

# An improvement, but clunky:
system.time({
  indices <- list(1:125000, 125001:250000)
  ans <- foreach(i=indices,
                 .combine=c) %dopar%
         {
           return(apply(x[,i], 2, sum))
         }
})

# A more elegant solution:
library(itertools)
system.time({
  iter <- isplitIndices(ncol(x), chunks=2)
  ans <- foreach(i=iter,
                 .combine=c) %dopar%
         {
           return(apply(x[,i], 2, sum))
         }
})

# With doSNOW instead of doMC: note that
# the "real work" is unchanged!  Only the
# up-front registration is modified...
library(doSNOW)                           # changed
machines <- rep("localhost", each=2)      # changed
cl <- makeCluster(machines, type="SOCK")  # changed
registerDoSNOW(cl)                        # changed
# A more elegant solution:                 # NOT CHANGED
library(itertools)                         # NOT CHANGED
system.time({                              # NOT CHANGED
  iter <- isplitIndices(ncol(x), chunks=2) # NOT CHANGED
  ans <- foreach(i=iter,                   # NOT CHANGED
                 .combine=c) %dopar%       # NOT CHANGED
         {                                 # NOT CHANGED
           return(apply(x[,i], 2, sum))    # NOT CHANGED
         }                                 # NOT CHANGED
})                                         # NOT CHANGED
stopCluster(cl)                   # strongly recommended


########################################################################### 80 #
####################################################### 60 #

### Part III
###
### Extending bigmemory for HPC with data.frame-like objects:
###   - shared-memory for parallel computing
###   - memory-mapped files for larger-than-RAM objects
###     and persistence
###
### This laptop: dual-core, 8 GB RAM, 512 GB flash storage;
### all this is relevant in the context of this talk, but
### all points scale and apply equally in any environment.
###

if (!file.exists("/Users/jwe/Desktop/JaksTap/"))
  dir.create("/Users/jwe/Desktop/JaksTap/")
setwd("/Users/jwe/Desktop/JaksTap/")

#################################################
## bigmemory: this has been around for a while...
## JSS paper: http://www.jstatsoft.org/v55/i14
##            by Kane, Emerson, Weston
##
## Special note: bigmemory will once again be
## provided on all platforms; a new Windows-friendly
## version will soon be on CRAN.

library(bigmemory)

###########################################
## In-RAM, shared memory (examples omitted)

######################################################
## Beyond RAM, persistent objects (also shared memory)
##
## 500 million rows, 4 columns
## Default type: 8-byte double, so this gives us
## 2 billion elements, about 16 GB total, with
## columns about 4 GB each.  Run this and talk while
## it inializes (a 1-time cost, ~ 2-3 minutes):
system.time({
  x <- big.matrix(nrow=5e8,
                  ncol=4,
                  init=0, # Optional but a good idea
                  backingfile="big.bin",
                  descriptorfile="big.desc")
})
x <- attach.big.matrix("big.desc")

dim(x)
head(x)
tail(x)

# Set the entire first column
system.time({
  x[,1] <- 2.71828
})
# Oops... I changed my mind!
system.time({
  x[,1] <- 3.14159
})
# ? what just happened?! ?
# Let's try that again, with the second column:
system.time({
  x[,2] <- 1.1
})
# Oops... I changed my mind!
system.time({
  x[,2] <- 2.2
})
# This can be an important point and could influence
# your construction of a Big Data algorithm.

# How about this?   ~ 1 minute
mygc(TRUE) # This itself may be a surprise
system.time({
  x[,3] <- runif(nrow(x))
})
mygc()     # Sanity check: ~4 GB of "real R" memory
           # consumption, temporarily.
mygc(TRUE)

##
## JAY: Quit R now, restart.  Persistence!
##

setwd("/Users/jwe/Desktop/JaksTap/")
library(bigmemory)
x <- attach.big.matrix("big.desc")
dim(x)
head(x)
tail(x)

# Using foreach, put random normals into
# the last column and do some counting;
# ~ 1 minute-plus (2 cores in parallel, but
# I expect normals are slower than uniforms):
library(doMC)
registerDoMC(2)
library(itertools)
system.time({
  iter <- isplitIndices(nrow(x), chunks=100)
  ans <- foreach(i=iter,
                 .combine=sum) %dopar%
         {
           x[i,4] <- rnorm(length(i))
           return(length(i))
         }
})

ans
head(x)
tail(x)

######
###### JAY REMINDER #* obscure the right side now
######

if (FALSE) {
  # Be very very careful.  Ask yourself, what would happen?
  
  hist(x[,4])       #* Case 1: the world grinds to a halt
                    #*         unless you have more than
                    #*         3 * object.size(x[,4]) RAM
  cor(x)            #* Case 2: error
  as.data.frame(x)  #* Case 3: error
  y <- x[]          #* Case 4: the world grinds to a halt
                    #*         unless you can handle the
                    #*         it in-RAM as a matrix
}

##
## big.char: vector-like, holding strings up to some
## fixed length.  This is an extension of bigmemory
## with some S4 class inheritance.
##
## https://github.com/jayemerson/big.char
##

setwd("/Users/jwe/Desktop/JaksTap/")
library(big.char)

# Basic instantiation; using the init
# option is not well-implemented
# at the moment and is slow.
system.time({
  x <- big.char(length=2e9,  # 2 billion elements
                maxchar=5,   # strings up to length 5
                backingfile="bigchar.bin",
                descriptorfile="bigchar.desc") # 10 GB total
})
x <- attach.big.char("bigchar.desc")

length(x)
x[1] <- "A"
x[2] <- "Hello"
x[3] <- "world"
x[4] <- "!"
x[length(x)] <- "\tJaksTap"
x[c(1:5,length(x))]
nchar(x[length(x)])

# Definitely don't run this!  But think about it,
# and remember I have 8 GB RAM, which *is* relevant:
if (FALSE) {
  system.time({
    x[1:length(x)] <- "abcde"
  })
}
# 1:length(x) is an R vector of 4-byte integers,
# so in this case it would use 8 GB of RAM.  It isn't
# a big.char/bigmemory issue... it's dealing with
# life in R when working with big objects.  It isn't
# always "business as usual"... unless you can afford
# the RAM.

# Pop Quiz: suppose length(x) had been 3 billion... how
# much RAM would be needed for 1:length(x)?

# I could run this next example, but it would waste
# about 3 hours with 2 processor cores.  The apply() is
# faster than replicate() would be, but is still
# about 20 seconds for each chunk of 2 million
# strings of length 5.  Memory consumption per worker
# is ~ 300 MB.  Here's a challenge: do it in C++ with
# Rcpp with the low-level bigmemory API...
if (FALSE) { # DON'T RUN AT JAKS!
  library(doMC)
  registerDoMC(2)
  system.time({
    iter <- isplitIndices(length(x), chunks=1000)
    ans <- foreach(i=iter, .combine=sum) %dopar% {
      x[i] <- apply(matrix(sample(letters, 5*length(i),
                                  replace=TRUE), nrow=5),
                    2, paste, collapse="")
      return(length(i))
    }
  })
}

###############
# NOTE TO SELF:
# Jay, need toy bigmemory/Rcpp example, see
# http://gallery.rcpp.org/articles/using-bigmemory-with-rcpp/
# and realize we're really hacking the underlying big.matrix
# of type char.  Need random integers with appropriate ASCII
# offset.  Remember the new CRAN policies on random number
# generation, etc...

############################################################
##
## big.data.frame: very much in development, but
## surely you see where I'm going.
##
## For example, I think there is a bug in handling the column
## names (header=TRUE) if they exist.  And plenty of other
## things aren't yet done, either.  But it will work...
## at some basic level.
##
## https://github.com/jayemerson/big.data.frame
##

setwd("/Users/jwe/Desktop/JaksTap/")
library(big.data.frame)

x <- big.read.table("f0_1000.csv", nrows=100)
y <- read.csv("f0_1000.csv", as.is=TRUE)
dim(x)
dim(y)
cbind(x[c(1:5,nrow(x)),98:100], y[c(1:5,nrow(x)),98:100])
table(sapply(y, class)) # Standard R syntax
table(x@desc$classes)

system.time({
  x <- big.read.table("f1_100000.csv", nrow=10000)
})
system.time({
  y <- read.csv("f1_100000.csv", as.is=TRUE)
})

# Test the backingfile usage (trivial case):
x <- big.read.table("f0_1000.csv", nrow=100,
                    location="test_f0")
xx <- attach.big.data.frame("test_f0")
rbind(x[,1], xx[,1], y[1,])

#########################################################
# Can I really do it?  This is ~ 6 GB as a big.data.frame

if (FALSE) {  # DON'T RUN AT JAKS
  x4 <- big.read.table("f4_6000000.csv", nrow=100000,
                       location="test_f4")
}
x4 <- attach.big.data.frame("test_f4")
