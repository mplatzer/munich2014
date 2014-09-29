


wapply <- function(x, fun, n, n.right=n, ...) {
  ## Apply function to windows of data.
  ##
  ## Returns vector y where y[i] = fun(c(x[(i-n):(i+n.right)]). If
  ## `n.right` is omitted it defaults to `n`, e.g the window is
  ## symmetric around i. Additional arguments are passed through to
  ## `fun`. Both `n` and `n.right` can be negative as long as the
  ## window is not empty.
  ##
  ## `x` can be either a vector or a data.frame, but the returned
  ## value is always a vector with same length as x. The input to
  ## function `fun` always has length n+n.right+1. NA are appended to
  ## beginning and end of input data as necessary, for example to
  ## calculate the value in the first window the first `n` values are
  ## NA.
  ##
  ## Example:
  ##  > x <- c(-1, 0, 1, 3, 4)
  ##  > wapply(x, mean, 1, 0) # (x[i-1]+x[i])/2
  ##   [1]   NA -0.5  0.5  2.0  3.5
  ##  > wapply(x, sum, 1, 0, na.rm=TRUE) # x[i-1]+x[i]
  ##   [1] -1 -1  1  4  7
  ##  > wapply(1:10, sum, 2, -2) # Lag by 2
  ##   [1] NA NA  1  2  3  4  5  6  7  8
  
  ## Make sure the window is not empty.
  if (-n > n.right) stop("ERROR: Empty window [", -n, ", ", n.right, "].\n")
  ## Number of NAs to append to both ends.
  n.start <- max(n,0)
  n.end <- max(n.right,0)
  ## Append NAs to both ends.
  if ("data.frame" %in% class(x)) { ## data.frame or data.table
    x <- data.table(x) # To allow vector-like syntax.
    len.x <- nrow(x)
    ## Creating the NA data.tables is a bit more complicated than
    ## usual because each column must have the same class as the
    ## columns of `x` (in fact this is only needed for NA.start; rbind
    ## will convert columns to the class of the first argument, and if
    ## there are only NAs these are by default logicals). Factors are
    ## converted to characters.
    x.classes <- lapply(x, function(v) {if (class(v)=="factor") "character" else class(v)})    
    NA.start <- as.data.table(lapply(x.classes, function(cc) as(rep(NA,n.start), cc)))
    NA.end <- as.data.table(lapply(x.classes, function(cc) as(rep(NA,n.end), cc)))    
    x <- rbind(NA.start, x, NA.end)
  } else { # vector
    len.x <- length(x) 
    NA.start <- rep(NA, n.start)
    NA.end <- rep(NA, n.end)
    x <- c(NA.start, x, NA.end)
  }
  ## Create the sequence of windows.
  centers <- (1 + n.start):(len.x + n.start)
  windows <- lapply(centers, function(a) (a-n):(a+n.right))
  ## Apply to each window.
  return (unlist(lapply(windows, function(b) fun(x[b],...))))
}
