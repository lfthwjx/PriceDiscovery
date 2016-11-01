# Generate a vector 
set.seed(158)
x <- round(rnorm(20, 10, 5))
x
#>  [1] 14 11  8  4 12  5 10 10  3  3 11  6  0 16  8 10  8  5  6  6

# For each element: is this one a duplicate (first instance of a particular value
# not counted)
duplicated(x)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE
#> [15]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

# The values of the duplicated entries
# Note that '6' appears in the original vector three times, and so it has two
# entries here.
x[duplicated(x)]
#> [1] 10  3 11  8 10  8  5  6  6

# Duplicated entries, without repeats
unique(x[duplicated(x)])
#> [1] 10  3 11  8  5  6

# The original vector with all duplicates removed. These do the same:
unique(x)
#>  [1] 14 11  8  4 12  5 10  3  6  0 16
x[!duplicated(x)]
#>  [1] 14 11  8  4 12  5 10  3  6  0 16

# Determine if range of vector is FP 0.
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

# A sample data frame:
df <- read.table(header=TRUE, text='
                 label value
                 A     4
                 B     3
                 C     6
                 B     3
                 B     1
                 A     2
                 A     4
                 A     4
                 ')


# Is each row a repeat?
duplicated(df)
#> [1] FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE

# Show the repeat entries
df[duplicated(df),]
#>   label value
#> 4     B     3
#> 7     A     4
#> 8     A     4

# Show unique repeat entries (row names may differ, but values are the same)
unique(df[duplicated(df),])
#>   label value
#> 4     B     3
#> 7     A     4

# Original data with repeats removed. These do the same:
unique(df)
#>   label value
#> 1     A     4
#> 2     B     3
#> 3     C     6
#> 5     B     1
#> 6     A     2
df[!duplicated(df),]
#>   label value
#> 1     A     4
#> 2     B     3
#> 3     C     6
#> 5     B     1
#> 6     A     2

#test for equality
all( abs(y - mean(y)) < tol ) 
abs(max(x) - min(x)) < tol

isTRUE(all.equal( max(y) ,min(y)) )
#TRUE
isTRUE(all.equal( max(x) ,min(x)) )
#FALSE

diff(range(x)) < .Machine$double.eps ^ 0.5
#FALSE
diff(range(y)) < .Machine$double.eps ^ 0.5
#TRUE


compare <- function(v) all(sapply( as.list(v[-1]), 
                                      +                         FUN=function(z) {identical(z, v[1])}))
compare(x)
#FALSE
compare(y)
#TRUE


# Determine if range of vector is FP 0.
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}