##
## strReverse.R, reverses a string.
##
## Note: https://stackoverflow.com/a/13613183
##
strReverse <- function(x) {
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = '')
}
