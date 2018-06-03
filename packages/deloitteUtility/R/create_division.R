##
## strReverse.R, reverses a string.
##
## Note: https://stackoverflow.com/a/13613183
##
create_division <- function(vector) {
  return(
    lapply(
      vector,
      function (x) {
        if (!as.numeric(trimws(x))) {
          return(NA)
        } else if (x < 14) {
          return(1)
        } else if (x >= 14 & x < 19) {
          return(2)
        } else {
          return(x%/%10)
        }
      }
    )
  )
}
