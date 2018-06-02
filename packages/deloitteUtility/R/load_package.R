##
## load_package.R, install + install defined package
##
load_package <- function(package) {
  if (is.vector(package)) {
    for (i in package){
      if(!is.element(i, .package(all.available = TRUE)) ) {
        install.packages(i)
      }
      library(i,character.only = TRUE)
    }
  } else {
    if (!require(package, character.only=TRUE)) install.packages(package)
    library(package, character.only = TRUE)
  }
}
