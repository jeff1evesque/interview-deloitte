##
## main.R, performs the following exploratory topics:
##
##     1. determine the mean, median, mode, and range of the race result for
##        all racers by gender
##
##     2. analyze the difference between gun, and net time race results
##
##     3. determine time difference between Chris Doe, from top 10 percentile
##        of racers of the same division
##
##     4. compare the race results of each division
##

## set project cwd
cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(cwd)

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/deloitteUtility'))
library('deloitteUtility')

## load packages
load_package('reshape2')
load_package('openintro')

## load datasets
data.f <- read.delim(
  'data/MA_Exer_PikesPeak_Females.txt',
  sep='\t', header=TRUE
)

data.m <- read.delim(
  'data/MA_Exer_PikesPeak_Males.txt',
  sep='\t', header=TRUE
)

## rename columns
colnames(data.f) <- tolower(c('place', 'division_place', 'bib', 'name', 'age', 'hometown', 'gun_time', 'net_time', 'pace'))
colnames(data.m) <- tolower(c('place', 'division_place', 'bib', 'name', 'age', 'hometown', 'gun_time', 'net_time', 'pace'))

## cleanup dataset
data.f$gun_time <- trimws(gsub('[#*a-zA-Z]', '', data.f$gun_time))
data.f$net_time <- trimws(gsub('[#*a-zA-Z]', '', data.f$net_time))
data.f$pace <- trimws(gsub('[#*a-zA-Z]', '', data.f$pace))
data.f$age <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', data.f$age)))
data.f$place <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', data.f$place)))
data.f$bib <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', data.f$bib)))
data.f$division_place <- trimws(gsub('[#*a-zA-Z]', '', data.f$division_place))
data.f$hometown <- trimws(gsub('[,.]', '', data.f$hometown))
data.f$hometown <- trimws(data.f$hometown)

data.m$gun_time <- trimws(gsub('[#*a-zA-Z]', '', data.m$gun_time))
data.m$net_time <- trimws(gsub('[#*a-zA-Z]', '', data.m$net_time))
data.m$pace <- trimws(gsub('[#*a-zA-Z]', '', data.m$pace))
data.m$age <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', data.m$age)))
data.m$place <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', data.m$place)))
data.m$bib <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', data.m$bib)))
data.m$division_place <- trimws(gsub('[#*a-zA-Z]', '', data.m$division_place))
data.m$hometown <- trimws(gsub('[,.]', '', data.m$hometown))
data.m$hometown <- trimws(data.m$hometown)

## insert delimiter
data.f$hometown <- lapply(data.f$hometown, function(x) {
  return(strReverse(sub(' ', ' ,', strReverse(x))))
})

data.m$hometown <- lapply(data.m$hometown, function(x) {
  return(strReverse(sub(' ', ' ,', strReverse(x))))
})

## explode column: 'hometown' column into 'city', and 'state' columns
data.f <- cbind(
  colsplit(data.f$hometown, ',', c('city', 'state')),
  data.f[,-which(names(data.f) == 'hometown')]
)

## explode column: 'hometown' column into 'city', and 'state' columns
data.m <- cbind(
  colsplit(data.m$hometown, ',', c('city', 'state')),
  data.m[,-which(names(data.m) == 'hometown')]
)

## consistent lowercase
data.f$city <- tolower(trimws(data.f$city))
data.f$city <- tolower(trimws(data.f$city))
data.f$state <- tolower(abbr2state(trimws(data.f$state)))
data.m$state <- tolower(abbr2state(trimws(data.m$state)))