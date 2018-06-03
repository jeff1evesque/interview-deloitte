##
## gun_delta_net.R, performs the following exploratory topics:
##
##     2. analyze the difference between gun, and net time race results
##

## set project cwd
cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(cwd)

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/deloitteUtility'))
library('deloitteUtility')

## load packages
load_package(c('reshape2', 'openintro', 'ggplot2', 'Rmisc', 'grid', 'gridExtra'))

## create ignored directories
dir.create(file.path(cwd, 'visualization'), showWarnings = FALSE)

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

## convert time to seconds
data.f$gun_time <- unlist(lapply(data.f$gun_time, ctimeToSeconds))
data.f$net_time <- unlist(lapply(data.f$net_time, ctimeToSeconds))
data.f$pace <- unlist(lapply(data.f$pace, ctimeToSeconds))

data.m$gun_time <- unlist(lapply(data.m$gun_time, ctimeToSeconds))
data.m$net_time <- unlist(lapply(data.m$net_time, ctimeToSeconds))
data.m$pace <- unlist(lapply(data.m$pace, ctimeToSeconds))

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

## complete case: remove any rows with NA
data.f.complete <- data.f[complete.cases(data.f),]
data.m.complete <- data.m[complete.cases(data.m),]

## incomplete case: anticipate missing values
data.f.adjusted <- data.f
data.m.adjusted <- data.m

data.f.adjusted$state[data.f$city == 'silver spring' & is.na(data.f$state)] <- 'maryland'
data.f.adjusted$state[data.f$city == 'ellicott city' & is.na(data.f$state)] <- 'maryland'
data.f.adjusted$state[data.f$city == 'north potomac' & is.na(data.f$state)] <- 'maryland'
data.f.adjusted$state[data.f$city == 'fredericksburg' & is.na(data.f$state)] <- 'virginia'
data.f.adjusted$state[data.f$city == 'washington' & is.na(data.f$state)] <- 'district of columbia'
data.f.adjusted$state[data.f$city == 'potomac falls' & is.na(data.f$state)] <- 'maryland'
data.f.adjusted$city[data.f$city == 'potomac falls'] <- 'potomac'

data.m.adjusted$state[data.m$city == 'silver spring' & is.na(data.m$state)] <- 'maryland'
data.m.adjusted$state[data.m$city == 'ellicott city' & is.na(data.m$state)] <- 'maryland'
data.m.adjusted$state[data.m$city == 'north potomac' & is.na(data.m$state)] <- 'maryland'
data.m.adjusted$state[data.m$city == 'fredericksburg' & is.na(data.m$state)] <- 'virginia'
data.m.adjusted$state[data.m$city == 'washington' & is.na(data.m$state)] <- 'district of columbia'
data.m.adjusted$state[data.m$city == 'potomac falls' & is.na(data.m$state)] <- 'maryland'
data.m.adjusted$city[data.m$city == 'potomac falls'] <- 'potomac'

data.f.adjusted$age[data.f.adjusted$age < 1] <- NA
data.m.adjusted$age[data.m.adjusted$age < 1] <- NA

data.f.adjusted <- data.f.adjusted[complete.cases(data.f.adjusted),]
data.m.adjusted <- data.m.adjusted[complete.cases(data.m.adjusted),]

##
## 2: mean, median, mode, and range
##
## Note: https://rpubs.com/mohammadshadan/meanmedianmode
##
