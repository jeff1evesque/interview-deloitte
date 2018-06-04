##
## descriptive_statistics.R, performs the following exploratory topics:
##
##     1. determine the mean, median, mode, and range of the race result for
##        all racers by gender
##

## set project cwd
cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(cwd)

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/deloitteUtility'))
library('deloitteUtility')

## load packages
load_package(c('reshape2', 'openintro', 'ggplot2', 'Rmisc', 'grid', 'gridExtra', 'oce'))

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

## complete case: define division
data.f.complete$division <- create_division(data.f.complete$age)
data.m.complete$division <- create_division(data.m.complete$age)

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

data.f.adjusted$division <- create_division(data.f.adjusted$age)
data.m.adjusted$division <- create_division(data.m.adjusted$age)

##
## 1: mean, median, mode, and range
##
## Note: https://rpubs.com/mohammadshadan/meanmedianmode
##
mean.f.adjusted <- mean(data.f.adjusted$net_time)
median.f.adjusted <- median(data.f.adjusted$net_time)
mode.f.adjusted <- names(table(data.f.adjusted$net_time))[table(data.f.adjusted$net_time)==max(table(data.f.adjusted$net_time))]
range.f.adjusted <- range(data.f.adjusted$net_time)

mean.m.adjusted <- mean(data.m.adjusted$net_time)
median.m.adjusted <- median(data.m.adjusted$net_time)
mode.m.adjusted <- names(table(data.m.adjusted$net_time))[table(data.m.adjusted$net_time)==max(table(data.m.adjusted$net_time))]
range.m.adjusted <- range(data.m.adjusted$net_time)

mean.f.complete <- mean(data.f.complete$net_time)
median.f.complete <- median(data.f.complete$net_time)
mode.f.complete <- names(table(data.f.complete$net_time))[table(data.f.complete$net_time)==max(table(data.f.complete$net_time))]
range.f.complete <- range(data.f.complete$net_time)

mean.m.complete <- mean(data.m.complete$net_time)
median.m.complete <- median(data.m.complete$net_time)
mode.m.complete <- names(table(data.m.complete$net_time))[table(data.m.complete$net_time)==max(table(data.m.complete$net_time))]
range.m.complete <- range(data.m.complete$net_time)

##
## barchart: mean, median, mode
##
data.descriptive <- data.frame(
  c(mean.f.adjusted, median.f.adjusted),
  c(mean.m.adjusted, median.m.adjusted),
  c(mean.f.complete, median.f.complete),
  c(mean.m.complete, median.m.complete)
)
colnames(data.descriptive) <- c('female_adjusted', 'male_adjusted', 'female_complete', 'male_complete')
rownames(data.descriptive) <- c('mean', 'median')

##
## boxplot + points: females with adjusted anticipated values
##
gg_adjusted_females <- ggplot(data.f.adjusted)

gg_adjusted_females_boxplot <- gg_adjusted_females +
  geom_boxplot(aes(x=factor(1), y=net_time), alpha=0.5) +
  labs(x = 'Females', y = 'Net Time (seconds)', title = 'Net Time vs. Female Runners') +
  theme(plot.title = element_text(hjust = 0.5))

gg_adjusted_females_points <- gg_adjusted_females +
  geom_point(stat = 'identity', aes(x=age, y=net_time), alpha=0.35) +
  geom_abline(slope=0, intercept = mean.f.adjusted, color = 'dodgerblue3') +
  geom_abline(slope=0, intercept = median.f.adjusted, color = 'darkorchid3') +
  labs(x = 'Age', y = 'Net Time (seconds)', title = 'Net Time vs. Age') +
  theme(plot.title = element_text(hjust = 0.5))

for (mode in mode.f.adjusted) {
  gg_adjusted_females_points <- gg_adjusted_females_points +
    geom_abline(slope=0, intercept = as.numeric(mode), color = 'darkgreen')
}

## save visualization
png('visualization/points-females-adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_adjusted_females_boxplot,
  gg_adjusted_females_points,
  nrow = 1,
  top = 'Female Runners: with anticipated values',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()

##
## boxplot + points: males with adjusted anticipated values
##
gg_adjusted_males <- ggplot(data.m.adjusted)

gg_adjusted_males_boxplot <- gg_adjusted_males +
  geom_boxplot(aes(x=factor(1), y=net_time), alpha=0.5) +
  labs(x = 'Males', y = 'Net Time (seconds)', title = 'Net Time vs. Male Runners') +
  theme(plot.title = element_text(hjust = 0.5))

gg_adjusted_males_points <- gg_adjusted_males +
  geom_point(stat = 'identity', aes(x=age, y=net_time), alpha=0.35) +
  geom_abline(slope=0, intercept = mean.m.adjusted, color = 'dodgerblue3') +
  geom_abline(slope=0, intercept = median.m.adjusted, color = 'darkorchid3') +
  labs(x = 'Age', y = 'Net Time (seconds)', title = 'Net Time vs. Age') +
  theme(plot.title = element_text(hjust = 0.5))

for (mode in mode.m.adjusted) {
  gg_adjusted_males_points <- gg_adjusted_males_points +
    geom_abline(slope=0, intercept = as.numeric(mode), color = 'darkgreen')
}

## save visualization
png('visualization/points-males-adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_adjusted_males_boxplot,
  gg_adjusted_males_points,
  nrow = 1,
  top = 'Male Runners: with anticipated values',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()

##
## boxplot + points: females with empty values removed (not adjusted)
##
gg_complete_females <- ggplot(data.f.complete)

gg_complete_females_boxplot <- gg_complete_females +
  geom_boxplot(aes(x=factor(1), y=net_time), alpha=0.5) +
  labs(x = 'Females', y = 'Net Time (seconds)', title = 'Net Time vs. Female Runners') +
  theme(plot.title = element_text(hjust = 0.5))

gg_complete_females_points <- gg_complete_females +
  geom_point(stat = 'identity', aes(x=age, y=net_time), alpha=0.35) +
  geom_abline(slope=0, intercept = mean.f.complete, color = 'dodgerblue3') +
  geom_abline(slope=0, intercept = median.f.complete, color = 'darkorchid3') +
  labs(x = 'Age', y = 'Net Time (seconds)', title = 'Net Time vs. Age') +
  theme(plot.title = element_text(hjust = 0.5))

for (mode in mode.f.complete) {
  gg_complete_females_points <- gg_complete_females_points +
    geom_abline(slope=0, intercept = as.numeric(mode), color = 'darkgreen')
}

## save visualization
png('visualization/points-females-complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_complete_females_boxplot,
  gg_complete_females_points,
  nrow = 1,
  top = 'Female Runners: with NA values removed',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()

##
## boxplot + points: males with empty values removed (not adjusted)
##
gg_complete_males <- ggplot(data.m.complete)

gg_complete_males_boxplot <- gg_complete_males +
  geom_boxplot(aes(x=factor(1), y=net_time), alpha=0.5) +
  labs(x = 'Males', y = 'Net Time (seconds)', title = 'Net Time vs. Male Runners') +
  theme(plot.title = element_text(hjust = 0.5))

gg_complete_males_points <- gg_complete_males +
  geom_point(stat = 'identity', aes(x=age, y=net_time), alpha=0.35) +
  geom_abline(slope=0, intercept = mean.m.complete, color = 'dodgerblue3') +
  geom_abline(slope=0, intercept = median.m.complete, color = 'darkorchid3') +
  labs(x = 'Age', y = 'Net Time (seconds)', title = 'Net Time vs. Age') +
  theme(plot.title = element_text(hjust = 0.5))

for (mode in mode.m.complete) {
  gg_complete_males_points <- gg_complete_males_points +
    geom_abline(slope=0, intercept = as.numeric(mode), color = 'darkgreen')
}

## save visualization
png('visualization/points-males-complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_complete_males_boxplot,
  gg_complete_males_points,
  nrow = 1,
  top = 'Male Runners: with NA values removed',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()
