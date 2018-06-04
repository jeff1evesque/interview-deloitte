##
## chris_doe.R, performs the following exploratory topics:
##
##     3. how much time separates chris doe from the top 10 percentile
##        of racers of the same division?
##

## set project cwd
cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(cwd)

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/deloitteUtility'))
library('deloitteUtility')

## load packages
load_package(c('reshape2', 'openintro', 'ggplot2', 'Rmisc', 'grid', 'gridExtra', 'oce', 'ggmap', 'fiftystater'))

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
## 3: analyze chris doe vs. 10th percentile same division
##
data.f.adjusted$name <- tolower(data.f.adjusted$name)
data.m.adjusted$name <- tolower(data.m.adjusted$name)
data.f.complete$name <- tolower(data.f.complete$name)
data.m.complete$name <- tolower(data.m.complete$name)

##
## chris doe's division: with adjusted anticipated values
##
row_adjusted <- which(data.m.adjusted$name == 'chris doe')
column_division_adjusted <- which(colnames(data.m.adjusted) == 'division')
column_net_time_adjusted <- which(colnames(data.m.adjusted) == 'net_time')
division_adjusted_chris <- data.m.adjusted[row_adjusted, column_division_adjusted]
time_adjusted_chris <- data.m.adjusted[row_adjusted, column_net_time_adjusted]

## create subset dataframe
data.m.adjusted.sub <- subset(data.m.adjusted[order(data.m.adjusted$net_time),], as.numeric(division) == division_chris, select = colnames(data.m.adjusted))
data.m.complete.sub <- subset(data.m.adjusted[order(data.m.adjusted$net_time),], as.numeric(division) == division_chris, select = colnames(data.m.adjusted))

## generate boxplot + violin
color_scale <- c('darkblue', 'darkred', 'darkgreen', 'dodgerblue2', 'darkorchid')
gg_full_adjusted <- ggplot(data.m.adjusted.sub, aes(x=factor(1), y=net_time)) +
  geom_violin(trim=FALSE, fill='darkorchid4') +
  geom_boxplot(width=0.25) +
  geom_hline(yintercept=time_adjusted_chris, color='gray10', size=0.75, linetype='dashed') +
  labs(x = 'State', y = 'Net Time (seconds)', title = 'Full Data: Net Time vs. State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_segregate_adjusted <- ggplot(data.m.adjusted.sub, aes(x=state, y=net_time)) +
  geom_violin(trim=FALSE, aes(fill=state, color=state)) +
  scale_fill_manual(values=color_scale) +
  scale_color_manual(values=color_scale) +
  geom_boxplot(width=0.75) +
  geom_hline(yintercept=time_adjusted_chris, color='gray10', size=0.75, linetype='dashed') +
  guides(color=FALSE) +
  labs(x = 'State', y = 'Net Time (seconds)', title = 'Net Time vs State', fill = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/10percentile-chris-adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_full_adjusted,
  gg_segregate_adjusted,
  nrow = 1,
  top = '10 Percentile vs Chris Doe: with adjusted anticipated values',
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
## chris doe's division: with empty values removed (not adjusted)
##
row_complete <- which(data.m.complete$name == 'chris doe')
column_division_complete <- which(colnames(data.m.complete) == 'division')
column_net_time_complete <- which(colnames(data.m.complete) == 'net_time')
division_complete_chris <- data.m.complete[row_complete, column_division_complete]
time_complete_chris <- data.m.complete[row_complete, column_net_time_complete]

## create subset dataframe
data.m.complete.sub <- subset(data.m.complete[order(data.m.complete$net_time),], as.numeric(division) == division_chris, select = colnames(data.m.complete))
data.m.complete.sub <- subset(data.m.complete[order(data.m.complete$net_time),], as.numeric(division) == division_chris, select = colnames(data.m.complete))

## generate boxplot + violin
color_scale <- c('darkblue', 'darkred', 'darkgreen', 'dodgerblue2', 'darkorchid')
gg_full_complete <- ggplot(data.m.complete.sub, aes(x=factor(1), y=net_time)) +
  geom_violin(trim=FALSE, fill='darkorchid4') +
  geom_boxplot(width=0.25) +
  geom_hline(yintercept=time_complete_chris, color='gray10', size=0.75, linetype='dashed') +
  labs(x = 'State', y = 'Net Time (seconds)', title = 'Full Data: Net Time vs. State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_segregate_complete <- ggplot(data.m.complete.sub, aes(x=state, y=net_time)) +
  geom_violin(trim=FALSE, aes(fill=state, color=state)) +
  scale_fill_manual(values=color_scale) +
  scale_color_manual(values=color_scale) +
  geom_boxplot(width=0.75) +
  geom_hline(yintercept=time_complete_chris, color='gray10', size=0.75, linetype='dashed') +
  guides(color=FALSE) +
  labs(x = 'State', y = 'Net Time (seconds)', title = 'Net Time vs State', fill = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/10percentile-chris-complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_full_complete,
  gg_segregate_complete,
  nrow = 1,
  top = '10 Percentile vs Chris Doe: with empty values removed (not adjusted)',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()
