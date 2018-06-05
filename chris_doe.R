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
load_package(c('ggplot2', 'grid', 'gridExtra', 'oce'))

## create dataframes
data.f.adjusted <- munger('MA_Exer_PikesPeak_Females.txt')
data.m.adjusted <- munger('MA_Exer_PikesPeak_Males.txt')
data.f.complete <- munger('MA_Exer_PikesPeak_Females.txt', TRUE)
data.m.complete <- munger('MA_Exer_PikesPeak_Males.txt', TRUE)

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
column_division <- which(colnames(data.m.adjusted) == 'division')
column_net_time <- which(colnames(data.m.adjusted) == 'net_time')
division_chris <- data.m.adjusted[row_adjusted, column_division]
time_adjusted_chris <- data.m.adjusted[row_adjusted, column_net_time]

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
