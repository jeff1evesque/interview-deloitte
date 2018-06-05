##
## descriptive_statistics.R, performs the following exploratory topics:
##
##     1. determine the mean, median, mode, and range of the race result for
##        all racers by gender
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(cwd)
}

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
## barchart: mean, median
##
data.descriptive.adjusted <- data.frame(
  c(mean.f.adjusted, mean.m.adjusted),
  c(median.f.adjusted, median.m.adjusted)
)
colnames(data.descriptive.adjusted) <- c('mean', 'median')
rownames(data.descriptive.adjusted) <- c('female', 'male')

data.descriptive.complete <- data.frame(
  c(mean.f.complete, mean.m.complete),
  c(median.f.complete, median.m.complete)
)
colnames(data.descriptive.complete) <- c('mean', 'median')
rownames(data.descriptive.complete) <- c('female', 'male')

## melt dataframe
data.descriptive.adjusted <- melt(as.matrix(data.descriptive.adjusted))
data.descriptive.complete <- melt(as.matrix(data.descriptive.complete))

##
## mean + median: with adjusted anticipated values
##
gg_descriptive_adjusted <- ggplot(data.descriptive.adjusted) +
  geom_bar(aes(x=Var1, y=value, fill=Var1), stat = 'identity') +
  facet_wrap(~Var2) +
  labs(x = 'Gender', y = 'Net Time (seconds)', title = 'Gender: with adjusted anticipated values', fill='Gender') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c('darkred', 'darkblue'))

ggsave(
  'visualization/mean-median-adjusted.png',
  width = 16,
  height = 9,
  dpi = 100
)

##
## mean + median: with empty values removed (not adjusted)
##
gg_descriptive_complete <- ggplot(data.descriptive.complete) +
  geom_bar(aes(x=Var1, y=value, fill=Var1), stat = 'identity') +
  facet_wrap(~Var2) +
  labs(x = 'Gender', y = 'Net Time (seconds)', title = 'Net Time vs. Gender: with empty values removed (not adjusted)', fill='Gender') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c('darkred', 'darkblue'))

ggsave(
  'visualization/mean-median-complete.png',
  width = 16,
  height = 9,
  dpi = 100
)

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
