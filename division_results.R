##
## division_results.R, performs the following exploratory topics:
##
##     4. analyze the race results for each division
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
## 4: analyze the race results for each division
##
data.f.adjusted <- as.data.frame(lapply(data.f.adjusted, unlist))
data.m.adjusted <- as.data.frame(lapply(data.m.adjusted, unlist))

gg_female_adjusted_divisions <- ggplot(data.f.adjusted, aes(x = division, y = net_time)) +
  geom_boxplot(aes(group=division), outlier.shape = NA) +
  geom_point(aes(color=state), alpha=0.35, position = 'jitter') +
  labs(x = 'Division', y = 'Net Time (seconds)', title = 'Females: Net Time vs Division', color = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_male_adjusted_divisions <- ggplot(data.m.adjusted, aes(x = division, y = net_time)) +
  geom_boxplot(aes(group=division), outlier.shape = NA) +
  geom_point(aes(color=state), alpha=0.35, position = 'jitter') +
  labs(x = 'Division', y = 'Net Time (seconds)', title = 'Males: Net Time vs Division', color = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/divisions_net_time_adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_female_adjusted_divisions,
  gg_male_adjusted_divisions,
  nrow = 1,
  top = 'Net Time vs Division: with adjusted anticipated values',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()

data.f.complete <- as.data.frame(lapply(data.f.complete, unlist))
data.m.complete <- as.data.frame(lapply(data.m.complete, unlist))

gg_female_complete_divisions <- ggplot(data.f.complete, aes(x = division, y = net_time)) +
  geom_boxplot(aes(group=division), outlier.shape = NA) +
  geom_point(aes(color=state), alpha=0.35, position = 'jitter') +
  labs(x = 'Division', y = 'Net Time (seconds)', title = 'Females: Net Time vs Division', color = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_male_complete_divisions <- ggplot(data.m.complete, aes(x = division, y = net_time)) +
  geom_boxplot(aes(group=division), outlier.shape = NA) +
  geom_point(aes(color=state), alpha=0.35, position = 'jitter') +
  labs(x = 'Division', y = 'Net Time (seconds)', title = 'Males: Net Time vs Division', color = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/divisions_net_time_complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_female_complete_divisions,
  gg_male_complete_divisions,
  nrow = 1,
  top = 'Net Time vs Division: with empty values removed (not adjusted)',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()
