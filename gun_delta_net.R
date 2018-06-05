##
## gun_delta_net.R, performs the following exploratory topics:
##
##     2. analyze the difference between gun, and net time race results
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
## 2: analyze the difference between gun, and net time race results
##
data.f.adjusted$delta_time <- data.f.adjusted$gun_time - data.f.adjusted$net_time
data.f.complete$delta_time <- data.f.complete$gun_time - data.f.complete$net_time
data.m.adjusted$delta_time <- data.m.adjusted$gun_time - data.m.adjusted$net_time
data.m.complete$delta_time <- data.m.complete$gun_time - data.m.complete$net_time

## generate ggmap
us <- map_data('state')
gg <- ggplot() +
  geom_map(data=us, map=us, aes(x=us$long, y=us$lat, map_id=region), fill='gray75', color='gray7', size=0.15)

##
## us heatmap: females with adjusted anticipated values
##
gg_adjusted_females_heatmap <- gg +
  geom_map(data=data.f.adjusted, map=us, aes(fill=delta_time, map_id=state), color='gray10') +
  guides(color=FALSE) +
  coord_map() +
  expand_limits(x=us$long, y=us$lat) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Female Runners', fill = 'Delta time') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(low='darkred', high='orange')

##
## us heatmap: males with adjusted anticipated values
##
gg_adjusted_males_heatmap <- gg +
  geom_map(data=data.m.adjusted, map=us, aes(fill=delta_time, map_id=state), color='gray10') +
  guides(color=FALSE) +
  coord_map() +
  expand_limits(x=us$long, y=us$lat) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Male Runners', fill = 'Delta time') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/us-heatmap-adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_adjusted_females_heatmap,
  gg_adjusted_males_heatmap,
  nrow = 1,
  top = 'Gun Time minus Net Time: with anticipated values',
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
## us heatmap: females with complete with empty values removed (not adjusted)
##
gg_complete_females_heatmap <- gg +
  geom_map(data=data.f.complete, map=us, aes(fill=delta_time, map_id=state), color='gray10') +
  guides(color=FALSE) +
  coord_map() +
  expand_limits(x=us$long, y=us$lat) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Female Runners', fill = 'Delta time') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(low='darkred', high='orange')

##
## us heatmap: males with empty values removed (not adjusted)
##
gg_complete_males_heatmap <- gg +
  geom_map(data=data.m.complete, map=us, aes(fill=delta_time, map_id=state), color='gray10') +
  guides(color=FALSE) +
  coord_map() +
  expand_limits(x=us$long, y=us$lat) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Male Runners', fill = 'Delta time') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/us-heatmap-complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_complete_females_heatmap,
  gg_complete_males_heatmap,
  nrow = 1,
  top = 'Gun Time minus Net Time: with NA values removed',
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
## histogram + boxplot: females with adjusted anticipated values
##
gg_adjusted_females <- ggplot(data.f.adjusted)

gg_adjusted_females_bar <- gg_adjusted_females +
  geom_bar(stat = 'summary', fun.y = 'mean', color='black', aes(x=age, y=delta_time, fill=state)) +
  labs(x = 'Age', y = 'Delta Time (g-n)', title = 'Delta Time vs. Female Age', fill = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_adjusted_females_boxplot <- gg_adjusted_females +
  geom_boxplot(aes(x=factor(1), y=delta_time), alpha=0.5) +
  labs(x = 'Females', y = 'Delta Time (seconds)', title = 'Delta Time vs. Female Runners') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/deltatime-female-adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_adjusted_females_bar,
  gg_adjusted_females_boxplot,
  nrow = 1,
  top = 'Gun Time minus Net Time: with adjusted anticipated values',
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
## histogram + boxplot: males with adjusted anticipated values
##
gg_adjusted_males <- ggplot(data.m.adjusted)

gg_adjusted_males_bar <- gg_adjusted_males +
  geom_bar(stat = 'summary', fun.y = 'mean', color='black', aes(x=age, y=delta_time, fill=state)) +
  labs(x = 'Age', y = 'Delta Time (g-n)', title = 'Delta Time vs. Males Age', fill = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_adjusted_males_boxplot <- gg_adjusted_males +
  geom_boxplot(aes(x=factor(1), y=delta_time), alpha=0.5) +
  labs(x = 'males', y = 'Delta Time (seconds)', title = 'Delta Time vs. Males Runners') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/deltatime-males-adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_adjusted_males_bar,
  gg_adjusted_males_boxplot,
  nrow = 1,
  top = 'Gun Time minus Net Time: with adjusted anticipated values',
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
## histogram + boxplot: females with empty values removed (not adjusted)
##
gg_complete_females <- ggplot(data.f.complete)

gg_complete_females_bar <- gg_complete_females +
  geom_bar(stat = 'summary', fun.y = 'mean', color='black', aes(x=age, y=delta_time, fill=state)) +
  labs(x = 'Age', y = 'Delta Time (g-n)', title = 'Delta Time vs. Female Age', fill = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_complete_females_boxplot <- gg_complete_females +
  geom_boxplot(aes(x=factor(1), y=delta_time), alpha=0.5) +
  labs(x = 'Females', y = 'Delta Time (seconds)', title = 'Delta Time vs. Female Runners') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/deltatime-female-complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_complete_females_bar,
  gg_complete_females_boxplot,
  nrow = 1,
  top = 'Gun Time minus Net Time: with NA values removed',
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
## histogram + boxplot: males with empty values removed (not adjusted)
##
gg_complete_males <- ggplot(data.m.complete)

gg_complete_males_bar <- gg_complete_males +
  geom_bar(stat = 'summary', fun.y = 'mean', color='black', aes(x=age, y=delta_time, fill=state)) +
  labs(x = 'Age', y = 'Delta Time (g-n)', title = 'Delta Time vs. Males Age', fill = 'State') +
  theme(plot.title = element_text(hjust = 0.5))

gg_complete_males_boxplot <- gg_complete_males +
  geom_boxplot(aes(x=factor(1), y=delta_time), alpha=0.5) +
  labs(x = 'males', y = 'Delta Time (seconds)', title = 'Delta Time vs. Males Runners') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/deltatime-males-complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_complete_males_bar,
  gg_complete_males_boxplot,
  nrow = 1,
  top = 'Gun Time minus Net Time: with NA values removed',
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
## guntime + nettime correlation: females with adjusted anticipated values
##
gg_correlation_female_adjusted <- ggplot(data.f.adjusted, aes(x = net_time, y = gun_time, color=as.numeric(division))) +
  geom_line() +
  labs(x = 'Net Time (seconds)', y = 'Gun Time (seconds)', title = 'Females: Gun Time vs. Net Time', color = 'Division') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_gradient(low='darkmagenta', high='orange')

##
## guntime + nettime correlation: males with adjusted anticipated values
##
gg_correlation_male_adjusted <- ggplot(data.m.adjusted, aes(x = net_time, y = gun_time, color=as.numeric(division))) +
  geom_line() +
  labs(x = 'Net Time (seconds)', y = 'Gun Time (seconds)', title = 'Males: Gun Time vs. Net Time', color = 'Division') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/correlation-times-adjusted.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_correlation_female_adjusted,
  gg_correlation_male_adjusted,
  nrow = 1,
  top = 'Correlation: with anticipated values',
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
## guntime + nettime correlation: females with empty values removed (not adjusted)
##
gg_correlation_female_complete <- ggplot(data.f.complete, aes(x = net_time, y = gun_time, color=as.numeric(division))) +
  geom_line() +
  labs(x = 'Net Time (seconds)', y = 'Gun Time (seconds)', title = 'Females: Gun Time vs. Net Time', color = 'Division') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_gradient(low='darkmagenta', high='orange')

##
## guntime + nettime correlation: males with empty values removed (not adjusted)
##
gg_correlation_male_complete <- ggplot(data.m.complete, aes(x = net_time, y = gun_time, color=as.numeric(division))) +
  geom_line() +
  labs(x = 'Net Time (seconds)', y = 'Gun Time (seconds)', title = 'Males: Gun Time vs. Net Time', color = 'Division') +
  theme(plot.title = element_text(hjust = 0.5))

## save visualization
png('visualization/correlation-times-complete.png', width = 1200, height = 600)

## generate grid to contain visualization
grid.arrange(
  gg_correlation_female_complete,
  gg_correlation_male_complete,
  nrow = 1,
  top = 'Correlation: with NA values removed',
  bottom = textGrob(
    'Jeffrey Levesque',
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

## close current plot
dev.off()

