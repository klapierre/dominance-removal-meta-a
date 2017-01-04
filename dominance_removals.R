library(ggplot2)
library(grid)
library(nlme)
library(plyr)
library(tidyr)
library(dplyr)

setwd("C:\\Users\\Kim\\Dropbox\\Dominance ms\\Analyses\\removal_metaanalysis")

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=34, color='black'),
             axis.title.y=element_text(size=40, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=34, color='black'),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


###bar graph summary statistics function
#barGraphStats(data=, variable="", byFactorNames=c(""))

barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  

##################################################################################
##################################################################################
#read in removals data
removals <- read.csv('removal data_11032016.csv')%>%
  #calculate lrr
  mutate(lrr=log(treatment_mean/control_mean))%>%
  #calculate abs value of lrr
  mutate(abs_lrr=abs(lrr))
  
#split into community, ecosystem, and competition responses
ecosystem <- subset(removals, response_recategorized=='ecosystem')%>%
  mutate(response_variable_code=ifelse(response_variable=='productivity'|response_variable=='total cover (proxy for biomass)'|response_variable=='total vascular cover (proxy for biomass)', 'productivity', 'other'))
community <- subset(removals, response_recategorized=='community')%>%
  mutate(response_variable_code=ifelse(response_variable=='richness', 'richness', 'other'))
competition <- subset(removals, response_recategorized=='competition')


#######################
###directional responses

#mixed effects models
summary(lrrModel <- lme(lrr ~ pulse.press + study_months + study_system_detail, random=~1|NUM, data=removals))

###ttests - lnRR
t.test(ecosystem$lrr, alternative='t', conf.level=0.983334) #bonferroni corrected conf.level for 3 tests
t.test(community$lrr, alternative='t', conf.level=0.983334)
t.test(competition$lrr, alternative='t', conf.level=0.983334)
#ecosystem, competition, and community are not significantly diff from 0



#######################
###abs value of lrr

#mixed effects models
summary(lrrModel <- lme(abs_lrr ~ pulse.press + study_months + study_system_detail, random=~1|NUM, data=removals))

###ttests - abs lnRR
t.test(ecosystem$abs_lrr, alternative='t', conf.level=0.983334)
t.test(community$abs_lrr, alternative='t', conf.level=0.983334)
t.test(competition$abs_lrr, alternative='t', conf.level=0.983334)
#ecosystem and community are significantly diff from 0



#######################
###figures
#abs lnRR
absLnRRPlot <- ggplot(removals, aes(x=response_recategorized, y=abs_lrr)) +
  geom_violin() +
  geom_boxplot(width=0.05) +
  xlab('') +
  ylab('|lnRR|') +
  scale_y_continuous(breaks=seq(-4,8,2)) +
  coord_cartesian(ylim=c(0,8), xlim=c(1,3)) +
  scale_x_discrete(labels=c('', '', '')) +
  annotate('text', x=1.1, y=0.8, label='*', size=10, hjust='left') +
  annotate('text', x=3.1, y=0.8, label='*', size=10, hjust='left') +
  annotate('text', x=0.6, y=8, label='(a)', size=10, hjust='left')

#lnRR
lnRRPlot <- ggplot(removals, aes(x=response_recategorized, y=lrr)) +
  geom_violin() +
  geom_boxplot(width=0.05) +
  xlab('') +
  ylab('lnRR') +
  scale_y_continuous(breaks=seq(-4,8,2)) +
  coord_cartesian(ylim=c(-4,8), xlim=c(1,3)) +
  scale_x_discrete(labels=c('community\n(45)', 'competition\n(12)', 'function\n(49)')) +
  annotate('text', x=0.6, y=8, label='(b)', size=10, hjust='left')

pushViewport(viewport(layout=grid.layout(2,1)))
print(absLnRRPlot, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(lnRRPlot, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
#export at 900x1400