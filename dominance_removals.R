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
  #calculate hedges' d for studies with variance
  mutate(hedges_d=((treatment_mean-control_mean)/sqrt(((treatment_number-1)*treatment_variance+(control_number-1)*control_variance)/(treatment_number+control_number-2)))*(1-(3/(4*(control_number+treatment_number-2)-1))))%>%
  #calculate abs value of lrr
  mutate(abs_lrr=abs(lrr))%>%
  #calculate abs value of hedges' d
  mutate(abs_hedges_d=abs(hedges_d))
  
#split into community, ecosystem, and competition responses
ecosystem <- subset(removals, response_recategorized=='ecosystem')%>%
  mutate(response_variable_code=ifelse(response_variable=='productivity'|response_variable=='total cover (proxy for biomass)'|response_variable=='total vascular cover (proxy for biomass)', 'productivity', 'other'))
community <- subset(removals, response_recategorized=='community')%>%
  mutate(response_variable_code=ifelse(response_variable=='richness', 'richness', 'other'))
competition <- subset(removals, response_recategorized=='competition')

#mixed effects models
summary(lrrModel <- lme(lrr ~ pulse.press + response_recategorized + study_months + study_system_detail, random=~1|NUM, data=removals))
summary(hedgesModel <- lme(hedges_d ~ pulse.press + response_recategorized + study_months + study_system_detail, random=~1|NUM, data=subset(removals, !is.na(hedges_d))))

###ttests - lnRR
# ecosystemProductivity <- subset(ecosystem, response_variable=='productivity')
# ecosystemOther <- subset(ecosystem, response_variable!='productivity')
# t.test(ecosystemProductivity$lrr, alternative='t', conf.level=0.95)
# t.test(ecosystemOther$lrr, alternative='t', conf.level=0.95)


t.test(ecosystem$lrr, alternative='t', conf.level=0.983334) #bonferroni corrected conf.level for 3 tests

t.test(community$lrr, alternative='t', conf.level=0.983334)

t.test(competition$lrr, alternative='t', conf.level=0.983334)


###ttests - hedges' d
t.test(ecosystem$hedges_d, alternative='t', conf.level=0.983334)

t.test(community$hedges_d, alternative='t', conf.level=0.983334)

t.test(competition$hedges_d, alternative='t', conf.level=0.983334)



###plot - lnRR
ggplot(removals, aes(x=response_recategorized, y=lrr)) +
  geom_boxplot() +
  xlab('') +
  ylab('ln Response Ratio') +
  scale_y_continuous(breaks=seq(-4,8,2)) +
  scale_x_discrete(labels=c('community\n(45)', 'competition\n(12)', 'function\n(49)'))
#export at 900x700

#hedges' d
ggplot(removals, aes(x=response_recategorized, y=hedges_d)) +
  geom_boxplot() +
  xlab('') +
  ylab('Hedges d') +
  # scale_y_continuous(breaks=seq(-4,8,2)) +
  scale_x_discrete(labels=c('community\n(29)', 'competition\n(11)', 'function\n(38)'))


#######################

###abs value of lrr

#mixed effects models
summary(lrrModel <- lme(abs_lrr ~ pulse.press + response_recategorized + study_months + study_system_detail, random=~1|NUM, data=removals))
summary(hedgesModel <- lme(abs_hedges_d ~ pulse.press + response_recategorized + study_months + study_system_detail, random=~1|NUM, data=subset(removals, !is.na(abs_hedges_d))))

###ttests - lnRR
t.test(ecosystem$abs_lrr, alternative='t', conf.level=0.983334)

t.test(community$abs_lrr, alternative='t', conf.level=0.983334)

t.test(competition$abs_lrr, alternative='t', conf.level=0.983334)
#ecosystem, community, and competition are all significantly diff from 0 when abs value

###plot - lnRR
absLnRRPlot <- ggplot(removals, aes(x=response_recategorized, y=abs_lrr)) +
  geom_boxplot() +
  xlab('') +
  ylab('|ln Response Ratio|') +
  # scale_y_continuous(breaks=seq(-4,8,2)) +
  scale_x_discrete(labels=c('community\n(45)', 'competition\n(12)', 'function\n(49)')) +
  annotate('text', x=1.1, y=0.8, label='*', size=10, hjust='left') +
  annotate('text', x=3.1, y=0.8, label='*', size=10, hjust='left') +
annotate('text', x=0, y=7, label='(a)', size=10, hjust='left')
#export at 900x700