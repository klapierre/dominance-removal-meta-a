library(ggplot2)
library(grid)
library(nlme)
library(plyr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(car)

setwd("C:\\Users\\Kim\\Dropbox\\Dominance ms\\Analyses\\removal_metaanalysis")
#meghan
setwd("~/Dropbox/Dominance ms/Analyses/removal_metaanalysis")

setwd("C:\\Users\\megha\\Dropbox\\manuscripts\\Dominance ms\\Analyses\\removal_metaanalysis")

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
  
# #split into community, ecosystem, and competition responses
# ecosystem <- subset(removals, response_recategorized=='ecosystem')%>%
#   mutate(response_variable_code=ifelse(response_variable=='productivity'|response_variable=='total cover (proxy for biomass)'|response_variable=='total vascular cover (proxy for biomass)', 'productivity', 'other'))
# community <- subset(removals, response_recategorized=='community')%>%
#   mutate(response_variable_code=ifelse(response_variable=='richness', 'richness', 'other'))
# competition <- subset(removals, response_recategorized=='competition')

#ecosystem type
system<-removals%>%
  filter(response_recategorized!="competition")%>%
  group_by(study_system)%>%
  summarise(n=length(study_system))
type<-removals%>%
  filter(response_recategorized!="competition")%>%
  group_by(pulse.press)%>%
  summarise(n=length(pulse.press))


#######################
###directional responses

###ttests - lnRR
#we are not going to bonferonni correct because we are only doing 2 t-tests
t.test(ecosystem$lrr, alternative='t') #bonferroni corrected conf.level for 2 tests
t.test(community$lrr, alternative='t')
#t.test(competition$lrr, alternative='t', conf.level=0.983334)
#ecosystem, competition, and community are not significantly diff from 0

#GLM ecosystem
M1e<-glm(lrr ~ pulse.press + study_months + study_system_detail, data=subset(removals, response_recategorized=="ecosystem"))
summary(M1e)
Anova(M1e)


#GLM community
M1c<-glm(lrr ~ pulse.press + study_months + study_system_detail, data=subset(removals, response_recategorized=="community"))
summary(M1c)
Anova(M1c)

#######################
###abs value of lrr

###ttests - abs lnRR
t.test(ecosystem$abs_lrr, alternative='t')
t.test(community$abs_lrr, alternative='t')
#t.test(competition$abs_lrr, alternative='t', conf.level=0.983334)
#ecosystem and community are significantly diff from 0

#GLM - ecosystem
M2e<-glm(abs_lrr ~ pulse.press + study_months + study_system_detail, data=subset(removals, response_recategorized=="ecosystem"))
summary(M2e)
Anova(M2e)

#GLM community
M2c<-glm(abs_lrr ~ pulse.press + study_months + study_system_detail, data=subset(removals, response_recategorized=="community"))
summary(M2c)
Anova(M2c)







#######################
###figures
#abs lnRR
theme_set(theme_bw(16))
absLnRRPlot <- 
  ggplot(removals, aes(x=response_recategorized, y=abs_lrr)) +
  #geom_violin() +
  geom_boxplot(width=.5) +
  xlab('') +
  ylab('|lnRR|') +
  # scale_y_continuous(breaks=seq(-4,8,2)) +
  # coord_cartesian(ylim=c(0,8), xlim=c(1,3)) +
  scale_x_discrete(labels=c('', '', '')) +
  annotate('text', x=1.1, y=0.8, label='*', size=10, hjust='left') +
  annotate('text', x=3.1, y=0.8, label='*', size=10, hjust='left') +
  annotate('text', x=0.6, y=8, label='(a)', size=5, hjust='left')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0))

#lnRR
lnRRPlot <- 
  ggplot(removals, aes(x=response_recategorized, y=lrr)) +
  #geom_violin() +
  geom_boxplot(width=0.5) +
  xlab('') +
  ylab('lnRR') +
  # scale_y_continuous(breaks=seq(-4,8,2)) +
  # coord_cartesian(ylim=c(-4,8), xlim=c(1,3)) +
  scale_x_discrete(labels=c('Community\n(45)', 'Competition\n(12)', 'Function\n(49)')) +
  annotate('text', x=0.6, y=8, label='(b)', size=5, hjust='left')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0))


grid.arrange(absLnRRPlot,lnRRPlot, ncol=1)

pushViewport(viewport(layout=grid.layout(2,1)))
print(absLnRRPlot, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(lnRRPlot, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
#export at 900x1400

#####doing means +/- 95%CI
means<-removals%>%
  filter(response_recategorized!="competition")%>%
  group_by(response_recategorized)%>%
  summarize(mlrr=mean(lrr), 
            malrr=mean(abs_lrr),
            n=length(lrr),
            sdlrr=sd(lrr),
            sdalrr=sd(abs_lrr),
            selrr=sdlrr/sqrt(n),
            sealrr=sdalrr/sqrt(n),
            ci_lrr=1.96*selrr,
            ci_alrr=1.96*sealrr)

Mean_absLnRRPlot <-
  ggplot(means, aes(x=response_recategorized, y=malrr, color=response_recategorized)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=malrr-ci_alrr, ymax=malrr+ci_alrr), width=0.2)+
  ylab('|lnRR|') +
  scale_x_discrete(labels=c('', '', '')) +
  annotate('text', x=1.1, y=.5, label='*', size=5, hjust='left') +
  annotate('text', x=2.1, y=.5, label='*', size=5, hjust='left') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0))+
  theme(legend.position = "none")+
  scale_color_manual(values=c("darkgreen","blue"))+
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.title.x= element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_blank())

#lnRR
Mean_lnRRPlot <- 
  ggplot(means, aes(x=response_recategorized, y=mlrr, color=response_recategorized)) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=mlrr-ci_lrr, ymax=mlrr+ci_lrr), width=0.2)+
  xlab('') +
  ylab('lnRR') +
  scale_color_manual(values=c("darkgreen","blue"))+
  scale_x_discrete(labels=c('Community\n(45)', 'Function\n(49)')) +
  annotate('text', x=2.1, y=-.25, label='*', size=10, hjust='left')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0))+
  theme(legend.position = "none")

grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.35, height = 0.3, x = 0.8, y = 0.82) #plot area for the inset map
print(Mean_lnRRPlot,vp=v1) 
print(Mean_absLnRRPlot,vp=v2)
