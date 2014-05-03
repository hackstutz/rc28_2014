setwd("C:/Users/Hackstutz/Dropbox/Git/rc28_2014/")

library(foreign)
communities <- read.dta("C:/Users/Hackstutz/Dropbox/PX-Daten/kontextvariablen0.02.dta")

library(dplyr)

communities <- communities %.% 
  filter(Jahr %in% c(1971,1981,1995,2001,2005,2008)) %.%
  mutate(employed=prim+sek+tert, #create some variables
         bev=frau_schw+frau_ausl+mann_schw+mann_ausl,
         tertiary=tert/employed,
         secondary=sek/employed,
         primary=prim/employed) %.%
  select(gini_steink, median_steink, Gemeindenr, Jahr, primary, secondary, tertiary,tert,sek,prim)

communities <- communities %.%
  group_by(Jahr) %.%
  mutate(rank_median=rank(median_steink),
         rank_gini=rank(gini_steink))

communities_wide <- reshape(communities, direction="wide",timevar="Jahr",idvar="Gemeindenr")

trim <- function(x) {
  low<-quantile(x,0.05,na.rm=TRUE)
  high<-quantile(x,0.95,na.rm=TRUE)
  ifelse(x>low, ifelse(x<high, x, high),low)
}

communities_wide <- communities_wide %.%
  mutate(diff_gini=trim(rank_gini.2008-rank_gini.1981),
         diff_median=trim(rank_median.2008-rank_median.1981),
         diff_primary=trim(primary.2008-primary.1995),
         diff_secondary=trim(secondary.2008-secondary.1995),
         diff_tertiary=trim(tertiary.2008-tertiary.1995)
  )



write.table(communities_wide,file="interactive/communities.tsv",row.names=FALSE,na="",sep="\t")


communities <- communities %.%
  group_by(Jahr) %.% # to have the correct reference when calculating rank of median/gini
  mutate(employed=prim+sek+tert, #create some variables
         tertiary=tert/employed,
         secondary=sek/employed,
         primary=prim/employed,
         rank_median=rank(median_steink),
         rank_gini=rank(gini_steink)) %.%
  select(rank_gini, rank_median, Gemeindenr, Jahr, primary, secondary, tertiary)
