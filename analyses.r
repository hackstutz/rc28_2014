### Libraries ###
library(foreign)
library(ggplot2)
library(dplyr)
library(plm)
library(sjPlot)
library(texreg)

### Data ###
communities <- tbl_df(read.dta("C:/Users/Hackstutz/Dropbox/PX-Daten/kontextvariablen0.02.dta")) # adjust path

communities <- communities %.% 
  select(-Kantonnr, -Einheit, -gemj) %.% # Drop useless stuff 
  group_by(kj) %.%
  mutate(bev=mann_schw+mann_ausl+frau_schw+frau_ausl,  # Create additional variables
         ausl_anteil=(mann_ausl+frau_ausl)/bev, # share of foreigners in community
         maenner_anteil=(mann_schw+mann_ausl)/bev, # share of males
         tert_abschluss=bachelor+master+diplom+bachelor_fh+master_fh+diplom_fh+doktorat, # canton sum of tertiary educ. degrees
         bev_kanton=sum(bev,na.rm=TRUE), # canton sum of inhabitants
         tert_quote=tert_abschluss/bev_kanton # share of tertiary educated
         ) 

communities <- communities %.%
  ungroup() %.%
  group_by(Gemeindenr) %.%
  mutate(lag_gini_steink=lag(gini_steink))


### Regression analyses ###

fit <- plm(gini_steink~lag_gini_steink+(log(prim+0.1)+log(sek+0.1)+log(tert+0.1))*I(Jahr-1995)+log(stpf), data=communities,index="Gemeindenr")
summary(fit)
mytable <- capture.output(htmlreg(list(fit),
        custom.coef.names=c("Lagged Y", "log of full-time jobs in primary sector (1995)", "log of full-time jobs in secondary sector (1995)","log of full-time jobs in tertiary sector (1995)","linear time trend","log of taxable inhabitants","Interaction: primary X linear time trend","Interaction: secondary X linear time trend","Interaction: tertiary X linear time trend"),
        digits=4,
        caption="",
        leading.zero=FALSE,
        star.symbol="\\*"
        ))

# escape stars so markdown in gitbook can deal with it
cat(gsub("\\$","",mytable))


### Individual Canton data

load("Z:\soz\group-jann\Projekte\Ungleichheit\Daten\Obwalden\obwalden_trimmed.RData"))
isco <- read.csv("C:/Users/Hackstutz/Dropbox/Ungleichheit/Obwalden/berufe_isco.csv")
isco$isco2[isco$isco2<0]<-NA
isco$sektor[isco$sektor<1]<-NA
isco$sektor[isco$sektor>3]<-NA
d$beruf <- tolower(d$BERUF)
d$isco2 <- isco$isco2[match(d$beruf,isco$beruf)]
d$isco1 <- as.numeric(substr(as.character(d$isco2),1,1))
d$persid_jahr <- paste(d$PERSID,d$STEUERJAHR)
d <- d[-which(duplicated(d$persid_jahr)),]

fit <- lm(log(ESATZBESTKOPF+1)~SEX+factor(isco2)*I(STEUERJAHR-2001), data=d)
summary(fit)  

fit <- lm(log(ESATZBESTKOPF+1)~SEX+factor(sector)*I(STEUERJAHR-2001), data=d)
summary(fit)  
