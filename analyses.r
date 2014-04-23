### Libraries ###
library(foreign)
library(ggplot2)
library(dplyr)

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

### Descriptive analyses ###


### Regression analyses ###


### Exporting figures ###

#for(fig in c("fig1","fig2","fig3")) {
#  pdf(paste0("figure/",fig,".pdf"))
#  print(get(fig))
#  dev.off()
#}
