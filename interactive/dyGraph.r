### Libraries ###
library(foreign)
library(ggplot2)
library(dplyr)
library(reshape2)
library(zoo)

source("https://gist.githubusercontent.com/hackstutz/2262223bc088ae503ba7/raw/dygraph")

# Setup community data
communities <- tbl_df(read.dta("C:/Users/Hackstutz/Dropbox/PX-Daten/kontextvariablen0.02.dta")) # adjust path
communities <- communities %.% 
  select(-Kantonnr, -Einheit, -gemj) %.% # Drop useless stuff 
  group_by(kj) %.%
  mutate(bev=mann_schw+mann_ausl+frau_schw+frau_ausl,  # Create additional variables
         ausl_anteil=(mann_ausl+frau_ausl)/bev, # share of foreigners in community
         maenner_anteil=(mann_schw+mann_ausl)/bev, # share of males
         tert_abschluss=bachelor+master+diplom+bachelor_fh+master_fh+diplom_fh+doktorat, # canton sum of tertiary educ. degrees
         bev_kanton=sum(bev,na.rm=TRUE), # canton sum of inhabitants
         tert_quote=tert_abschluss/bev_kanton, # share of tertiary educated
         KG=paste(Kantonname,Gemeindename) %.%
  ) 

w_communities <- tbl_df(dcast(communities %.% ungroup() %.% filter(bev>1000) %.% select(gini_steink,KG, Jahr), Jahr~KG,value.var="gini_steink",fun.aggregate = mean))

# Communities with too many missings
names(which(sapply(w_communities, function(x) sum(is.na(x)))>20))
#w_communities<-w_communities[,-which(sapply(w_communities, function(x) sum(is.na(x)))>20)]
z_communities <- zoo(w_communities)
index(z_communities) <- z_communities[,1]
z_communities <- na.approx(z_communities)
names(z_communities)[1]<-""
write.table(z_communities,file="data/communities2.csv",sep=",", row.names=FALSE)

# Create Graph
createDygraph(dy_data="data/communities2.csv",id="1000")


# Setup individual Obwalden data

# load obwalden tax data
load("Z:\\soz\\group-jann\\Projekte\\Ungleichheit\\Daten\\Obwalden\\obwalden_trimmed.RData")

# load isco_classification table, trim
isco_classes <- read.csv("C:/Users/Hackstutz/Dropbox/Ungleichheit/Obwalden/berufe_isco.csv")
isco_classes <-  isco_classes %.%
  filter(isco2>0, sektor>0) %.%
  mutate(isco1=as.numeric(substring(as.character(isco2),1,1))) %.%
  group_by(isco2) %.%
  mutate(summe=sum(count)) %.%
  filter(summe>100) #all have more than 100, thats fine

# match job field to isco class
d$beruf <- tolower(d$BERUF)
d$isco2 <- isco_classes$isco2[match(d$beruf,isco_classes$tolowerberuf)]
d$isco1 <- isco_classes$isco1[match(d$beruf,isco_classes$tolowerberuf)]
d$sector <- isco_classes$sektor[match(d$beruf,isco_classes$tolowerberuf)]

# calculate median and variance for the isco2 classes

#median_inc = ave(d$ESATZBESTKOPF,d$STEUERJAHR,d$isco2, FUN=median, na.rm = TRUE)

mini.d <- d %.% 
  filter(!is.na(isco2)) %.%
  select(STEUERJAHR,ESATZBESTKOPF,VSATZBESTKOPF,beruf,isco2,isco1,sector)

agg.d <- mini.d %.%
  group_by(STEUERJAHR,isco2) %.% 
  summarise(count = n(), 
            median_inc = median(ESATZBESTKOPF, na.rm = TRUE), 
            mean_inc = mean(ESATZBESTKOPF, na.rm = TRUE), 
            mean_wealth = mean(VSATZBESTKOPF, na.rm = TRUE), 
            sd_inc = var(ESATZBESTKOPF, na.rm = TRUE)^0.5, 
            lnstdinc = var(log(ESATZBESTKOPF+1), na.rm = TRUE)^0.5) %.% 
  arrange(isco2,STEUERJAHR)
agg.d$sector <- isco_classes$sektor[match(agg.d$isco2,isco_classes$isco2)]

# get english labels
label.d <- read.csv("C:/Users/Hackstutz/Dropbox/Ungleichheit/Obwalden/struct08.csv")
agg.d$label <- label.d$Title.EN[match(agg.d$isco2,label.d$ISCO.08.Code)]
agg.d$label <- gsub(",","",agg.d$label)

# test plot with ggplot
pdf("sektoren.pdf")
ggplot(agg.d, aes(x=STEUERJAHR, y=median_inc, colour=factor(isco2), group=isco2))+geom_line()+ggtitle("Median taxable income different sectors") + facet_wrap(~sector)+ opts(legend.position = "none") +ylab("Median taxable income")+xlab("Year")
dev.off()


# shape for dygraph and plot

max(agg.d$median_inc) #124700
min(agg.d$median_inc) #15700
valuerange <- "null,125000"

sector1 <- dcast(filter(agg.d,sector==1), STEUERJAHR~label,value.var="median_inc")
names(sector1)[1]<-""
write.table(sector1,file="interactive//sector1.csv", sep=",",row.names=FALSE)
createDygraph(dy_data="interactive/sector1.csv",id="sector1",title="primary sector",ylab="Medin txable income (CHF)",valueRange=valuerange,width=600)

sector2 <- dcast(filter(agg.d,sector==2), STEUERJAHR~label,value.var="median_inc")
names(sector2)[1]<-""
write.table(sector2,file="interactive//sector2.csv",sep=",", row.names=FALSE)
createDygraph(dy_data="interactive/sector2.csv",id="sector2",title="secondary sector",ylab="Medin txable income (CHF)",valueRange=valuerange,width=600)

sector3 <- dcast(filter(agg.d,sector==3), STEUERJAHR~label,value.var="median_inc")
names(sector3)[1]<-""
write.table(sector3,file="interactive//sector3.csv",sep=",", row.names=FALSE)
createDygraph(dy_data="interactive/sector3.csv",id="sector3",title="tertiary sector",ylab="Medin txable income (CHF)",valueRange=valuerange,width=600)

