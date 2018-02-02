library (dplyr)
library (tidyr)
library(ggplot2)
library(ggthemes)

groups <- c("ARB","CSS","CEB", "EAP","EAR","ECA","ECS","EMU","EUU","FCS","HIC","HPC","IBD","IBT","IDA","IDB","IDX","LAC","LCN","LDC","LIC","LMC","LMY","LTE","MEA","MIC","MNA","OED",
  "OSS","PRE","PSS","PST","SSA","SST","SSF","TEA","TEC","TLA","TMN","TSA","TSS","UMC","WLD")

names(forestdata)[2] <- "code"
names(area)[2] <- "code"

forestdata$code <- as.factor(forestdata$code)

nogroups <- forestdata[!(forestdata$code %in% groups),]
nogrouparea <- area[!(area$code %in% groups),]

hist(nogroups$`2013`/100000,xlim=c(0,20),breaks=40)

hist(nogroups$`1990`/100000,xlim=c(0,20),breaks=40)

yearavg <- as.data.frame(colMeans(nogroups[,5:30],na.rm=TRUE))

shortyears <- nogrouparea[,c(1,2,35:60)]

#find forest loss
cover <- nogroups
cover$diff1990 <- cover$`1991` - cover$`1990`
cover$diff1995 <- cover$`1996` - cover$`1995`
cover$diff2000 <- cover$`2001` - cover$`2000`
cover$diff2005 <- cover$`2006` - cover$`2005`
cover$diff2010 <- cover$`2011` - cover$`2010`
cover$diff2013 <- cover$`2014` - cover$`2013`

colMeans(cover[,c(33,34)],na.rm=TRUE)

hist(diff1990/1000,xlim=c(-10,20))
hist(diff2013/1000)

#plotting

usonly <- cover[cover$code=="USA",]
toplot <- gather(cover,"year","forest",5:32)

usplot <- gather(usonly,"year","forest",5:32)
usplot <- usplot[-c(27,28),]
usplot$forest <- as.integer(usplot$forest)
usplot$year <- as.integer(usplot$year)

ggplot(usplot,aes(x=year,y=(forest/1000),group=1)) + theme_hc() + scale_colour_hc() + 
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015),label=c(1990,1995,2000,2005,2010,2015)) +
  geom_line() + geom_point() + xlab("Year") + ylab("Forest cover (in thousands of sq. km)")

