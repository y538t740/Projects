setwd("C:/Users/tanyi/OneDrive/Desktop")
#Libraries:
library(zoo)
library(dplyr) #ranking frequency
library(data.table)  #R's dataframe
library(reshape2)
library(grDevices) #save as png
library(wordcloud)
library(RColorBrewer) #provide colors

#read and reshape data table
chi_dat = read.csv("dp02ks.csv")
chi_dat = as.data.table(chi_dat)
chi_pop = chi_dat[Description == "Population in households"]
x = colnames(chi_pop)
t = melt(chi_pop,measure.vars=x,variable.name="States",value.name="Population",na.rm=TRUE)
Pop = as.data.table(t[4:213,])
n = Pop[1,Population]

#Use gsub to get rid of commas
#Use order to order the data table
Pop$Population<-as.numeric(gsub(",","",Pop[,Population]))
Pop$States<-as.character(gsub(".County.Estimate","",Pop[,States]))
Pop.des = Pop[order(-Population)]
Pop.final = Pop.des[which(index(Pop)%%2==1)]

#Workcloud
png("workcloud4.png", width=12,height=8,units="in",res=300)
par(mar=rep(0,4))
set.seed(1337)
wordcloud(words = Pop.final$States, freq=Pop.final$Population, scale=c(3.5,0.25),max.words=105,colors=brewer.pal(8,"Dark2"))
