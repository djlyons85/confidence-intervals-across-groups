rm(list=ls())
library(XML)
setwd("c:/Users/djl/Documents/summer project")
library(rio)
library(data.table)
library(stringr)
library(xlsx)
library(matrixStats)
dataclean<-import("data_clean.xlsx")
dataclean<-dataclean[-which(grepl('NA',dataclean$treatment1)),]
dataclean<-dataclean[-which(grepl('NA',dataclean$treatment2)),]
dataclean<-dataclean[-which(grepl('NA',dataclean$treatment3)),]
dataclean<-dataclean[-which(grepl('NA',dataclean$treatment4)),]


#list = column name and conditions


condinterval<-function(bootdata,ingroups,listconditions=NA,bootnum){
  outcome<-rbind(rep(NA,12),rep(NA,12))
  for(i in 1:bootnum){
  temp<-bootdata[sample(1:nrow(bootdata),nrow(bootdata),replace = TRUE),]
  if(!is.na(listconditions)){
    for(j in length(listconditions)){
      k<-which(colnames(temp)== names(listconditions)[j])
      temp<-temp[grep(listconditions[j],as.character(temp[,k])),]
    }
  }
  ingrouptemp<-temp[which(grepl(paste0(paste0(ingroups,sep = "|",collapse = ""),"zzz",collapse = ""),temp$group)),22:25]
  outgrouptemp<-temp[-which(grepl(paste0(paste0(ingroups,sep = "|",collapse = ""),"zzz",collapse = ""),temp$group)),22:25]
  outcome<-rbind(outcome,c(colMeans(data.matrix(ingrouptemp)),colMeans(data.matrix(outgrouptemp)),colMeans(data.matrix(ingrouptemp))-colMeans(data.matrix(outgrouptemp))))
  }

returnvalues<-as.data.frame(cbind(colMeans(na.omit(outcome)),colQuantiles(na.omit(outcome),probs = c(.025,.975))))
names(returnvalues)[1]<-"means"
return(returnvalues)
}


####examples########################


####expert versus non-expert
condinterval(dataclean[-which(grepl('9',dataclean$group)),],c(1,2,3,4),bootnum = 500)

###expert fact versus non-expert fact
condinterval(dataclean[-which(grepl('9|3|4|7|8',dataclean$group)),],c(1,2),bootnum = 500)

###expert opinion versus non-expert opinion
condinterval(dataclean[-which(grepl('9|1|2|5|6',dataclean$group)),],c(3,4),bootnum = 500)


###########removing low attention subject (for example purposes only)

####expert versus non-expert
condinterval(dataclean[-which(grepl('9',dataclean$group)),],c(1,2,3,4),bootnum = 500,listconditions = list(lowattention = '0'))

###expert fact versus non-expert fact
condinterval(dataclean[-which(grepl('9|3|4|7|8',dataclean$group)),],c(1,2),bootnum = 500,listconditions = list(lowattention = '0'))

###expert opinion versus non-expert opinion
condinterval(dataclean[-which(grepl('9|1|2|5|6',dataclean$group)),],c(3,4),bootnum = 500,listconditions = list(lowattention = '0'))


