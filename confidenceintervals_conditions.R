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



#BOOTDATA is our data but as done immediately above, you need to remove the NA's from our
#treatment columns. 
#INGROUPS are the treatment groups you want to look at, 1st 4 rows in the returned data
#are mean and confidence intervals for these groups. Next 4 rows in returned data are
#mean/CI for all other groups that are not excluded. Last 4 returned rows are the mean/CIs
#for the difference between in groups and the other, not excluded groups.
#EXCLUDEDGROUPS are entered the same way as ingroups and are those that will be removed 
#from the ingroup/outgroup comparison. for example if we want to compare experts versus
#non experts then we would want to exclude group 9, the control group
#LISTCONDITIONS are variables and the variable values that we want to look. It requres list type data
#where the names of the various list(s) are the variables(columns) in our data and the values
#within the lists are the values want to select on. 
#so pass in list(column name = "responses you want included", second column name = "responses 
#for that variable that we are interested in", ect.) <-you can add more than one
#value in a column by separating with '|'
#BOOTNUM is the number of bootstrap samples to draw for creating confidence intervals

condinterval<-function(bootdata,ingroups,excludedgroups="zzz",listconditions=NA,bootnum){
  outcome<-rbind(rep(NA,12),rep(NA,12))
  bootdata<-bootdata[-which(grepl(paste0(paste0(excludedgroups,sep = "|",collapse = ""),"zzz",collapse = ""),bootdata$group)),]
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
condinterval(dataclean,c(1,2,3,4),excludedgroups = c(9),bootnum = 500)

###expert fact versus non-expert fact, removed groups 3, 4, 7, 8 and 9 so it compares
#groups 1 and 2 to groups 5 and 6
condinterval(dataclean,c(1,2),c(9,3,4,7,8),bootnum = 500)

###expert opinion versus non-expert opinion, removes groups 1,2,5,6 and 9 so it compares
#groups 3 and 4 to 7 and 8
condinterval(dataclean,c(3,4),c(1,2,5,6,9),bootnum = 500)


###########removing low attention subject (for example purposes only, we need 
#to consider low attention futher)

####expert versus non-expert
condinterval(dataclean,c(1,2,3,4),c(9),bootnum = 500,listconditions = list(lowattention = '0'))

###expert fact versus non-expert fact
condinterval(dataclean,c(1,2),c(3,4,7,8,9),bootnum = 500,listconditions = list(lowattention = '0'))

###expert opinion versus non-expert opinion
condinterval(dataclean,c(3,4),c(1,2,5,6,9),bootnum = 500,listconditions = list(lowattention = '0'))


