#this is for the pre and post pandemic data cleaning (S275)
library(readstata13)
#dat is from the past up until 2021
dat <- read.dta13("~/Downloads/INDEP/teacher_attrition_covid/data_S275_aesy_01_1995-96to2020-21prelim.dta")
library(tidyverse)

library(doMC)
registerDoMC(cores = 4)

#this is the new dat 22
dat22 <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/s275-23.csv")

colnames(dat) #65 ish column?
colnames(dat22) #52 column


length(Reduce(intersect, list(colnames(dat), colnames(dat22)))) #there are 52 columns with the same name. ok nice

#ok so we dont have one important variable which is schyear so I'll just create one
dat22$schyear <- 2022

#ok now there are 53 intersecting columns, nice. We shall merge them with left join, but before that, we will cut dat down to just year 2018
dat <- dat %>% filter(schyear >= 2018)
dat_post18 <- dat
#save file
write.csv(dat_post18, "~/Downloads/INDEP/teacher_attrition_covid/S275_post18")
#dat_post18 <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/S275_post18")

#ok now we can delete other columns from dat_post18
keepcols <- (Reduce(intersect, list(colnames(dat_post18), colnames(dat22))))
dat_post18 <- dat_post18[, keepcols] #great, now two dataframes have the same amount of columns


#first we will need to sort the column names so that two data frame will have the same order of column
new_order = sort(colnames(dat_post18))
dat_post18 <- dat_post18[, new_order] #ordered for dat_post18
dat22 <- dat22[, new_order] #ordered for dat_post18

#we need to take care of columns where the data are different types (eg double VS char)
#write a function that tells you what types of data there are

checkdiftype <- function(dat1, dat2) {
  check <- data.frame(
   colnames = "",
   Type_dat18 = "",
   Type_dat22 = ""
  
  )
  vec <- colnames(dat1)
  for (i in 1:length(vec)) {
   var = vec[i]
   check[i,1] = var
   check[i,2] = typeof(dat1[,var])
   check[i,3] = typeof(dat2[,var])
  }
  print(check)

}

check <- checkdiftype(dat_post18, dat22) #works. So lets look at how to take care of these types----
#variables with different types are


for(i in 1:nrow(check)) {
  if(check[i,2] != check[i,3]) {
    print(check[i,1])
  }
}


#these are variables that are not too important to my analysis right now, EXCEPT schyear, so I will take care of that.

dat22$schyear <- as.integer(dat22$schyear)

#run function again
check <- checkdiftype(dat_post18, dat22) #ok cool. 

#delete other variables (sorry for now ><)

for(i in 1:nrow(check)) {
  if(check[i,2] != check[i,3]) {
    del = check[i,1]
    dat_post18[,del] <- NULL
    dat22[,del] <- NULL
  }
}
  
#check if there are any different var
check <- checkdiftype(dat_post18, dat22) #works. So lets look at how to take care of these types----


for(i in 1:nrow(check)) {
  if(check[i,2] != check[i,3]) {
    print(check[i,1])
  }
}
#ok cool

#BIND DATA! call it dat again so i can use the previous cleaning codes---------
dat <- rbind(dat_post18, dat22)
table(dat$schyear)

#now this data is ready to be analyzed!
write.csv(dat, "~/Downloads/INDEP/teacher_attrition_covid/S275_2018_2022.csv")


