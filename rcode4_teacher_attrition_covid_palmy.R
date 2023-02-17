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

#-----------------------------------------#
#-----------clean!------------------------#
#functions
dupecol <- function(data) {
  data <- data %>% ungroup() %>% group_by(cert, schyear) %>% mutate(dupyear = case_when(
    length(schyear) > length(unique(schyear)) ~ 0, #yes dupes
    length(schyear) == length(unique(schyear)) ~ 1 #no dupes
  ))
  return(data)
}


anydupe <- function(data) {
  data <- data %>% group_by(cert, schyear) %>% mutate(dupyear = case_when(
    length(schyear) != length(unique(schyear)) ~ "Contains dupes", 
    length(schyear) == length(unique(schyear)) ~ "Do not contains dupes"
  ))
  return(unique(data$dupyear))
  #return(data)
  
}

narrow <- function(data) {
  data <- data %>% filter(dupyear == "Contains dupes")
  return(data)
}


#how many major assignment are there
table(dat$major) #this works just like tab in Stata
#so now we know that there are major assignment 0,1,2,3,4; major == 0 and 1 are the MOST common

#ok, so we will then make major > 0 == 1, so that major is binary.
dat <- dat %>% mutate(major = case_when(
  major > 0 ~ 1,
  TRUE ~ 0
))

table(dat$schyear)
length(unique(dat$cert))



#now major is binary # we will come back and deal with major a little later, since we are a bit uncertain of what it is based on the codebook.
#but basically if major > 0, it is now a 1, the rest is 0.

#a teacher is someone with droot 31,32, or 33 with at least 0.5 FTE (is it assfte OR certfte??)-------
#first, create a teacher variable based on department root
dat <- dat %>% mutate(teacher = case_when(
  droot == 31 ~ 1,
  droot == 32 ~ 1,
  droot == 33 ~ 1,
  TRUE ~ 0
))

#because we are interested in assfte, if assfte == NA or MISSING, then we should not include those rows:
sum(is.na(dat$assfte)) #there are some rows missing assfte, so we will remove them
dat <- dat %>% filter(!is.na(assfte))
#check if max function works, if it does, then we are left with numbers which is what we want
max(dat$assfte) #WORKS!

#we also need to track people using cert, so we will also remove rows with missing cert number
sum(!complete.cases(dat$cert)) #no NA - so there might be some other type of missing data
dat <- dat %>% filter(cert != "")

#code that can allow us to see the data structure better.
dat %>% group_by(schyear) %>% summarise(
  across(assfte, list(mean = mean, median = median))
) %>% print(n = length(unique(dat$schyear)))


#then create a variable that will track fte by teacher. 
#If teacher variable == 1 (the correct droot), this variable will be the sum of that person's assfte for that schyear
dat <- dat %>% group_by(cert, schyear, teacher) %>% mutate(teach_FTE = case_when(
  teacher == 1 ~ sum(assfte),
  teacher == 0 ~ 0
))

max(dat$teach_FTE) #someone literally has 6 full time jobs

#so I will create another variable that says "full time teacher" to go along with teacher. This will be 1 if the teacher_FTE is more than or equal to 500


dat <- dat %>% group_by(cert, schyear) %>% mutate(fulltime_teach = case_when(
  teach_FTE >= 0.5 ~ 1,
  TRUE ~ 0
))
table(dat$fulltime_teach)




#So in conclusion, teachers are those with variable teacher ==1 and fulltime_teach == 1

#we need one more variable to signify teacher as a final variable. I'll call this finteach
#we will now get the correct droot with more than half time employment
dat <- dat %>% ungroup() %>% mutate(finteach = case_when(
  teacher == 1 & fulltime_teach == 1 ~ 1,
  TRUE ~ 0
))

#----NOW we will start removing rows
#how about we remove secondary jobs that are non-teaching and teaching.

dat <- dat %>% group_by(cert, schyear, finteach) %>% mutate(max_assfte = max(assfte))

#now lets start a new dataset so we dont lose all the work. We will use "data"
#we have disected our operations into teaching VS non-teaching jobs. And we are now keeping the job per year that has the most fte from each category

data <- dat %>% filter(assfte == max_assfte)

anydupe(data) #there are still many dupes - so we will continue down the decision tree. Which is salary
data <- dupecol(data)


#once again, we will think in two categories - which is non-teaching jobs and teaching jobs. We will remove secondary jobs from each category
#variable tfinsal, cins, and cman represent total salary but we dont know if it includes insurance benefit? My guess is that it doesnt include insurance benefit. So I will add them up?

data <- data %>% group_by(cert, schyear, finteach) %>% mutate(max_tfinsal = max(tfinsal, na.rm = TRUE))

#create another dataset for safety. Now we will remove secondary salaries - i think we removed about 1,000 
data1 <- data %>% filter(max_tfinsal == tfinsal)

data1 <- dupecol(data1)
table(data1$dupyear)

#START the final cleaning process---------
#how many unique certs are there per schyear
dat <- dat %>% group_by(schyear) %>% mutate(how_many_unique_cert = length(unique(cert)))
table(dat$how_many_unique_cert) 

#here is the lay out of how many unique certs are there in 2019, 2020, and 2021. We can already see that it has decreased
dat %>% group_by(schyear) %>% summarise(
  across(how_many_unique_cert, list(mean = mean))
)

#see if building code is missing
dat %>% group_by(bldgn) %>% tally() #this is how we can do tab but vertically
#there is a "!!!!" as a building number, we shall exclude that
length(which(dat$bldgn == "")) #0
length(which(dat$bldgn == "!!!!")) #1

dat <- dat %>% filter(bldgn != "!!!!") #done

#see if district identification code is missing
dat %>% group_by(dis) %>% tally()
sum(!complete.cases(dat$dis)) #0
length(which(dat$dis == "")) #0
#So i think district identification code is NOT missing

#check if major shows up once within each person
dat <- dat %>% group_by(schyear, cert) %>% mutate(total_maj = sum(major))
table(dat$total_maj) 

#keep if major == 1
#dat <- dat %>% ungroup() %>% filter(major == 1) #maybe not for now, since major is not the best indicator?


dat <- dupecol(dat)
anydupe(dat)
table(dat$dupyear) #now we have just 1210 rows that contains duplicates
dat <- dat[with(dat, order(cert, schyear)),] #order

#we will keep someone who has been a teacher before #across the year, has anyone received a 1 for finteach (our final variable for teacher)
dat <- dat %>% ungroup() %>% group_by(cert) %>% mutate(ever_teach = max(finteach))
table(dat$ever_teach) 

findat <- dat %>% filter(ever_teach == 1)
findat <- dupecol(findat19)
table(findat$dupyear) 


#num_obs_yr is within cert and year, tell me the number of rows that that person show up
findat <- findat %>% group_by(schyear, cert) %>% mutate(num_obs_yr = n()) #little n = count == big N in Stata

#obs_cnt_yr  - this is an index variable
findat <- findat[with(findat, order(cert, schyear, assfte, tfinsal)),]
findat <- findat %>% group_by(schyear, cert) %>% mutate(obs_cnt_yr = row_number())

length(which(findat$num_obs_yr != findat$obs_cnt_yr)) 

#then keep ones that obs_cnt_yr == numn_obs_yr
findat <- findat %>% filter(num_obs_yr == obs_cnt_yr)

findat <- dupecol(findat)
table(findat$dupyear) #no more dupes! 

#---ALL DONE
write.csv( findat, "~/Downloads/INDEP/teacher_attrition_covid/final_s275-18-23.csv")
