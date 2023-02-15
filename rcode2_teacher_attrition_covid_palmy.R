library(readstata13)
dat <- read.dta13("path to file.dta")
library(tidyverse)


#maximize the cores for faster processing since this is a large dataset
#install.packages("doMC")
library(doMC)
registerDoMC(cores = 4)

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


#load the data - use the stata13 command because the file is stata version - 11 million rows
dat <- read.dta13("~/Downloads/INDEP/teacher_attrition_covid/data_S275_aesy_01_1995-96to2020-21prelim.dta")

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
sum(is.na(dat$assfte)) #there are 160 rows missing assfte, so we will remove them
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

max(dat$teach_FTE) #how can this result in 2000? HMMM Someone is doubling a job? Two teachers jobs within the same academic year?

#For those that have teach_FTE more than 1000, we will have to remove their second teacher job. But first we need to remove some rows

#let's subset a dataset that has people working TWO full-time teaching jobs

subset1 <- dat %>% filter(teach_FTE > 1000)
subset1 <- arrange(subset1, cert)
subset1 <- subset1 %>% select(lname, fname, cert, schyear, teacher, assfte, teach_FTE,droot)

#OK there are def hard working people. 
#so I will create another variable that says "full time teacher" to go along with teacher. This will be 1 if the teacher_FTE is more than or equal to 500


dat <- dat %>% group_by(cert, schyear) %>% mutate(fulltime_teach = case_when(
  schyear < 2002 & teach_FTE >= 500 ~ 1,
  schyear >= 2002 & teach_FTE >= 0.5 ~ 1,
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

data <- dat %>% filter(assfte == max_assfte) # we are now down to 3.9 million rows

anydupe(data) #there are still many dupes - so we will continue down the decision tree. Which is salary
data <- dupecol(data)

#how many teachers do we have now in each year
#length(unique(data %>% ungroup() %>% select(cert) %>% filter(finteach == 1, schyear == 2004)))


#once again, we will think in two categories - which is non-teaching jobs and teaching jobs. We will remove secondary jobs from each category
#variable tfinsal, cins, and cman represent total salary but we dont know if it includes insurance benefit? My guess is that it doesnt include insurance benefit. So I will add them up?

data <- data %>% group_by(cert, schyear, finteach) %>% mutate(max_tfinsal = max(tfinsal, na.rm = TRUE))

#create another dataset for safety. Now we will remove secondary salaries - i think we removed about 1,000 
data1 <- data %>% filter(max_tfinsal == tfinsal)

data1 <- dupecol(data1)
table(data1$dupyear) #there are about 543202 that are duplicates - each person here have two jobs per year - one is teaching and one is non-teaching

#let's arrange data 
#data1 <- data1[with(data1, order(cert, schyear)),]



#subset data for quick view
temp <- data1 %>% select(dupyear, lname, fname, cert, schyear, assfte, teacher, teach_FTE, droot, fulltime_teach, finteach, max_assfte,tfinsal, max_tfinsal)
temp <- temp[30000:800000,]
temp <- temp[with(temp, order(cert, schyear)),]
table(temp$assfte)






