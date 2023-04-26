#PREP THE DATA 
library(tidyverse)

missing <- function(text) {
  var <- as.character(text)
  length(which(is.na(findat[[var]])))
}
empty <- function(text) {
  var <- as.character(text)
  length(which(findat[[var]] == ""))
}



#we now have data for 2018 - 2023. We will need to classify them
findat <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/with_mobility_s275-18-23.csv")

#filter out the 2023 schyear because we do not use that data anymore. we only use it to calculate attrition

findat <- findat %>% filter(schyear != 2023)


#we have to decide on which variables we are going to choose
#according to literature, we will include:

#dis - district
table(findat$dis)
missing("dis")

#hispanic
table(findat$hispanic) #55 rows empty 

#exp - certified years of experience
empty("exp")
missing("exp") #183 missing

#tfinsal - total salary
table(findat$tfinsal)
missing("tfinsal") #2 missing
empty("tfinsal")

#cis - annual insurance benefits
missing("cins") #missing 7
empty("cins")

#sex - gender
missing("sex")
empty("sex") #9 empty
table(findat$sex) #3 types
#gender is important so we will also delete ones with no gender identity


#hdeg - higher ed degree #too much missingness might not be able to use
table(findat$hdeg) #m = missing?, there is 1 zero, which we shall remove
empty("hdeg")

#race - staff race - we're gonna need to recode this
table(findat$race)
empty("race") #476 empty
missing("race")

#codist - school district
table(findat$codist)
empty("codist")
missing("codist")

#county
length(unique(findat$cou))
missing("cou")
empty("cou")


#now we will supplement data on county
#https://ofm.wa.gov/pop/geographic/codes/geographic_codes.xlsx
geo <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/geographic_codes.csv")



#join data to create a new data variable
newdat <- findat %>% left_join(geo, c("cou" = "COUNTYN"))


#school district table
#https://eds.ospi.k12.wa.us/DirectoryEDS.aspx
dis <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/dis_directory.csv")

#join data again
newdat <- newdat %>% left_join(dis, c("codist" = "DistrictCode"))

newdat$COUNTY_NAME <- tolower(newdat$COUNTY_NAME)

#now we are ready to join some demographic data e.g., crime rate
#I added the perc white variable - which is the percentage of white people

#house hold income https://ofm.wa.gov/washington-data-research/economy-and-labor-force/median-household-income-estimates
#year 2017 is equal to schyear 2018
medinc <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/medinc.csv")
medinc$county <- tolower(medinc$county)

newdat <- newdat %>% left_join(medinc, c("COUNTY_NAME" = "county", "schyear" = "schyear"))

#https://wcrer.be.uw.edu/archived-reports/
house <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/medhousing.csv", header = TRUE)

house$County <- tolower(house$County)
#the years are already aligned because we use Q4 data, so year 2018


newdat <- newdat %>% left_join(house, c("COUNTY_NAME" = "County", "schyear" = "schyear"))
str(newdat$medhouse)


#we also need unemployment rate based on the year, in each county 
#https://fred.stlouisfed.org/release/tables?rid=116&eid=256525&od=2018-01-01#
unemp <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/unemp_county.csv", header = TRUE)
unemp <- unemp %>% select(-per)
unemp$county <- tolower(unemp$county)


newdat <- newdat %>% left_join(unemp, c("COUNTY_NAME" = "county", "schyear" = "schyear"))

#we also will put crime rate per county
#https://data.wa.gov/Public-Safety/Washington-State-Uniform-Crime-Reporting-Summary-R/6njs-53y5
table(newdat$schyear)
crime <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/crime2.csv", header = TRUE)
crime <- crime %>% filter(INDEXYEAR >= 2017 & LOCATION == "COUNTY TOTAL") 
#we need to match it to the previous year
crime$INDEXYEAR <- crime$INDEXYEAR + 1 #the crime rate in 2017 will influence attrition rate in schyear 2018, so we plus 1

crime$COUNTY <- tolower(crime$COUNTY)

#convert everything into rate
crime[,7:34] <- crime[,7:34]/crime[,4]
crime$RATE <- crime$RATE/100

newdat <- newdat %>% left_join(crime, c("COUNTY_NAME" = "COUNTY", "schyear" = "INDEXYEAR"))
#table(newdat$mobility)


#electric_vehicle
elec <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/electric_vehicle.csv", header = TRUE)
elec <- elec[,c(3,17:18)]
#alignment of year, transaction year 2017 = schyear 2018
colnames(elec) <- c("vehicleID", "transaction_year", "county")
elec$transaction_year <- as.integer(elec$transaction_year)
elec$schyear <- elec$transaction_year + 1
elec$county <- tolower(elec$county)
table(elec$schyear)

newdat <- newdat %>% left_join(elec, c("COUNTY_NAME" = "county", "schyear" = "schyear"))

#JOIN report card data
#https://data.wa.gov/education/Report-Card-Enrollment-2018-19-School-Year/u4gd-6wxx
repc19 <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/repc19.csv", header = TRUE)
repc19 <- repc19 %>% filter(OrganizationLevel == "School", GradeLevel == "AllGrades")
repc19$SchoolYear <- 2019
repc19 <- repc19[,c(1,9,10,14:33)]

repc21 <- read.csv("~/Downloads/INDEP/teacher_attrition_covid/repc21.csv", header = TRUE)
repc21 <- repc21 %>% filter(OrganizationLevel == "School", GradeLevel == "AllGrades")
repc21$SchoolYear <- 2021
repc21 <- repc21[,c(1,9,10,14:33)]

repc <- rbind(repc19, repc21)
colnames(repc) <- c("SchoolYear","SchoolCode","SchoolName", "allstu", "femstu", "genXstu", "malstu", "indstu", "Astu", "bstu", "hispstu", 
                     "pcstu", "mulstu", "whitestu", "ellstu", "giftedstu", "homeless", "lowincstu", 
                     "migstu", "milpar", "mobstu","discard", "disstu")



newdat <- newdat %>% left_join(repc, c("schyear" = "SchoolYear", "bldgn" = "SchoolCode"))
newdat$discard <- NULL

#now we are going to take care of race as a variable. We can see that race is basically collected using maximum representation method
table(newdat$race) #I am going to exclude those with no race data, because this is an important part
newdat <- newdat %>% filter(race != "")
table(newdat$race)

#IN-OUT-MIGRATION BY COUNTY?
#https://www.census.gov/data/tables/2019/demo/geographic-mobility/county-to-county-migration-2015-2019.html

#per pupil expenditures


#we will create these binary variables: A, I, B, P, W


#install.packages("stringr")
library("stringr")  

newdat <- newdat %>% mutate(raceA = case_when(
  str_detect(race, 'A') == TRUE ~ 1,
  TRUE ~ 0
)) %>% mutate(raceI = case_when(
  str_detect(race, 'I') == TRUE ~ 1,
  TRUE ~ 0
)) %>% mutate(raceB = case_when(
  str_detect(race, 'B') == TRUE ~ 1,
  TRUE ~ 0
)) %>% mutate(raceP = case_when(
  str_detect(race, 'P') == TRUE ~ 1,
  TRUE ~ 0
)) %>% mutate(raceW = case_when(
  str_detect(race, 'W') == TRUE ~ 1,
  TRUE ~ 0
))

#delete ones with no gender identity
#newdat <- newdat %>% filter(sex != "")
#recode sex - we have to make sure that there is no character
#newdat <- newdat %>% mutate(sex = case_when(
#  str_detect(sex, 'F') == TRUE ~ 1,
#  str_detect(sex, 'M') == TRUE ~ 2,
#  TRUE ~ 0,
#))
newdat$sex <- as.factor(newdat$sex)



#we also need to recode hispanic
newdat <- newdat %>% filter(hispanic != "")
newdat <- newdat %>% mutate(hispanic = case_when(
  str_detect(hispanic, 'Y') == TRUE ~ 1,
  TRUE ~ 0
))
newdat$hispanic <- as.integer(newdat$hispanic) #be careful this is just a quick fix
table(newdat$hispanic)

#now we need to know which are factors
#newdat$DistrictName <- as.factor(newdat$DistrictName)
#newdat$COUNTY_NAME <- as.factor(newdat$COUNTY_NAME)
newdat$sex <- as.factor(newdat$sex)
newdat$raceA <- as.integer(newdat$raceA)
newdat$raceB <- as.integer(newdat$raceB)
newdat$raceI <- as.integer(newdat$raceI)
newdat$raceP <- as.integer(newdat$raceP)
newdat$raceW <- as.integer(newdat$raceW)


#transform all the labels into appropriate format
#newdat$intmobility <- as.integer(newdat$mobility)
#table(newdat$intmobility)

#now dont forget to take care of the experiences, make it quad
newdat$quadexp <- newdat$exp^2

#create a district mean salary variable
newdat <- newdat %>% group_by(schyear, dis) %>% mutate(tfinsal_dis = sum(tfinsal)/n())

newdat <- newdat %>% group_by(schyear, dis) %>% mutate(tfinsal_centered = scale(tfinsal, center = TRUE, scale = FALSE))

table(newdat$ESDName.x)
table(newdat$left_dataset)
#start with the newdat18

newdat18 <- newdat %>% filter(schyear == 2018)
table(newdat18$intmobility) # we got only 0 and 1

newdat19 <- newdat %>% filter(schyear == 2019)
table(newdat19$intmobility) #we have all 4 here

newdat20 <- newdat %>% filter(schyear == 2020)
table(newdat20$intmobility)

newdat21 <- newdat %>% filter(schyear == 2021)
table(newdat21$intmobility)

write.csv(newdat, "~/Downloads/INDEP/teacher_attrition_covid/dat_econ_of_ed.csv")


#WE ARE NOW READY TO DO THE MODEL#
#install.packages("xgboost")
library(xgboost)
#install.packages("caTools")
library(caTools)
#install.packages("cvms")
library(cvms)
#install.packages("caret")
library(caret)
install.packages("fastDummies")
library(fastDummies)
library(Matrix)
install.packages("shapr")
library(shapr)



var <- c("sex", "cins", "tfinsal","tfinsal_centered", "quadexp", "exp", "hispanic", "medhouse", "perc_white",
"raceA", "raceB", "raceI", "raceP", "raceW", "avqwint", "avqspring", "avqsummer", "avqfall") #no crime rate

var <- c("tfinsal_centered", "medhouse19", "perc_white",
          "avqwint", "avqspring", "avqsummer", "avqfall", "allstu", "femstu", "genXstu", "malstu", "indstu", "Astu", "bstu", "hispstu", 
           "pcstu", "mulstu", "whitestu", "ellstu", "giftedstu", "homeless", "lowincstu", 
          "migstu", "milpar", "mobstu", "disstu") 


#first we use catools to split the data into train and test
sample_split <- caTools::sample.split(Y = newdat19$intmobility, SplitRatio = 0.7)
train_set <- subset(x = newdat19, sample_split == TRUE)
test_set <- subset(x = newdat19, sample_split == FALSE)

y_train <- as.integer(train_set$intmobility) 
y_test <- as.integer(test_set$intmobility) 
x_train <- train_set[,var]
x_test <- test_set[,var]



#we need to dummy everything for xgboost to work
#sparse_matrix_train <- Matrix::sparse.model.matrix(y_train ~ ., data = traincomp)[,-1]
#X_train_dmat = xgb.DMatrix(sparse_matrix_train, label = y_train)




x_train <- fastDummies::dummy_cols(x_train)
x_train <- x_train %>% select(-sex)

x_test <- fastDummies::dummy_cols(x_test)
x_test <- x_test %>% select(-sex)

table(y_train)
str(x_train)

#we have to use DMatrix because this is how XGBoost store data. It is a specific data structure
#used to store data in a way optimized for memory efficiency and training speed.


xgb_train <- xgboost::xgb.DMatrix(data = as.matrix(x_train), label = y_train)
xgb_test <- xgboost::xgb.DMatrix(data = as.matrix(x_test), label = y_test)

#we will go with the most basic parameter values
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = 4
)



#RUN THE MODEL
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100, #let's start with this and hope that we land on something
  verbose = 1
)

xgb_model

importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = xgb_model
)
importance_matrix

#plot the importance matrix
xgb.plot.importance(importance_matrix) #------



#predictions - predictors are returned in a form of probabilities
xgb_preds <- predict(xgb_model, as.matrix(x_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(as.factor(newdat19$intmobility))



#see how well it predicts
xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(as.factor(newdat19$intmobility))[y_test + 1]
xgb_preds

#calculate over-all accuracy score

accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy #73% accuracy - needs more round

#confusion matrix
confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

#plot confusion matrix
cm <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
cfm <- as_tibble(cm$table)
cvms::plot_confusion_matrix(cfm, target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

install.packages("lme4")
install.packages("optimx")
install.packages("nnet")
library(nnet)
library(optimx)
library(lme4)
#run a regression model nnet::multinom
mod19 <- glm(as.factor(mobility) ~ tfinsal + tfinsal_centered + cins + quadexp + 
               medhouse19 + MED_INC_2018 + MED_INC_2019 + perc_white + as.factor(sex) +
               hispanic + raceW + raceI + raceB + raceA + raceP, data = newdat19, family = "binomial")
summary(mod19)

mod19 <- nnet::multinom(as.factor(mobility) ~ tfinsal + tfinsal_centered + cins + quadexp + 
               medhouse19 + MED_INC_2018 + MED_INC_2019 + perc_white + as.factor(sex) +
               hispanic + raceW + raceI + raceB + raceA + raceP, data = newdat19)

mod19_r <- glmer(as.factor(mobility) ~ tfinsal + tfinsal_centered + cins + quadexp + 
               medhouse19 + MED_INC_2018 + MED_INC_2019 + perc_white + as.factor(sex) +
               hispanic + raceW + raceI + raceB + raceA + raceP + (1|cert), data = newdat18, 
               family = "binomial", control = glmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))


####------------ post-pandemic data------#####
var21 <- c("sex", "cins", "tfinsal","tfinsal_centered", "quadexp", "exp", "hispanic", "medhouse19", "perc_white",
         "raceA", "raceB", "raceI", "raceP", "raceW", "avqwint", "avqspring", "avqsummer", "avqfall") #no crime rate



#first we use catools to split the data into train and test
sample_split21 <- caTools::sample.split(Y = newdat21$intmobility, SplitRatio = 0.7)
train_set21 <- subset(x = newdat21, sample_split21 == TRUE)
test_set21 <- subset(x = newdat21, sample_split21 == FALSE)

y_train21 <- as.integer(train_set21$intmobility) 
y_test21 <- as.integer(test_set21$intmobility) 
x_train21 <- train_set21[,var21]
x_test21 <- test_set21[,var21]



#we need to dummy everything for xgboost to work
#sparse_matrix_train <- Matrix::sparse.model.matrix(y_train ~ ., data = traincomp)[,-1]
#X_train_dmat = xgb.DMatrix(sparse_matrix_train, label = y_train)




x_train21 <- fastDummies::dummy_cols(x_train21)
x_train21 <- x_train21 %>% select(-sex)

x_test21 <- fastDummies::dummy_cols(x_test21)
x_test21 <- x_test21 %>% select(-sex)

str(x_train21)

#we have to use DMatrix because this is how XGBoost store data. It is a specific data structure
#used to store data in a way optimized for memory efficiency and training speed.


xgb_train21 <- xgboost::xgb.DMatrix(data = as.matrix(x_train21), label = y_train21)
xgb_test21 <- xgboost::xgb.DMatrix(data = as.matrix(x_test21), label = y_test21)

#we will go with the most basic parameter values
xgb_params21 <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = 4
)



#RUN THE MODEL
xgb_model21 <- xgb.train(
  params = xgb_params21,
  data = xgb_train21,
  nrounds = 8000, #let's start with this and hope that we land on something
  verbose = 1
)

xgb_model21

importance_matrix21 <- xgb.importance(
  feature_names = colnames(xgb_train21), 
  model = xgb_model21
)
importance_matrix21

#plot the importance matrix
xgb.plot.importance(importance_matrix21) #-----



#predictions - predictors are returned in a form of probabilities
xgb_preds21 <- predict(xgb_model21, as.matrix(x_test21), reshape = TRUE)
xgb_preds21 <- as.data.frame(xgb_preds21)
colnames(xgb_preds21) <- levels(as.factor(newdat21$intmobility))



#see how well it predicts
xgb_preds21$PredictedClass <- apply(xgb_preds21, 1, function(y) colnames(xgb_preds21)[which.max(y)])
xgb_preds21$ActualClass <- levels(as.factor(newdat21$intmobility))[y_test21 + 1]


#calculate over-all accuracy score

accuracy21 <- sum(xgb_preds21$PredictedClass == xgb_preds21$ActualClass) / nrow(xgb_preds21)
accuracy21 #73% accuracy - needs more round

#confusion matrix
confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

#plot confusion matrix
cm <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
cfm <- as_tibble(cm$table)
cvms::plot_confusion_matrix(cfm, target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

#-------test--- with all data but only for asian----#

vara <- c("sex", "cins", "tfinsal","tfinsal_centered", "quadexp", "exp", "hispanic", "medhouse19", "perc_white",
         "avqwint", "avqspring", "avqsummer", "avqfall") #no crime rate

newdata <- newdat %>% filter(raceA == 1)

#first we use catools to split the data into train and test
sample_splita <- caTools::sample.split(Y = newdata$intmobility, SplitRatio = 0.7)
train_seta <- subset(x = newdata, sample_splita == TRUE)
test_seta <- subset(x = newdata, sample_splita == FALSE)

y_traina <- as.integer(train_seta$intmobility) 
y_testa <- as.integer(test_seta$intmobility) 
x_traina <- train_seta[,vara]
x_testa <- test_seta[,vara]



#we need to dummy everything for xgboost to work
#sparse_matrix_train <- Matrix::sparse.model.matrix(y_train ~ ., data = traincomp)[,-1]
#X_train_dmat = xgb.DMatrix(sparse_matrix_train, label = y_train)




x_traina <- fastDummies::dummy_cols(x_traina)
x_traina <- x_traina %>% select(-sex)

x_testa <- fastDummies::dummy_cols(x_testa)
x_testa <- x_testa %>% select(-sex)



#we have to use DMatrix because this is how XGBoost store data. It is a specific data structure
#used to store data in a way optimized for memory efficiency and training speed.


xgb_traina <- xgboost::xgb.DMatrix(data = as.matrix(x_traina), label = y_traina)
xgb_testa <- xgboost::xgb.DMatrix(data = as.matrix(x_testa), label = y_testa)

#we will go with the most basic parameter values
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = 4
)



#RUN THE MODEL
xgb_modela <- xgb.train(
  params = xgb_params,
  data = xgb_traina,
  nrounds = 100, #let's start with this and hope that we land on something
  verbose = 1
)

xgb_modela

importance_matrixa <- xgb.importance(
  feature_names = colnames(xgb_traina), 
  model = xgb_modela
)
importance_matrixa

#plot the importance matrix
xgb.plot.importance(importance_matrixa, measure = "Gain")
?xgb.plot.importance


#predictions - predictors are returned in a form of probabilities
xgb_predsa <- predict(xgb_modela, as.matrix(x_testa), reshape = TRUE)
xgb_predsa <- as.data.frame(xgb_predsa)
colnames(xgb_predsa) <- levels(as.factor(newdata$intmobility))



#see how well it predicts
xgb_predsa$PredictedClass <- apply(xgb_predsa, 1, function(y) colnames(xgb_predsa)[which.max(y)])
xgb_predsa$ActualClass <- levels(as.factor(newdata$intmobility))[y_testa + 1]
xgb_predsa

#calculate over-all accuracy score

accuracya <- sum(xgb_predsa$PredictedClass == xgb_predsa$ActualClass) / nrow(xgb_predsa)
accuracya 

#--------WHITE-------#


varw <- c("sex", "cins", "tfinsal","tfinsal_centered", "quadexp", "exp", "medhouse19", "perc_white",
          "avqwint", "avqspring", "avqsummer", "avqfall") #no crime rate

newdatw <- newdat %>% filter(raceW == 1)

#first we use catools to split the data into train and test
sample_splitw <- caTools::sample.split(Y = newdatw$intmobility, SplitRatio = 0.7)
train_setw <- subset(x = newdatw, sample_splitw == TRUE)
test_setw <- subset(x = newdatw, sample_splitw == FALSE)

y_trainw <- as.integer(train_setw$intmobility) 
y_testw <- as.integer(test_setw$intmobility) 
x_trainw <- train_setw[,varw]
x_testw <- test_setw[,varw]



#we need to dummy everything for xgboost to work
#sparse_matrix_train <- Matrix::sparse.model.matrix(y_train ~ ., data = traincomp)[,-1]
#X_train_dmat = xgb.DMatrix(sparse_matrix_train, label = y_train)




x_trainw <- fastDummies::dummy_cols(x_trainw)
x_trainw <- x_trainw %>% select(-sex)

x_testw <- fastDummies::dummy_cols(x_testw)
x_testw <- x_testw %>% select(-sex)



#we have to use DMatrix because this is how XGBoost store data. It is a specific data structure
#used to store data in a way optimized for memory efficiency and training speed.


xgb_trainw <- xgboost::xgb.DMatrix(data = as.matrix(x_trainw), label = y_trainw)
xgb_testw <- xgboost::xgb.DMatrix(data = as.matrix(x_testw), label = y_testw)

#we will go with the most basic parameter values
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = 4
)



#RUN THE MODEL
xgb_modelw <- xgb.train(
  params = xgb_params,
  data = xgb_trainw,
  nrounds = 10, #let's start with this and hope that we land on something
  verbose = 1
)

xgb_modelw

importance_matrixw <- xgb.importance(
  feature_names = colnames(xgb_trainw), 
  model = xgb_modelw
)
importance_matrixw

#plot the importance matrix
xgb.plot.importance(importance_matrixw)



#predictions - predictors are returned in a form of probabilities
xgb_predsw <- predict(xgb_modelw, as.matrix(x_testw), reshape = TRUE)
xgb_predsw <- as.data.frame(xgb_predsw)
colnames(xgb_predsw) <- levels(as.factor(newdatw$intmobility))



#see how well it predicts
xgb_predsw$PredictedClass <- apply(xgb_predsw, 1, function(y) colnames(xgb_predsw)[which.max(y)])
xgb_predsw$ActualClass <- levels(as.factor(newdatw$intmobility))[y_testw + 1]
xgb_predsw

#calculate over-all accuracy score

accuracyw <- sum(xgb_predsw$PredictedClass == xgb_predsw$ActualClass) / nrow(xgb_predsw)
accuracyw

table(y_trainw)
table(y_traina)
mean(x_traina$tfinsal)

explain <-shapr(x_trainw, xgb_modelw)


