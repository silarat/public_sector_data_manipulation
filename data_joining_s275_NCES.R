setwd("XX")
dat <- read.csv("dat_ML.csv")
join <- read.csv("XXX")
library(tidyverse)
library(stringr)

options(scipen = 100)
options(digits = 3)
colnames(join)
join_data <- join %>% filter(STATENAME == "WASHINGTON") %>% 
  select(ST_SCHID, NCESSCH, SCHID, ST_LEAID, LEAID, STATENAME)
data <- dat %>% filter(schyear == 2022)  %>% select(cert, FirstName, LastName, bldgn, dis, codist)

#note that the crosswalk or join file document school code in a combined way,
#whereas S275 have separate code for coudist and building. So we will have to pull apart
#the combined code in the crosswalk

join_data <- join_data %>% mutate(codist = str_extract(ST_SCHID, "\\d{5}")) %>%
  mutate(bldgn = str_extract(ST_SCHID, "(?<=-)[0-9]{4}$"))

#make sure these new variables are integer
join_data$codist <- as.integer(join_data$codist)
join_data$bldgn <- as.integer(join_data$bldgn)

str(join_data)

combined <- data %>% left_join(join_data, by = c("bldgn", "codist"))

#awesome, now we just need to reanem
combined$dis <- NULL
combined$SCHID <- NULL

colnames(combined) <- c("Certificate_Number", "First_Name", "Last_Name",
                        "OSPI_School_Code", "OSPI_countydistrict_ID", "State_School_ID", 
                        "NCES_School_ID", "State_LEA_ID", "NCES_LEA_ID", "State")

write.csv(combined, "c:/Users/Palmy/Desktop/teacher_attrition_STEM_workforce/RAND/s275-schyear2022-with-NCES-info.csv")
