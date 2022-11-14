library("DBI")
con <- DBI::dbConnect(odbc::odbc(),
                      "Databricks",
                      timeout = 60,
                      PWD = rstudioapi::askForPassword("Databricks personal access token:"))

# Load data from DataBricks ----------------------------------------------------
library(dbplyr)
library(dplyr)

#'Load _population_geography table (in two steps cause there is to much data to load in one go)
mydata1<- dbGetQuery(con,"SELECT NHS_NUMBER_DEID, RAW_ETHNICITY, CATEGORISED_ETHNICITY, SEX, DATE_OF_BIRTH, DATE_OF_DEATH 
  FROM dars_nic_391419_j3w9t_collab.ccu037_dp_study_population_geography")

mydata2<- dbGetQuery(con,"SELECT NHS_NUMBER_DEID, AGE_AT_COHORT_START, region_name, imd_decile 
  FROM dars_nic_391419_j3w9t_collab.ccu037_dp_study_population_geography")

mydata = left_join(mydata1,mydata2)
mydata_geo = mydata 

#'Load _study_population_cov 
#'(not including "alcohol_consumption" = daily? consumption, cause almost 100% NA)
mydata1 = mydata #save prior status and merge with following new columns
mydata2<- dbGetQuery(con,"SELECT NHS_NUMBER_DEID,bmi, smoking_status, heavy_alcohol_hasbled  FROM dars_nic_391419_j3w9t_collab.ccu037_01_study_population_cov")
mydata = left_join(mydata1,mydata2)

#'Load _study_population_como 
mydata1 = mydata #save prior status and merge with following new columns
mydata2<- dbGetQuery(con,"SELECT NHS_NUMBER_DEID, AF, AMI, CKD, COPD, DVT, HF, PE, cancer  
                            FROM dars_nic_391419_j3w9t_collab.ccu037_01_study_population_como")
mydata3<- dbGetQuery(con,"SELECT NHS_NUMBER_DEID, dementia, diabetes, hypertension_chads, liver_disease_hasbled, obesity, stroke_chads  
                            FROM dars_nic_391419_j3w9t_collab.ccu037_01_study_population_como")
mydata = left_join(mydata1,mydata2)
mydata = left_join(mydata,mydata3)








# Data exploration -------------------------------------------------------------
#Variables format
mydata$death = ifelse(is.na(mydata$DATE_OF_DEATH),0,1)
mydata$DATE_OF_BIRTH = as.Date(mydata$DATE_OF_BIRTH)
mydata$DATE_OF_DEATH = as.Date(mydata$DATE_OF_DEATH)
mydata$AGE_AT_COHORT_START = as.numeric(mydata$AGE_AT_COHORT_START)
mydata$SMOKE = ifelse(is.na(mydata$smoking_status),0,mydata$smoking_status)

#Sex in "U" = Unknown, "M" = Male and "F" = Female
mydata$SEX3[mydata$SEX == 0 | mydata$SEX == 9] = "U"
mydata$SEX3[mydata$SEX == 1] = "M"
mydata$SEX3[mydata$SEX == 2] = "F"

#Age at cohort start or at death date:
mydata$AGE = mydata$AGE_AT_COHORT_START
mydata$AGE = ifelse(mydata$death == 1,(difftime(mydata$DATE_OF_DEATH, mydata$DATE_OF_BIRTH, units = "days"))/365.25,mydata$AGE)
mydata$AGE = round(mydata$AGE,0)


list_como = c("heavy_alcohol_hasbled","AF","AMI","CKD")
position = match(list_como,names(mydata))
for (i in position){   mydata[,i] = factor(ifelse(is.na(mydata[,i]),0,mydata[,i])) }

list_como = c("COPD","DVT","HF","PE")
position = match(list_como,names(mydata))
for (i in position){   mydata[,i] = factor(ifelse(is.na(mydata[,i]),0,mydata[,i])) }

list_como = c("cancer","dementia","diabetes","hypertension_chads")
position = match(list_como,names(mydata))
for (i in position){  mydata[,i] = factor(ifelse(is.na(mydata[,i]),0,mydata[,i])) }

list_como = c("liver_disease_hasbled","obesity")
position = match(list_como,names(mydata))
for (i in position){  mydata[,i] = factor(ifelse(is.na(mydata[,i]),0,mydata[,i])) }

list_como = c("stroke_chads")
position = match(list_como,names(mydata))
for (i in position){  mydata[,i] = factor(ifelse(is.na(mydata[,i]),0,mydata[,i])) }

#Save a copy of the data as it is now before excluding by age: -----------------
copy <- mydata                                        # Dim = 61810973




#Save the data_frame into the environment before before excluding by age: ------
# set target folder
setwd("/mnt/efs/marta.pinedamoncusi/dars_nic_391419_j3w9t_collab/CCU037/CCU037_01")
# cohort_index
cohort_start_date = '2022_03_23'
# save as rds
filename1 = paste("DATA/CCU037_base_cohort_", cohort_start_date, ".rds", sep="")
  # saveRDS(copy,filename1)

#Recover data copy: ------------------------------------------------------------
mydata <- copy

