# This script extracts tables from Databricks with an ethicity field
# Author: Marta Pineda

rm(list = ls())

# Setup Databricks connection --------------------------------------------------
library("DBI")
con <- DBI::dbConnect(odbc::odbc(),
                      "Databricks",
                      timeout = 60,
                      PWD = rstudioapi::askForPassword("Databricks personal access token:"))

# Transfer data from DataBricks ------------------------------------------------

#token: dapi178a33ca95dfe4f8828f435a4f903d8f

#New code after OMOP course (rum from here):
#library(DBI)
library(dbplyr)
library(dplyr)

# Load data from DataBricks ----------------------------------------------------

#'Load _population_geography table (in two steps cause there is to much data to load in one go)
  mydata1<- dbGetQuery(con,"SELECT NHS_NUMBER_DEID, RAW_ETHNICITY, CATEGORISED_ETHNICITY, SEX, DATE_OF_BIRTH, DATE_OF_DEATH 
  FROM dars_nic_391419_j3w9t_collab.ccu037_dp_study_population_geography")

  mydata2<- dbGetQuery(con,"SELECT NHS_NUMBER_DEID, AGE_AT_COHORT_START, region_name, imd_decile 
  FROM dars_nic_391419_j3w9t_collab.ccu037_dp_study_population_geography")

  mydata = left_join(mydata1,mydata2)
  #mydata_geo = mydata 

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

#'Relise space in the environment:  
  rm(mydata1,mydata2,mydata3)
  
#'Save mydata as it is now
  saved = mydata
      
#'subset with individuals present in Freya's table:   
  #ref_ID <- dbGetQuery(con,"SELECT distinct(NHS_NUMBER_DEID) FROM dars_nic_391419_j3w9t_collab.ccu037_gdppr_primary_and_snomed_fa")
  #mydata = inner_join(saved,ref_ID,  by = "NHS_NUMBER_DEID")
  
  
#'Include the columns with snomed codes from "dars_nic_391419_j3w9t_collab.ccu037_gdppr_skinny_patient_char_pcandsnomedcode" columns ETHNIC, SNOMED_conceptId_description, PrimaryCode 
  # where ETHNIC = primary code field in gdppr, SNOMED_conceptId_description, PrimaryCode = primary code converted from SNOMED 
  #'include only individuals from the subset for the joing or perform a left join
  
  eth_codes<- dbGetQuery(con,"SELECT PERSON_ID, ETHNIC, SNOMED_conceptId_description, PrimaryCode FROM dars_nic_391419_j3w9t_collab.ccu037_gdppr_skinny_patient_char_pcandsnomedcode")
  
  mydata = left_join(mydata,eth_codes,by=c("NHS_NUMBER_DEID" = "PERSON_ID"))
  

    
      
# Data legend  -----------------------------------------------------------------
# SEX: 0  Not Known
#      1  Male
#      2  Female
#      9  Not specified


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
  #summary(mydata$AGE)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #-80.00   23.00   41.00   41.91   60.00  141.00 
  ##Some ages are no possible: i.e., <0 and >115.
  #mydata[mydata$AGE < 0,]         # 2 cases where DoD < DoB       # Raising Question: should we exclude <18y?
  #View(mydata[mydata$AGE > 115,]) # No DoD, DoD<1900, DoB = 1860,


#Include 0 when Na in the following comorbidities and heavy_alcohol:
#"AF","AMI","CKD","COPD","DVT","HF","PE","cancer","dementia","diabetes","hypertension_chads","liver_disease_hasbled","obesity","stroke_chads"
#The loop including all of them is "to big" to be run at once
#   list_como = c("heavy_alcohol_hasbled","AF","AMI","CKD","COPD","DVT","HF","PE","cancer","dementia","diabetes","hypertension_chads","liver_disease_hasbled","obesity","stroke_chads")
#   position = match(list_como,names(mydata))
#   for (i in position){
#     mydata[,i] = factor(ifelse(is.na(mydata[,i]),0,mydata[,i])) }

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
filename1 = paste("DATA/CCU037_base_cohort_for_comparison_tables_", cohort_start_date, ".rds", sep="")
  # SAVE:#
  # saveRDS(copy,filename1)

#Load saved data: --------------------------------------------------------------
  # setwd("/mnt/efs/marta.pinedamoncusi/dars_nic_391419_j3w9t_collab/CCU037/CCU037_01")
  # load(CCU037_base_cohort_for_comparison_tables_2022_03_23)

#Recover data from copy: -------------------------------------------------------
mydata <- copy

#Exclude by age: ---------------------------------------------------------------
#Exclude AGE >=0 and AGE <= 115 
#date_of_birth_cut_off = '1908-01-01' #below this implies incorrect data as would be older than oldest UK living person:2022-1908 = 114y
mydata = mydata[mydata$AGE >= 0 & mydata$AGE <= 115,] #Current dim = 61810952  (we only have excluded 21 patients)
hist(mydata$AGE)

mydata$catAGE <- with(mydata, cut( as.numeric(AGE), breaks = c(-1,17,seq(29.999, 89.999, by = 10),116), 
                            labels = c('0-17','18-29','30-39','40-49', '50-59', '60-69', '70-79', '80-89', '90+')))
#summary(mydata$catAGE)

#Exclude by Sex="U": ---------------------------------------------------------------
mydata = mydata[mydata$SEX3 != "U",]

#Clear space (beware to do it correctly!) -------------------------------------------------------------------
temp = ls(); temp = temp[c(5,6,8:12)]
# rm(list = temp)
# gc ()

#continue----

# Left join more copmlete SNOMED codes:
eth_codes <- dbGetQuery(con,"SELECT PERSON_ID, PrimaryCode as PCs
 FROM dars_nic_391419_j3w9t_collab.ccu037_01_individual_snomed_codes_for_individuals_gdppr_23032022")

eth_codes2 <- dbGetQuery(con,"SELECT PERSON_ID, SNOMED_conceptId_description
 FROM dars_nic_391419_j3w9t_collab.ccu037_01_individual_snomed_codes_for_individuals_gdppr_23032022")

eth_codes = left_join(eth_codes,eth_codes2)
rm(eth_codes2)

mydata = left_join(mydata,eth_codes, by=c("NHS_NUMBER_DEID" = "PERSON_ID"))

#mydata$PC_combined <- mydata$PCs
mydata$PC_combined <- ifelse(is.na(mydata$PCs),mydata$ETHNIC,mydata$PCs)

#See characteristics of patients (up to now: AGE, SEX and Death)----------------
  library(table1)

#'Codes only from SNOMED CODES
table_HE = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                    factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                  | factor(PCs), data=mydata)

table(mydata$PrimaryCode, useNA = "always")
nrow(mydata[mydata$ETHNIC != "",]) #table(copy$ETHNIC[copy$ETHNIC == ""]) # n "" = 33139704 
nrow(mydata[(mydata$ETHNIC != "" & mydata$ETHNIC != "99"),])

table(mydata$RAW_ETHNICITY, useNA = "always")

#'Tables |Ethnicity_class
#'Stratify by High categories
     table_HE = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                       factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                       | factor(CATEGORISED_ETHNICITY), data=mydata)
     table_filename = paste("OUTPUT/table_HighCat_Ethnicity_variables", cohort_start_date, ".csv", sep="")
     write.csv(table_HE, table_filename, row.names=T, quote=F)
     rm(table_HE) 
     
#'Stratify by Primary Codes
#'
      #' Including 1-9 codes
      table_PC = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                     factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                   | factor(RAW_ETHNICITY), data=mydata)
     
     table_filename = paste("OUTPUT/table_PrimaryCodes_Ethnicity_variables", cohort_start_date, ".csv", sep="")
     write.csv(table_PC, table_filename, row.names=T, quote=F)
     rm(table_PC) 
     

     #' Excluding 1-9 codes (can I run at once, not in two parts?)
     # Part 1 
     temp = mydata[mydata$RAW_ETHNICITY != "0" | mydata$RAW_ETHNICITY != "1" | mydata$RAW_ETHNICITY != "2" | mydata$RAW_ETHNICITY != "3" | mydata$RAW_ETHNICITY != "4" | mydata$RAW_ETHNICITY != "5" | mydata$RAW_ETHNICITY != "6" | mydata$RAW_ETHNICITY != "7" | mydata$RAW_ETHNICITY != "8" |  mydata$RAW_ETHNICITY != "99" ,]
     
     table_PC_AZ   = table1(~ AGE + factor(catAGE) + factor(death) + factor(SEX3) + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi| factor(RAW_ETHNICITY) * factor(SEX3) , data=temp)
     table_filename = paste("OUTPUT/table_AZPrimaryCodes_Ethnicity_variables", cohort_start_date, ".csv", sep="")
     write.csv(table_PC_AZ, table_filename, row.names=T, quote=F)
     rm(table_PC_AZ) 
     
     # Part 2
     table_PC_AZ2  = table1(~ factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)| factor(RAW_ETHNICITY) , data=temp)
     
     table_filename = paste("OUTPUT/table_Part2AZPrimaryCodes_Ethnicity_variables", cohort_start_date, ".csv", sep="")
     write.csv(table_PC_AZ2, table_filename, row.names=T, quote=F)
     rm(table_PC_AZ2,temp) 

     #CONTINUE:
     
     #'Stratify by Z vs U vs Something
     mydata$Group = ifelse(mydata$RAW_ETHNICITY == "","NA","Something") #table(mydata$Group)
     mydata$Group = ifelse(mydata$RAW_ETHNICITY == "Z"|mydata$RAW_ETHNICITY == "99"| mydata$RAW_ETHNICITY == "X","Z",mydata$Group) 
     table_Z_U_S = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                            factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                          | factor(Group) , data=mydata)
     
     table_filename = paste("OUTPUT/table_ZvsUvsSomething_Ethnicity_variables", cohort_start_date, ".csv", sep="")
     write.csv(table_Z_U_S, table_filename, row.names=T, quote=F)
     rm(table_Z_U_S) 
     
     #'Stratify by Z/U vs something
     mydata$Group = ifelse(mydata$RAW_ETHNICITY == "" |mydata$RAW_ETHNICITY == "Z"|mydata$RAW_ETHNICITY == "99"| mydata$RAW_ETHNICITY == "X","NA/Z","Something") 
     table_ZU_S = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                           factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                          | factor(Group) , data=mydata)
     
     
     table_filename = paste("OUTPUT/table_Z&UvsSomething_Ethnicity_variables", cohort_start_date, ".csv", sep="")
     write.csv(table_ZU_S, table_filename, row.names=T, quote=F)
     rm(table_ZU_S) 
     

#' Open saved table:
#  table <- read.csv("OUTPUT/2_table_PrimaryCodes_Ethnicity_variables2022_03_23.csv", header=TRUE)
     


#'Tables |Ethnicity_class * SEX
#'Stratify by High categories
  #'1.Load tables 
     #table_HE_dem  = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi| factor(CATEGORISED_ETHNICITY) * factor(SEX3) , data=mydata)
     #table_HE_como = table1(~ factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)| factor(CATEGORISED_ETHNICITY) * factor(SEX3) , data=mydata)
     table_HE = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                      factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                    | factor(CATEGORISED_ETHNICITY) * factor(SEX3) , data=mydata)
  #'2.Save tables 
     table_filename = paste("OUTPUT/table_HighCat_Ethnicity_variables_*sex_", cohort_start_date, ".csv", sep="")
     write.csv(table_HE, table_filename, row.names=T, quote=F)

  #'3.Empty memory
     rm(table_HE) 
     
#'Stratify by Primary Codes
  #' Including 1-9 codes
    #'1.Load tables 
    #table_PC = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
    #                factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
    #              | factor(RAW_ETHNICITY) * factor(SEX3) , data=mydata)
    #'2.Save tables 
    #table_filename = paste("OUTPUT/table_PrimaryCodes_Ethnicity_variables", cohort_start_date, ".csv", sep="")
    #write.csv(table_PC, table_filename, row.names=T, quote=F)
    #'3.Empty memory
    #rm(table_PC) 
    #'Part 1
     table_PC   = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi| factor(RAW_ETHNICITY) * factor(SEX3) , data=mydata)
     table_filename = paste("OUTPUT/table_PrimaryCodes_Ethnicity_variables_*sex_", cohort_start_date, ".csv", sep="")
     write.csv(table_PC, table_filename, row.names=T, quote=F)
     rm(table_PC) 
     
     #Part 2
     table_PC2  = table1(~ factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)| factor(RAW_ETHNICITY) * factor(SEX3) , data=mydata)
     table_filename = paste("OUTPUT/table_Part2PrimaryCodes_Ethnicity_variables_*sex_", cohort_start_date, ".csv", sep="")
     write.csv(table_PC2, table_filename, row.names=T, quote=F)
     rm(table_PC2) 

             
  #' Excluding 1-9 codes
    #'1.Load tables 
    temp = mydata[mydata$RAW_ETHNICITY != "0" | mydata$RAW_ETHNICITY != "1" | mydata$RAW_ETHNICITY != "2" | mydata$RAW_ETHNICITY != "3" | mydata$RAW_ETHNICITY != "4" | mydata$RAW_ETHNICITY != "5" | mydata$RAW_ETHNICITY != "6" | mydata$RAW_ETHNICITY != "7" | mydata$RAW_ETHNICITY != "8" |  mydata$RAW_ETHNICITY != "99" ,]
    
    table_PC_AZ   = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi| factor(RAW_ETHNICITY) * factor(SEX3) , data=temp)
    table_filename = paste("OUTPUT/table_AZPrimaryCodes_Ethnicity_variables_*sex_", cohort_start_date, ".csv", sep="")
    write.csv(table_PC_AZ, table_filename, row.names=T, quote=F)
    rm(table_PC_AZ) 
    
    #Part 2
    table_PC_AZ2  = table1(~ factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)| factor(RAW_ETHNICITY) * factor(SEX3) , data=temp)
    
    table_filename = paste("OUTPUT/table_Part2AZPrimaryCodes_Ethnicity_variables_*sex_", cohort_start_date, ".csv", sep="")
    write.csv(table_PC_AZ2, table_filename, row.names=T, quote=F)
    rm(table_PC_AZ2,temp) 
    
    #table_PC_AZ = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
    #               factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
    #              | factor(RAW_ETHNICITY) * factor(SEX3) , data=temp)
    #'2.Save tables 
    #table_filename = paste("OUTPUT/table_AZPrimaryCodes_Ethnicity_variables", cohort_start_date, ".csv", sep="")
    #write.csv(table_PC_AZ, table_filename, row.names=T, quote=F)
    #'3.Empty memory
    #rm(table_PC_AZ,temp,table_PC_AZ2) 

#CONTINUE:
    
#'Stratify by Z vs U vs Something
mydata$Group = ifelse(mydata$RAW_ETHNICITY == "","NA","Something") #table(mydata$Group)
mydata$Group = ifelse(mydata$RAW_ETHNICITY == "Z"|mydata$RAW_ETHNICITY == "99"| mydata$RAW_ETHNICITY == "X","Z",mydata$Group) 
table_Z_U_S = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                      factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                    | factor(Group) * factor(SEX3) , data=mydata)

table_filename = paste("OUTPUT/table_ZvsUvsSomething_Ethnicity_variables_*sex_", cohort_start_date, ".csv", sep="")
write.csv(table_Z_U_S, table_filename, row.names=T, quote=F)
rm(table_Z_U_S) 



#'Stratify by Z/U vs something
mydata$Group = ifelse(mydata$RAW_ETHNICITY == "" |mydata$RAW_ETHNICITY == "Z"|mydata$RAW_ETHNICITY == "99"| mydata$RAW_ETHNICITY == "X","NA/Z","Something") 
table_ZU_S = table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + heavy_alcohol_hasbled + bmi +
                     factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                   | factor(Group) * factor(SEX3) , data=mydata)

table_filename = paste("OUTPUT/table_Z&UvsSomething_Ethnicity_variables_*sex_", cohort_start_date, ".csv", sep="")
write.csv(table_ZU_S, table_filename, row.names=T, quote=F)
rm(table_ZU_S) 


#Save table df
#data.table::fwrite(df,"data/ccu037_table_name_data.csv.gz")



#=================================================================================
#CODE FOR: See characteristics of patients (up to now: AGE, SEX and Death)----------------
library(table1)
run = NOT
if (run == YES) {
  #table1(~ AGE_AT_COHORT_START + factor(SEX) + factor(death) | factor(RAW_ETHNICITY), data=mydata)
  table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death) + factor(imd_decile) + factor(region_name) + factor(SMOKE) + alcohol_consumption + bmi| factor(RAW_ETHNICITY),         data=mydata)
  table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death) + factor(imd_decile) + factor(region_name) + factor(SMOKE) + alcohol_consumption + bmi| factor(CATEGORISED_ETHNICITY), data=mydata)

  table1(~ AGE + factor(catAGE) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + alcohol_consumption + bmi| factor(CATEGORISED_ETHNICITY) * factor(SEX3) , data=mydata)
  table1(~ factor(SEX3) + factor(death) + factor(imd_decile) + factor(region_name) + factor(SMOKE) + alcohol_consumption + bmi | factor(CATEGORISED_ETHNICITY) * factor(catAGE), data=mydata)

#'Compare NA vs the rest
  mydata$Group = ifelse(mydata$RAW_ETHNICITY == "","NA","Something") #table(mydata$Group) #NA=2195438 vs Something=59615514
  table1(~ AGE + factor(catAGE) +  factor(SEX3) + factor(death) + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + alcohol_consumption + bmi| factor(Group), data=mydata)

#'Compare NA/Z vs the rest
  mydata$Group = ifelse(mydata$RAW_ETHNICITY == "" |mydata$RAW_ETHNICITY == "Z"|mydata$RAW_ETHNICITY == "99"| mydata$RAW_ETHNICITY == "X","NA/Z","Something") 
  #table(mydata$Group) #NA= vs Something=
  table1(~ AGE + factor(catAGE) +  factor(SEX3) + factor(death) + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + alcohol_consumption + bmi| factor(Group), data=mydata)

#'Compare Unknown vs Z vs the rest
  mydata$Group = ifelse(mydata$RAW_ETHNICITY == "","NA","Something") #table(mydata$Group)
  mydata$Group = ifelse(mydata$RAW_ETHNICITY == "Z"|mydata$RAW_ETHNICITY == "99"| mydata$RAW_ETHNICITY == "X","Z",mydata$Group) 
  #table(mydata$Group) #NA= vs "Z"= vs Something=
  table1(~ AGE + factor(catAGE) +  factor(SEX3) + factor(death) + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + alcohol_consumption + bmi| factor(Group), data=mydata)

}

