# This script extracts tables from Databricks with an ethicity field
# Author: Marta Pineda

rm(list = ls())

library(dplyr)

# Setup Databricks connection --------------------------------------------------
library("DBI")
con <- DBI::dbConnect(odbc::odbc(),
                      "Databricks",
                      timeout = 60,
                      PWD = rstudioapi::askForPassword("Databricks personal access token:"))

# Transfer data from DataBricks ------------------------------------------------

#token: dapiaaad81ef183618b7fad9512bd6124d23


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

# Include most recent Ethnic field from GDPPR table ---------------------------
#eth_codes<- dbGetQuery(con,"SELECT PERSON_ID, ETHNIC FROM dars_nic_391419_j3w9t_collab.ccu037_gdppr_skinny_patient_char_pcandsnomedcode")
#mydata = left_join(mydata,eth_codes,by=c("NHS_NUMBER_DEID" = "PERSON_ID"))

eth_codes<- dbGetQuery(con,"SELECT PERSON_ID, ETHNIC FROM dars_nic_391419_j3w9t_collab.ccu037_01_ethnos_codes_for_individuals_gdppr_23032022")
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

#Load saved data in R: ---------------------------------------------------------
# setwd("/mnt/efs/marta.pinedamoncusi/dars_nic_391419_j3w9t_collab/CCU037/CCU037_01/DATA")
# load(CCU037_base_cohort_for_comparison_tables_2022_03_23.rds)

mydata<- readRDS("~/dars_nic_391419_j3w9t_collab/CCU037/CCU037_01/DATA/CCU037_base_cohort_for_comparison_tables_2022_03_23.rds")

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

#Exclude by Sex="U": -----------------------------------------------------------
mydata = mydata[mydata$SEX3 != "U",]

#Clear space (beware to do it correctly!) --------------------------------------
#temp = ls(); temp = temp[c(5,6,8:12)]
# rm(list = temp)
# gc ()



# Left join more complete SNOMED codes:   --------------------------------------
eth_codes <- dbGetQuery(con,"SELECT PERSON_ID, PrimaryCode as PCs
  FROM dars_nic_391419_j3w9t_collab.ccu037_01_individual_snomed_codes_for_individuals_gdppr_23032022")
eth_codes2 <- dbGetQuery(con,"SELECT PERSON_ID, SNOMED_conceptId_description
  FROM dars_nic_391419_j3w9t_collab.ccu037_01_individual_snomed_codes_for_individuals_gdppr_23032022")
eth_codes = left_join(eth_codes,eth_codes2)

rm(eth_codes2)

mydata = left_join(mydata,eth_codes, by=c("NHS_NUMBER_DEID" = "PERSON_ID"))
rm(eth_codes)

#Combine Primary Codes from Snomed codes abd 
mydata$PC_combined <- ifelse(is.na(mydata$PCs),mydata$ETHNIC,mydata$PCs)
mydata$PC_combined[is.na(mydata$PC_combined)] <- ""
table(mydata$PC_combined, useNA = "always")


#See characteristics of patients (up to now: AGE, SEX and Death)----------------
library(table1)

#'RUN BEFORE TABLES
#'Joint ""/99 as Missing
mydata$Miss <- ifelse(mydata$PC_combined == "" | mydata$PC_combined == "99" ,"Missing",mydata$PC_combined)
#'Joint ""/99/Z as Missing vs all other codes as "Ethnicity code"
mydata$MissZ <- ifelse(mydata$PC_combined == "Z","Missing",mydata$Miss)
table(mydata$MissZ, useNA = "always")
# mydata$RAW_ETHNICITY binary
mydata$RAW_ETHNICITY_binary <- ifelse(mydata$RAW_ETHNICITY == "" | mydata$RAW_ETHNICITY == "99" | mydata$RAW_ETHNICITY == "Z","Missing","RE")
#Include Ethnicity source => GDPPR (excluding Z)
mydata$source <- ""
mydata$source <- ifelse(mydata$PC_combined == "" |mydata$PC_combined == "99" | mydata$PC_combined == "Z", "","GDPPR")
mydata$source <- ifelse(mydata$PC_combined == "Z", "Z_GDPPR",mydata$source)
mydata$source= ifelse(mydata$Miss == "Missing" & mydata$RAW_ETHNICITY_binary == "RE","HES",mydata$source)
mydata$source= ifelse(mydata$source == "","Missing",mydata$source)

mydata$source_MZ= ifelse(mydata$source == "Z_GDPPR","Missing",mydata$source)

sum(
  length(mydata$source[mydata$source=="Z_GDPPR"]) + 
  length(mydata$RAW_ETHNICITY[mydata$RAW_ETHNICITY=="Z" & mydata$Miss == "Missing"])
)
#table(mydata$source, useNA = "always")


#'Codes only from SNOMED CODES
table_PC = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                    factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                  | factor(PC_combined), data=mydata)

#'SNOMED-YES(incl "Z") vs Miss("","99")
table_NAZ = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                     factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                   | factor(Miss), data=mydata)

#'SNOMED-YES vs MissZ("","99" or "Z")
table_NAZ = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                    factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                  | factor(MissZ), data=mydata)


#SNOMED-YES vs MissZ("","99" or "Z") vs HES Ethnicity data:
table_2 = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                     factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                   | factor(source_MZ), data=mydata)



#Combine HES Ethnicity data:
mydata$PC_combined3 = ifelse(mydata$PC_combined == "" | mydata$PC_combined == "99",mydata$RAW_ETHNICITY,mydata$PC_combined)
mydata$Miss3 <- ifelse(mydata$PC_combined3 == "" | mydata$PC_combined3 == "99" | mydata$PC_combined3 == "Z","Missing","Ethnicity code")

table(mydata$Miss3)



#NOT USED =====

#'Include P_value:

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}
#'Re-run 'NA/Z/99' vs 'Ethnic' with P_value column (necessary to exclude oveall column: 'overall=F'):
mydata$treat <- ifelse(mydata$Miss == "Ethnicity code",0,1)
mydata$treat <- factor(mydata$treat, levels=c(0, 1), labels=c("Control", "Treatment"))  #1=Treatment=Missing
#"Missing","Ethnicity code" comparison
table_NA = table1(~ AGE + factor(catAGE) + factor(SEX3) + factor(death)  + factor(imd_decile) + factor(region_name)  + factor(SMOKE) + factor(heavy_alcohol_hasbled) + bmi + 
                    factor(AF)+factor(AMI)+factor(CKD)+factor(COPD)+factor(DVT)+factor(HF)+factor(PE)+factor(cancer)+factor(dementia)+factor(diabetes)+factor(hypertension_chads)+factor(liver_disease_hasbled)+factor(obesity)+factor(stroke_chads)
                  #| treat, data=mydata,overall=F, extra.col=list(`P-value`=pvalue))
                   | treat, data=mydata, delzero=T )

# END  not used ===========


#See the most common Ethnicities obtained from linked-HES in individuals with a missing in GDPPR: --------
#i.e., mydata$RAW_ETHNICITY[mydata$source_MZ=="HES"]
library(epiR); 
#Transform 0-99 to Primary Codes
mydata$eth <- mydata$RAW_ETHNICITY
mydata$eth[mydata$RAW_ETHNICITY == "0"] <- "C"
mydata$eth[mydata$RAW_ETHNICITY == "1"] <- "M"
mydata$eth[mydata$RAW_ETHNICITY == "2"] <- "N"
mydata$eth[mydata$RAW_ETHNICITY == "3"] <- "P"
mydata$eth[mydata$RAW_ETHNICITY == "4"] <- "H"
mydata$eth[mydata$RAW_ETHNICITY == "5"] <- "J"
mydata$eth[mydata$RAW_ETHNICITY == "6"] <- "K"
mydata$eth[mydata$RAW_ETHNICITY == "7"] <- "R"
mydata$eth[mydata$RAW_ETHNICITY == "8"] <- "S"
mydata$eth[mydata$RAW_ETHNICITY == "X"] <- "Z"

mydata$eth <- factor(mydata$eth, 
                     levels = c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","R","S","T","W","Z"),
                     labels = c("British","Irish",
                                "Any other White background",
                                "White and Black Caribbean",
                                "White and Black African", 
                                "White and Asian",
                                "Any other Mixed background",
                                "Indian",
                                "Pakistani", 
                                "Bangladeshi",
                                "Any other Asian background",
                                "Caribbean", 
                                "African",
                                "Any other Black background",
                                "Chinese",
                                "Any other ethnic group",
                                "Gypsy or Irish Traveller",
                                "Arab",
                                "Unkown/Not stated" ))
mydata$eth <- factor(mydata$eth)

#Prevalence HES-linked ethnicity (Primary codes):
#list_HES = data.frame(round(prop.table(table(mydata$eth[mydata$source_MZ=="HES"]))*100,2))
#list_HES = list_HES[order(-list_HES$Freq),]
table_HES = data.frame(table(mydata$eth[mydata$source_MZ=="HES"]),npop = length((mydata$eth[mydata$source_MZ=="HES"])))
table_HES = table_HES[order(-table_HES$Freq),]
prev_HES  = epi.conf(as.matrix(table_HES[,c(2,3)]), ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95) * 100
prev_HES  = data.frame(Ethnicity = table_HES[,1],prev_HES)
prev_HES2 = data.frame(Ethnicity = table_HES[,1],IR_HES = prev_HES[,2], Low_HES = prev_HES[,3], High_HES = prev_HES[,2])

#Prevalence GDPPR ethnicity (Primary codes):
#list_GDPPR = data.frame(round(prop.table(table(mydata$eth[mydata$source_MZ=="GDPPR"]))*100,2))
#list_GDPPR = list_GDPPR[order(-list_GDPPR$Freq),]
table_GDPPR = data.frame(table(mydata$eth[mydata$source_MZ=="GDPPR"]),npop = length((mydata$eth[mydata$source_MZ=="GDPPR"])))
table_GDPPR = table_GDPPR[order(-table_GDPPR$Freq),]
prev_GDPPR  = epi.conf(as.matrix(table_GDPPR[,c(2,3)]), ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95) * 100
prev_GDPPR  = data.frame(Ethnicity = table_GDPPR[,1],prev_GDPPR)
prev_GDPPR2 = data.frame(Ethnicity = table_GDPPR[,1],IR_GDPPR = prev_GDPPR[,2], Low_GDPPR = prev_GDPPR[,3], High_GDPPR = prev_GDPPR[,2])

#Merge in one table (two formats):
prev = data.frame(rbind(prev_HES,prev_GDPPR), source = c(rep("HES",length(prev_HES[,1])),rep("GRPPR",length(prev_GDPPR[,1]))) )
prev2= left_join(prev_HES2,prev_GDPPR2)

#BAR PLOT:
#library(ggplot2)
#library(tidyverse)
prev$Ethnicity = factor(prev$Ethnicity,  levels = rev(prev_HES$Ethnicity))

p = ggplot(prev,aes( x=Ethnicity, y=est, group=source, fill=source))+
      coord_flip()+ guides(fill = guide_legend(reverse = TRUE)) + 								
      geom_bar(stat="identity", position='dodge', alpha=.6, width=.92)+ 	
      geom_text(aes(label = paste(round(est,2),"%"), fontface = "bold.italic"),alpha = .6, position = position_dodge(width = 0.8), hjust = c(1.2,rep(-0.15,18),1.2,rep(-0.15,18))) + #hjust = "left" ##' XX% text
      #geom_errorbar(aes(ymin = lower, ymax = upper), alpha=0.5,position=position_dodge(width = .9)) +
      theme_bw() + labs(title="Prevalence of ethnicity groups from linked-HES and GDPPR records", y="Prevalence (%)", x = "Ethnicity groups\n") +
      scale_fill_manual(values=(c("#E69F00", "#56B4E9")), name="Source of ethnicity\ndata", labels=c("GDPPR","linked-HES")) +  #The original order is first GDPPR and the HES, but it looks fliped in the plot for the guide_legend(reverse)
      theme(axis.text=element_text(size=12),  axis.title=element_text(size=14,face="bold")) 

#Subplot excluding British
p2 = ggplot(prev[prev$Ethnicity != "British",],aes( x=Ethnicity, y=est, group=source, fill=source))+
      coord_flip()+ guides(fill = guide_legend(reverse = TRUE)) + 								
      geom_bar(stat="identity", position='dodge', alpha=.6, width=.92)+ 	
      geom_text(aes(label = paste(round(est,2),"%"), fontface = "bold.italic"),alpha = .6, position = position_dodge(width = 0.8), hjust = c(rep(-0.15,18),1.1,rep(-0.15,17))) + #hjust = "left" ##' XX% text
      #geom_errorbar(aes(ymin = lower, ymax = upper), alpha=0.5,position=position_dodge(width = .9)) +
      theme_bw() + labs(title="Prevalence of ethnicity groups from linked-HES and GDPPR records\n (excluding British ethnicity)", y="Prevalence (%)", x = "Ethnicity groups\n") +
      scale_fill_manual(values=(c("#E69F00", "#56B4E9")), name="Source of ethnicity\ndata", labels=c("GDPPR","linked-HES")) +  #The original order is first GDPPR and the HES, but it looks fliped in the plot for the guide_legend(reverse)
      theme(axis.text=element_text(size=12),  axis.title=element_text(size=14,face="bold")) 

#1.Create pdf where each page is a separate plot.
#cairo_pdf("/mnt/efs/marta.pinedamoncusi/dars_nic_391419_j3w9t_collab/CCU037/CCU037_01/OUTPUT_diferences_NAvsAZ_gdppr/Prevalence_ethnicities_from_linkedHESvsGDPPR.pdf",width=10.06, height=8.06,onefile=T)  
 print(p)
 print(p2)
dev.off()


