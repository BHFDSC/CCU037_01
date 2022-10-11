#==================================================================================================================================================================================================#
# 
# DATA LOAD & PACKAGES, Functions, etc.
#
#==================================================================================================================================================================================================#

#--------------------------------------------------------------------------------------------------#
# Load Packages
#--------------------------------------------------------------------------------------------------#
#Library for horizontal interactive/collapsible Tree [saved in htlm]
#NOTE: Better to produce in R studio:
#install.packages('collapsibleTree')
library(collapsibleTree)

# Library for manaing strings
library(stringr)

# Libraries for vertical tree (non interactive/collapsible) [saved in tiff]
library(ggraph)
library(igraph)
library(tidyverse)
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
# Functions
#--------------------------------------------------------------------------------------------------#
'%!in%' <- function(x,y)!('%in%'(x,y))

#--------------------------------------------------------------------------------------------------#
# Load Data
#--------------------------------------------------------------------------------------------------#
 rm(list = ls())
 setwd("C:") 	#Disc of data location
 mydata <- read.csv("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Downoaded TRE tables/Patient_most_recent_Snomed_ethn_code_export_18022022.csv",fill = TRUE, header = TRUE)
 head(mydata)
#--------------------------------------------------------------------------------------------------#


#==================================================================================================================================================================================================#
#
# FORMAL ANALYSIS 
#
#==================================================================================================================================================================================================#

#--------------------------------------------------------------------------------------------------#
# 1. PREPARE DATA BASE: GET high level cat (i.e., six_cat), helper group variables and n(%)
#--------------------------------------------------------------------------------------------------#
#Merge PrimaryCode into 6 categories:
mydata$six_cat <- "NA"
mydata$six_cat <- ifelse(mydata$PrimaryCode == '1'|mydata$PrimaryCode == '2'|mydata$PrimaryCode == '3'|mydata$PrimaryCode == 'N'|mydata$PrimaryCode == 'M'|mydata$PrimaryCode == 'P', "Black or Black British", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == '0'|mydata$PrimaryCode == 'A'|mydata$PrimaryCode == 'B'|mydata$PrimaryCode == 'C', "White", mydata$six_cat) 
mydata$six_cat <- ifelse(mydata$PrimaryCode == '4'|mydata$PrimaryCode == '5'|mydata$PrimaryCode == '6'|mydata$PrimaryCode == 'L'|mydata$PrimaryCode == 'K'|mydata$PrimaryCode == 'J'|mydata$PrimaryCode == 'H',"Asian or Asian British", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == '7'|mydata$PrimaryCode == '8'|mydata$PrimaryCode == 'W'|mydata$PrimaryCode == 'T'|mydata$PrimaryCode == 'S'|mydata$PrimaryCode == 'R',"Other Ethnic Groups", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == 'D'|mydata$PrimaryCode == 'E'|mydata$PrimaryCode == 'F'|mydata$PrimaryCode == 'G',"Mixed", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == '9'|mydata$PrimaryCode == '99'|mydata$PrimaryCode == 'Z'|mydata$PrimaryCode == 'X',"Unknown",mydata$six_cat)

#Calculate total number of individuals
all_patients = sum(mydata$n_id_distinct)

#Obtain the frequency of each of the 6 categories vs. all GDPPR patients: mydata$p6_all 
mydata$p6_all[mydata$six_cat == "Asian or Asian British"] 	<- sum(mydata$n_id_distinct[mydata$six_cat == "Asian or Asian British"]) / all_patients
mydata$p6_all[mydata$six_cat == "Black or Black British"] 	<- sum(mydata$n_id_distinct[mydata$six_cat == "Black or Black British"]) / all_patients
mydata$p6_all[mydata$six_cat == "Mixed"]				<- sum(mydata$n_id_distinct[mydata$six_cat == "Mixed"])  / all_patients
mydata$p6_all[mydata$six_cat == "Other Ethnic Groups"] 	<- sum(mydata$n_id_distinct[mydata$six_cat == "Other Ethnic Groups"])  / all_patients
mydata$p6_all[mydata$six_cat == "Unknown"] 			<- sum(mydata$n_id_distinct[mydata$six_cat == "Unknown"])  / all_patients
mydata$p6_all[mydata$six_cat == "White"] 				<- sum(mydata$n_id_distinct[mydata$six_cat == "White"])  / all_patients

#A-Z categories (mydata$PrimaryCode): "0","1","2","3","4","5","6","7","8","9","99","X" is not used (we have seen before, to fix the all cat plot, we modify from here).
# 'See "0.0 Ethnicity Tree - Copy before claning.R" section/run "# 2C. BUILD THE TREE (using package )" to paste before this modifications)  
B_char = c('N','M','P')		#c('1','2','3','N','M','P')
W_char = c('A','B','C')		#c('0','A','B','C')
A_char = c('L','K','J','H')	#c('4','5','6','L','K','J','H')
O_char = c('W','T','S','R')	#c('7','8','W','T','S','R')
M_char = c('D','E','F','G')
U_char = c('Z')			#c('9','99','Z','X')

A_sum = sum(mydata$n_id_distinct[mydata$six_cat == "Asian or Asian British"]) 
B_sum = sum(mydata$n_id_distinct[mydata$six_cat == "Black or Black British"])
M_sum = sum(mydata$n_id_distinct[mydata$six_cat == "Mixed"])
O_sum = sum(mydata$n_id_distinct[mydata$six_cat == "Other Ethnic Groups"]) 
U_sum = sum(mydata$n_id_distinct[mydata$six_cat == "Unknown"])
W_sum = sum(mydata$n_id_distinct[mydata$six_cat == "White"])

# Get frequence of A-Z vs all_patients in GDPPR: pAZ_all: # "0","1","2","3","4","5","6","7","8","9","99","X" is not used (we have seen before, to fix the all cat plot, we modify from here).
# 'See "0.0 Ethnicity Tree - Copy before claning.R" section/run "# 2C. BUILD THE TREE (using package )" to check that before this modifications)  
# pAZ_list <- c("0","1","2","3","4","5","6","7","8","9","99","A","B","C","D","E","F","G","H","J","K","L","M","N","P","R","S","T","W","Z","X") 
  pAZ_list <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","R","S","T","W","Z") #

for (i in 1:length(pAZ_list)){mydata$pAZ_all[mydata$PrimaryCode == pAZ_list[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == pAZ_list[i]]) / all_patients}

# Get frequence of A-Z within its six_cat: pAZ_6
#mydata$pAZ_6 <- "NA"
for (i in 1:length(B_char)){mydata$pAZ_6[mydata$PrimaryCode == B_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == B_char[i]]) / B_sum}
for (i in 1:length(W_char)){mydata$pAZ_6[mydata$PrimaryCode == W_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == W_char[i]]) / W_sum}
for (i in 1:length(A_char)){mydata$pAZ_6[mydata$PrimaryCode == A_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == A_char[i]]) / A_sum}
for (i in 1:length(O_char)){mydata$pAZ_6[mydata$PrimaryCode == O_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == O_char[i]]) / O_sum}
for (i in 1:length(M_char)){mydata$pAZ_6[mydata$PrimaryCode == M_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == M_char[i]]) / M_sum}
for (i in 1:length(U_char)){mydata$pAZ_6[mydata$PrimaryCode == U_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == U_char[i]]) / U_sum}
# Get 'n' of A-Z within its six_cat: pAZ_6_n
#mydata$pAZ_6_n <- "NA"
for (i in 1:length(B_char)){mydata$pAZ_6_n[mydata$PrimaryCode == B_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == B_char[i]]) }
for (i in 1:length(W_char)){mydata$pAZ_6_n[mydata$PrimaryCode == W_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == W_char[i]]) }
for (i in 1:length(A_char)){mydata$pAZ_6_n[mydata$PrimaryCode == A_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == A_char[i]]) }
for (i in 1:length(O_char)){mydata$pAZ_6_n[mydata$PrimaryCode == O_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == O_char[i]]) }
for (i in 1:length(M_char)){mydata$pAZ_6_n[mydata$PrimaryCode == M_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == M_char[i]]) }
for (i in 1:length(U_char)){mydata$pAZ_6_n[mydata$PrimaryCode == U_char[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == U_char[i]]) }

# Get frequence of Snomed codes vs all_patients in GDPPR: snomed_all
#mydata$snomed_all <- mydata$n_id_distinct/all_patients
mydata$snomed_all <- (mydata$n_id_distinct/all_patients) *100 #%

# Get frequence of Snomed codes vs A-Z in GDPPR: snomed_pAZ
#MODIFY:#for (i in 1:length(pAZ_list)){mydata$pAZ_all[mydata$PrimaryCode == pAZ_list[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == pAZ_list[i]]) / all_patients}

all_data <- mydata

#--------------------------------------------------------------------------------------------------#
# 1.4 Barplots by six_cat (remove those whose SNOMED$n = 0)  - FULL SET
#--------------------------------------------------------------------------------------------------#
mydata <- all_data
mydata = mydata[mydata$n != 0,]
mydata$SNOMED_ord = factor(mydata$SNOMED_conceptId_description, levels=c(mydata$SNOMED_conceptId_description))

# 1.4.1 Asian subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Asian/Asian British") ))
pA<- ggplot(mydata[mydata$six_cat == "Asian or Asian British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
( ppA = pA + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED codes")+ theme(plot.title = element_text(vjust = 0)) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_Asian_gdppr.tiff", ppA, width=16 , height=8, units = "in", dpi=600 )

# 1.4.2 Black subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Black/African/Caribbean/Black British") ))
pB<- ggplot(mydata[mydata$six_cat == "Black or Black British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
( ppB = pB + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0)) )
 #ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_Black_gdppr.tiff", ppB, width=16 , height=6, units = "in", dpi=600 )

# 1.4.3 Mixed subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Mixed") ))
pM<- ggplot(mydata[mydata$six_cat == "Mixed",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
( ppM = pM + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0)) )
 #ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_Mixed_gdppr.tiff", ppM, width=16 , height=6, units = "in", dpi=600 )

# 1.4.4 Other Ethnic Groups subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Other Ethnic Groups") ))
pO<- ggplot(mydata[mydata$six_cat == "Other Ethnic Groups",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
( ppO = pO + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0)) )
 #ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_Other_gdppr.tiff", ppO, width=16 , height=6, units = "in", dpi=600 )

# 1.4.5 Unknown subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Unknown") ))
pU<- ggplot(mydata[mydata$six_cat == "Unknown",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
( ppU = pU + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0)) )
 #ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_Unknown_gdppr.tiff", ppU, width=16 , height=6, units = "in", dpi=600 )

# 1.4.6 White subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("White") ))
pW<- ggplot(mydata[mydata$six_cat == "White",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
( ppW = pW + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0)) )
 #ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_White_gdppr.tiff", ppW, width=16 , height=8, units = "in", dpi=600 )


#Print all together: ppB, ppM, ppU, ppA, ppO, ppW,
#	ppB; ppM; ppU;	
#	ppA;ppO;ppW; 
library(ggpubr)

ggarrange( ppA,ppB,ppM,ppO, ppW,ppU,
          labels = c("A", "B", "C", "D","E", "F" ),
          ncol = 1, nrow = 6, align = "v")

#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_all_gdppr.tiff", width=18 , height=40, units = "in", dpi=600 )



#--------------------------------------------------------------------------------------------------#
# 1.7 Barplots: select the 3 most frequent SNOMED codes in each Primary Code letter
#--------------------------------------------------------------------------------------------------#
mydata <- all_data
mydata = mydata[mydata$n != 0,]

#Order by PrimaryCode and then Freq
data <- mydata[order(mydata$PrimaryCode),]

mydata=data.frame(data[0,])
for (i in 1:length(pAZ_list)){ temp = data[data$PrimaryCode == pAZ_list[i], ]
					mydata = rbind(mydata,temp[c(1:3),])
			    }
mydata$SNOMED_ord = factor(mydata$SNOMED_conceptId_description, levels=c(mydata$SNOMED_conceptId_description))

PrimaryCode_concept = data.frame(PrimaryCode = pAZ_list,Concept = c("British (White)", "Irish (White)", "Any other White background", "White and Black Caribbean (Mixed)", "White and Black African (Mixed)", "White and Asian (Mixed)", 
				 "Any other Mixed background", "Indian (Asian or Asian British)", "Pakistani (Asian or Asian British)", "Bangladeshi (Asian or Asian British)", "Any other Asian background", "Caribbean (Black or Black British)",
				 "African (Black or Black British)", "Any other Black background", "Chinese (other ethnic group)", "Any other ethnic group", "Gypsy or Irish Traveller", "Arab", "Not stated" ))

# 1.7.1 Asian subset
#x_legend <- (paste(PrimaryCode_concept$PrimaryCode[PrimaryCode_concept$PrimaryCode %in% A_char], PrimaryCode_concept$Concept[PrimaryCode_concept$PrimaryCode %in% A_char] ))
my_title <- expression(paste("Top 3 SNOMED codes in each Primary Code category within ", italic("Asian/Asian British") ))
pA<- ggplot(mydata[mydata$six_cat == "Asian or Asian British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppA = pA + labs(title=my_title, x="Frequency of individuals (%) in GDPPR", y = "SNOMED codes")+ theme(plot.title = element_text(vjust = 0)) 
( ppA2 = ppA + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2 ,rep(0,11))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top3_SNOMED_Asian_gdppr.tiff", ppA2, width=16 , height=8, units = "in", dpi=600 )

# 1.7.2 Black subset
#x_legend <- (paste(PrimaryCode_concept$PrimaryCode[PrimaryCode_concept$PrimaryCode %in% B_char], PrimaryCode_concept$Concept[PrimaryCode_concept$PrimaryCode %in% B_char] ))
my_title <- expression(paste("Top 3 SNOMED codes in each Primary Code category within ", italic("Black/Black British") ))
pB<- ggplot(mydata[mydata$six_cat == "Black or Black British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppB = pB + labs(title=my_title, x="Frequency of individuals (%) in GDPPR", y = "SNOMED codes")+ theme(plot.title = element_text(vjust = 0)) 
( ppB2 = ppB + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(rep(0,3),1.2,rep(0,5))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top3_SNOMED_Black_gdppr.tiff", ppB2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.3 Mixed subset
my_title <- expression(paste("Top 3 SNOMED codes in each Primary Code category within ", italic("Mixed") ))
pM<- ggplot(mydata[mydata$six_cat == "Mixed",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppM = pM + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0))
( ppM2 = ppM + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(rep(0,9),1.2,rep(0,2))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top3_SNOMED_Mixed_gdppr.tiff", ppM2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.4 Other Ethnic Groups subset
my_title <- expression(paste("Top 3 SNOMED codes in each Primary Code category within ", italic("Other Ethnic Groups") ))
pO<- ggplot(mydata[mydata$six_cat == "Other Ethnic Groups",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppO = pO + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0)) 
( ppO2 = ppO + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2,rep(0,2),1.2,rep(0,8))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top3_SNOMED_Other_gdppr.tiff", ppO2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.5 Unknown subset
my_title <- expression(paste("Top 3 SNOMED codes in each Primary Code category within ", italic("Unknown") ))
pU<- ggplot(mydata[mydata$six_cat == "Unknown",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppU = pU + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0))
( ppU2 = ppU + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2,rep(0,2))) )
 #ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top3_SNOMED_Unknown_gdppr.tiff", ppU2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.6 White subset
my_title <- expression(paste("Top 3 SNOMED codes in each Primary Code category within ", italic("White") ))
pW<- ggplot(mydata[mydata$six_cat == "White",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppW = pW + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0))
( ppW2 = ppW + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2,rep(0,8))) )
 #ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top3_SNOMED_White_gdppr.tiff", ppW2, width=16 , height=8, units = "in", dpi=600 )




#--------------------------------------------------------------------------------------------------#
# 1.8 Barplots: select the 5 most frequent SNOMED codes in each Primary Code letter
#--------------------------------------------------------------------------------------------------#
mydata <- all_data
mydata = mydata[mydata$n != 0,]

#Order by PrimaryCode and then Freq
data <- mydata[order(mydata$PrimaryCode),]

mydata=data.frame(data[0,])
for (i in 1:length(pAZ_list)){ temp = data[data$PrimaryCode == pAZ_list[i], ]
					mydata = rbind(mydata,temp[c(1:5),])
			    }
mydata$SNOMED_ord = factor(mydata$SNOMED_conceptId_description, levels=c(mydata$SNOMED_conceptId_description))

PrimaryCode_concept = data.frame(PrimaryCode = pAZ_list,Concept = c("British (White)", "Irish (White)", "Any other White background", "White and Black Caribbean (Mixed)", "White and Black African (Mixed)", "White and Asian (Mixed)", 
				 "Any other Mixed background", "Indian (Asian or Asian British)", "Pakistani (Asian or Asian British)", "Bangladeshi (Asian or Asian British)", "Any other Asian background", "Caribbean (Black or Black British)",
				 "African (Black or Black British)", "Any other Black background", "Chinese (other ethnic group)", "Any other ethnic group", "Gypsy or Irish Traveller", "Arab", "Not stated" ))

mydata = mydata[!is.na(mydata$n),] #Categories with lesser that 5 groups included NA lines. We remove them here.


# 1.7.1 Asian subset
#x_legend <- (paste(PrimaryCode_concept$PrimaryCode[PrimaryCode_concept$PrimaryCode %in% A_char], PrimaryCode_concept$Concept[PrimaryCode_concept$PrimaryCode %in% A_char] ))
my_title <- expression(paste("Top 5 SNOMED codes in each Primary Code category within ", italic("Asian/Asian British") ))
pA<- ggplot(mydata[mydata$six_cat == "Asian or Asian British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppA = pA + labs(title=my_title, x="Frequency of individuals (%) in GDPPR", y = "SNOMED codes")+ theme(plot.title = element_text(vjust = 0)) 
( ppA2 = ppA + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2 ,rep(0,19))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top5_SNOMED_Asian_gdppr.tiff", ppA2, width=16 , height=8, units = "in", dpi=600 )

# 1.7.2 Black subset
#x_legend <- (paste(PrimaryCode_concept$PrimaryCode[PrimaryCode_concept$PrimaryCode %in% B_char], PrimaryCode_concept$Concept[PrimaryCode_concept$PrimaryCode %in% B_char] ))
my_title <- expression(paste("Top 5 SNOMED codes in each Primary Code category within ", italic("Black/African/Caribbean/Black British") ))
pB<- ggplot(mydata[mydata$six_cat == "Black or Black British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppB = pB + labs(title=my_title, x="Frequency of individuals (%) in GDPPR", y = "SNOMED codes")+ theme(plot.title = element_text(vjust = 0)) 
( ppB2 = ppB + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(rep(0,5),1.2,rep(0,9))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top5_SNOMED_Black_gdppr.tiff", ppB2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.3 Mixed subset
my_title <- expression(paste("Top 5 SNOMED codes in each Primary Code category within ", italic("Mixed") ))
pM<- ggplot(mydata[mydata$six_cat == "Mixed",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppM = pM + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0))
( ppM2 = ppM + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(rep(0,13),1.2,rep(0,4))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top5_SNOMED_Mixed_gdppr.tiff", ppM2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.4 Other Ethnic Groups subset
my_title <- expression(paste("Top 5 SNOMED codes in each Primary Code category within ", italic("Other Ethnic Groups") ))
pO<- ggplot(mydata[mydata$six_cat == "Other Ethnic Groups",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppO = pO + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0)) 
( ppO2 = ppO + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2,rep(0,4),1.2,rep(0,14))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top5_SNOMED_Other_gdppr.tiff", ppO2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.5 Unknown subset
my_title <- expression(paste("Top 5 SNOMED codes in each Primary Code category within ", italic("Unknown") ))
pU<- ggplot(mydata[mydata$six_cat == "Unknown",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppU = pU + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0))
( ppU2 = ppU + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2,rep(0,4))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top5_SNOMED_Unknown_gdppr.tiff", ppU2, width=16 , height=6, units = "in", dpi=600 )

# 1.7.6 White subset
my_title <- expression(paste("Top 5 SNOMED codes in each Primary Code category within ", italic("White") ))
pW<- ggplot(mydata[mydata$six_cat == "White",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
ppW = pW + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED concepts") + theme(plot.title = element_text(vjust = 0))
( ppW2 = ppW + aes(label = paste("N=", n), fontface = "bold") + geom_col() + geom_text(hjust = c(1.2,rep(0,14))) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top5_SNOMED_White_gdppr.tiff", ppW2, width=16 , height=8, units = "in", dpi=600 )


#Print all together: ppB, ppM, ppU, ppA, ppO, ppW,
library(ggpubr)

arranged = ggarrange( ppA2,ppB2,ppM2,ppO2, ppW2,ppU2,
          labels = c("A", "B", "C", "D","E", "F" ),
          ncol = 1, nrow = 6, align = "v")

arranged

#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/Top5_SNOMED_gdppr_with_n.tiff", arranged, width=18 , height=40, units = "in", dpi=600 )











