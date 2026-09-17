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
# 1.  GET VARIABLES FOR THE TREE
# 1.1 First Branch - 6 categories (mydata$six_cat)
#--------------------------------------------------------------------------------------------------#
#table(mydata$PrimaryCode, useNA="always")

#Merge PrimaryCode into 6 categories:
mydata$six_cat <- "NA"
mydata$six_cat <- ifelse(mydata$PrimaryCode == '1'|mydata$PrimaryCode == '2'|mydata$PrimaryCode == '3'|mydata$PrimaryCode == 'N'|mydata$PrimaryCode == 'M'|mydata$PrimaryCode == 'P', "Black or Black British", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == '0'|mydata$PrimaryCode == 'A'|mydata$PrimaryCode == 'B'|mydata$PrimaryCode == 'C', "White", mydata$six_cat) 
mydata$six_cat <- ifelse(mydata$PrimaryCode == '4'|mydata$PrimaryCode == '5'|mydata$PrimaryCode == '6'|mydata$PrimaryCode == 'L'|mydata$PrimaryCode == 'K'|mydata$PrimaryCode == 'J'|mydata$PrimaryCode == 'H',"Asian or Asian British", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == '7'|mydata$PrimaryCode == '8'|mydata$PrimaryCode == 'W'|mydata$PrimaryCode == 'T'|mydata$PrimaryCode == 'S'|mydata$PrimaryCode == 'R',"Other Ethnic Groups", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == 'D'|mydata$PrimaryCode == 'E'|mydata$PrimaryCode == 'F'|mydata$PrimaryCode == 'G',"Mixed", mydata$six_cat)
mydata$six_cat <- ifelse(mydata$PrimaryCode == '9'|mydata$PrimaryCode == '99'|mydata$PrimaryCode == 'Z'|mydata$PrimaryCode == 'X',"Unknown",mydata$six_cat)

#table(mydata$six_cat,useNA="always" )

all_patients = sum(mydata$n_id_distinct)

#Obtain the frequency of each of the 6 categories vs. all GDPPR patients: mydata$p6_all 
mydata$p6_all[mydata$six_cat == "Asian or Asian British"] <- sum(mydata$n_id_distinct[mydata$six_cat == "Asian or Asian British"]) / all_patients
mydata$p6_all[mydata$six_cat == "Black or Black British"] <- sum(mydata$n_id_distinct[mydata$six_cat == "Black or Black British"]) / all_patients
mydata$p6_all[mydata$six_cat == "Mixed"]			<- sum(mydata$n_id_distinct[mydata$six_cat == "Mixed"])  / all_patients
mydata$p6_all[mydata$six_cat == "Other Ethnic Groups"] 	<- sum(mydata$n_id_distinct[mydata$six_cat == "Other Ethnic Groups"])  / all_patients
mydata$p6_all[mydata$six_cat == "Unknown"] 			<- sum(mydata$n_id_distinct[mydata$six_cat == "Unknown"])  / all_patients
mydata$p6_all[mydata$six_cat == "White"] 			<- sum(mydata$n_id_distinct[mydata$six_cat == "White"])  / all_patients

#table(mydata$p6_all)

#--------------------------------------------------------------------------------------------------#
# 1.2 Second Branch - A-Z categories (mydata$PrimaryCode)
#--------------------------------------------------------------------------------------------------#
# "0","1","2","3","4","5","6","7","8","9","99","X" is not used (we have seen before, to fix the all cat plot, we modify from here.
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

# Get frequence of A-Z vs all_patients in GDPPR: pAZ_all
# "0","1","2","3","4","5","6","7","8","9","99","X" is not used (we have seen before, to fix the all cat plot, we modify from here.
# 'See "0.0 Ethnicity Tree - Copy before claning.R" section/run "# 2C. BUILD THE TREE (using package )" to check that before this modifications)  
# pAZ_list <- c("0","1","2","3","4","5","6","7","8","9","99","A","B","C","D","E","F","G","H","J","K","L","M","N","P","R","S","T","W","Z","X") 
  pAZ_list <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","R","S","T","W","Z") #

for (i in 1:length(pAZ_list)){mydata$pAZ_all[mydata$PrimaryCode == pAZ_list[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == pAZ_list[i]]) / all_patients}

# table(mydata$pAZ_all)
# Manually check
	# sum(mydata$n_id_distinct[mydata$PrimaryCode  == "A"]) / all_patients
	# (mydata$pAZ_all[mydata$PrimaryCode == "A"])[1]


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

# table(mydata$pAZ_6)
# Manually check
	# sum(mydata$n_id_distinct[mydata$PrimaryCode == 'L'])/ A_sum
	# mydata$pAZ_6[mydata$PrimaryCode == 'L'][1]


#--------------------------------------------------------------------------------------------------#
# 1.3 Third Branch - Snomed categories (mydata$SNOMED_conceptId_description)
#--------------------------------------------------------------------------------------------------#
# Get frequence of Snomed codes vs all_patients in GDPPR: snomed_all
#mydata$snomed_all <- mydata$n_id_distinct/all_patients
mydata$snomed_all <- (mydata$n_id_distinct/all_patients) *100 #%


# Get frequence of Snomed codes vs A-Z in GDPPR: snomed_pAZ
#MODIFY:#for (i in 1:length(pAZ_list)){mydata$pAZ_all[mydata$PrimaryCode == pAZ_list[i]] <- sum(mydata$n_id_distinct[mydata$PrimaryCode == pAZ_list[i]]) / all_patients}

all_data <- mydata

#--------------------------------------------------------------------------------------------------#
# 1.4 Barplots by six_cat (remove those whose SNOMED$n = 0)
#--------------------------------------------------------------------------------------------------#
mydata <- all_data
mydata = mydata[mydata$n != 0,]
mydata$SNOMED_ord = factor(mydata$SNOMED_conceptId_description, levels=c(mydata$SNOMED_conceptId_description))

#Templetes (e.g. using Asian subgroup):
	#Manual 100% and single colour in the bars
	#p<- ggplot(mydata[mydata$six_cat == "Asian or Asian British",], aes(x=snomed_all , y=SNOMED_ord))+
	#     geom_bar(stat="identity", width=0.7, fill="steelblue")+
	#     theme_minimal()
	#p + labs(title="SNOMED codes with a minimum of one individual within the Asian or Asian British", x="Frequency individuals (%)", y = "SNOMED codes")

	#Title with Italics colour in the bars
	#my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Asian/Asian British") ))
	#pA<- ggplot(mydata[mydata$six_cat == "Asian or Asian British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	#     geom_bar(stat="identity", width=0.7)+
	#     theme_minimal()
	#pA + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED codes")+ theme(plot.title = element_text(vjust = 0))

# 1.4.1 Asian subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Asian/Asian British") ))
pA<- ggplot(mydata[mydata$six_cat == "Asian or Asian British",], aes(x=snomed_all , y=SNOMED_ord, fill=PrimaryCode))+
	     geom_bar(stat="identity", width=0.7)+
	     theme_minimal()
( ppA = pA + labs(title=my_title, x="Frequency individuals (%)", y = "SNOMED codes")+ theme(plot.title = element_text(vjust = 0)) )
#ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_Asian_gdppr.tiff", ppA, width=16 , height=8, units = "in", dpi=600 )

# 1.4.2 Black subset
my_title <- expression(paste("SNOMED codes with a minimum of one individual within the ", italic("Black/Black British") ))
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

ggsave("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/Bar Plots/CodesFreq_all_gdppr.tiff", width=18 , height=40, units = "in", dpi=600 )


#--------------------------------------------------------------------------------------------------#
# 1.5 Exclude anny code whose frequenec is lower than 0.1%
#--------------------------------------------------------------------------------------------------#
mydata <- all_data

dim(mydata[mydata$snomed_all>0.1,])


mydata = mydata[mydata$snomed_all>0.1,]
table(mydata$PrimaryCode)
table(mydata$SNOMED,mydata$PrimaryCode)

#--------------------------------------------------------------------------------------------------#
# 1.6 Barplot (%) of those whose frequenecy is over 0.1%
#--------------------------------------------------------------------------------------------------#
mydata$SNOMED_ord = factor(mydata$SNOMED_conceptId_description, levels=c(mydata$SNOMED_conceptId_description))
p<- ggplot(mydata, aes(x=snomed_all , y=SNOMED_ord))+
	     geom_bar(stat="identity", width=0.7, fill="steelblue")+
	     theme_minimal()
p + labs(title="SNOMED codes that have a minimum of 1% of individuals", x="Frequency individuals (%)", y = "SNOMED codes")


results = aggregate(x = mydata$snomed_all,   	# Specify data column
          by = list(mydata$PrimaryCode),  # Specify group indicator
          FUN = sum)



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
# 2.  BUILD THE TREE
#
# 2.1   Using the package treemap
# 2.1.1 Defining a pathString:
#       The pathString describes the hierarchy by defining a path from the root to each leaf. 
#       (not necessary, but might help to mentally visualize )
#--------------------------------------------------------------------------------------------------#
mydata$pathString <- paste("All GDPPR patients", 
                            mydata$six_cat, 
                            mydata$PrimaryCode, 
                            mydata$SNOMED_conceptId_description,
                            sep = "/")

#--------------------------------------------------------------------------------------------------#
# 2B. BUILD THE TREE (using package collapsibleTree)
# 2.1 Interatcive, colours, n and freq.
#--------------------------------------------------------------------------------------------------#
GDPPR<-mydata
#Blue and simple (only SNOMEd concepts, no n or freq)
collapsibleTree(GDPPR, c("six_cat", "PrimaryCode", "SNOMED_conceptId_description"), collapsed = TRUE)

#Coloured, interactive and n and freq
# library(stringr)
GDPPR$six_cat_f     <- str_c(GDPPR$six_cat,    ":\n freq(",round(GDPPR$p6_all *100,2),"%)")
GDPPR$PrimaryCode_f <- str_c(GDPPR$PrimaryCode,":\nAll freq(",round(GDPPR$pAZ_all *100,2), "%);\nA-Z freq(",round(GDPPR$pAZ_6 *100,2),"%)")
GDPPR$n_dist        <- str_c("n = ",GDPPR$n_id_distinct, "; freq(",round(GDPPR$snomed_all,6),"%)")

  #' All listed categories (even without cases/patients assigned)
  collapsibleTreeSummary(GDPPR,  c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"),height='900px',width='1500px', root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,  zoomable = TRUE) # zoomable = TRUE is default
        # Select only per six_cat (better visualization):
	    collapsibleTreeSummary(GDPPR[GDPPR$six_cat=="Asian or Asian British",],   c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"),height='900px',width='1500px', root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,  zoomable = TRUE) # zoomable = TRUE is default
	    collapsibleTreeSummary(GDPPR[GDPPR$six_cat=="Black or Black British",],   c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"),height='900px',width='1500px', root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,  zoomable = TRUE) # zoomable = TRUE is default
	    collapsibleTreeSummary(GDPPR[GDPPR$six_cat=="Mixed",],  			c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"),height='900px',width='1500px', root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,  zoomable = TRUE) # zoomable = TRUE is default
	    collapsibleTreeSummary(GDPPR[GDPPR$six_cat=="Other Ethnic Groups",],  	c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"),height='900px',width='1500px', root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,  zoomable = TRUE) # zoomable = TRUE is default
	    collapsibleTreeSummary(GDPPR[GDPPR$six_cat=="Unknown",],  			c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"),height='900px',width='1500px', root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,  zoomable = TRUE) # zoomable = TRUE is default
	    collapsibleTreeSummary(GDPPR[GDPPR$six_cat=="White",],  			c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"),height='900px',width='1500px', root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,  zoomable = TRUE) # zoomable = TRUE is default


  #' Only categories that have cases (at least one patient has been assigned to the snomed code)
  collapsibleTreeSummary(GDPPR[GDPPR$n_id_distinct!=0,],  c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"), height='900px', width='1500px',root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,fontSize = 9)
        # Select only per six_cat (better visualization):
	    collapsibleTreeSummary(GDPPR[GDPPR$n_id_distinct!=0 & GDPPR$six_cat=="Asian or Asian British",],  c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"), height='900px', width='1500px',root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,fontSize = 9)
	    collapsibleTreeSummary(GDPPR[GDPPR$n_id_distinct!=0 & GDPPR$six_cat=="Black or Black British",],  c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"), height='900px', width='1500px',root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,fontSize = 9)
	    collapsibleTreeSummary(GDPPR[GDPPR$n_id_distinct!=0 & GDPPR$six_cat=="Mixed",],  			c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"), height='900px', width='1500px',root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,fontSize = 9)
	    collapsibleTreeSummary(GDPPR[GDPPR$n_id_distinct!=0 & GDPPR$six_cat=="Other Ethnic Groups",],  	c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"), height='900px', width='1500px',root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,fontSize = 9)
	    collapsibleTreeSummary(GDPPR[GDPPR$n_id_distinct!=0 & GDPPR$six_cat=="Unknown",],  			c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"), height='900px', width='1500px',root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,fontSize = 9)
	    collapsibleTreeSummary(GDPPR[GDPPR$n_id_distinct!=0 & GDPPR$six_cat=="White",],  			c("six_cat_f", "PrimaryCode_f", "SNOMED_conceptId_description","n_dist"), height='900px', width='1500px',root = deparse(substitute("GDPPR n=51,135,903")), attribute = "n",collapsed = TRUE,fontSize = 9)


#--------------------------------------------------------------------------------------------------#
# 2C. BUILD THE TREE (using package ggraph)
#    Start by creating a dataset and a graph object using the igraph package
#--------------------------------------------------------------------------------------------------#
# Libraries
# library(ggraph)
# library(igraph)
# library(tidyverse)
#E.g.# data: edge list
#d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep=""))                    #First Branch:  Level 1 to 2
#d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))     #Second Branch: Level 2 to 3
#edges <- rbind(d1, d2)
#name <- unique(c(as.character(edges$from), as.character(edges$to)))
#vertices <- data.frame(
#        name=name,
#        group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
#        cluster=sample(letters[1:4], length(name), replace=T),
#        value=sample(seq(10,30), length(name), replace=T)
#                       )

#Names 6 categories
six_cat = names(table(mydata$six_cat))
six_cat_lvl2 = c( rep(six_cat[1],length(A_char)),rep(six_cat[2],length(B_char)),
                  rep(six_cat[3],length(M_char)),rep(six_cat[4],length(O_char)),
                  rep(six_cat[5],length(U_char)),rep(six_cat[6],length(W_char)))

#' If we would manually assign the names to the structure, we would do the following:
#	d1 <- data.frame(from="GDPPR", to= six_cat)   
#	d2 <- data.frame(from=six_cat_lvl2, to= c(A_char, B_char, M_char, O_char, U_char, W_char) )   
#	edges <- rbind(d1, d2)

#===================-----------------===========================#
#' 2.1:  all available SNOMED CODES					    #
#' 2.1.1 First we make the structure of the tree:		    #
#===============================================================#
d1 <- data.frame(from="origin", to=paste("group", seq(1,length(six_cat)), sep=""))    

        list_char = list(A_char, B_char, M_char, O_char, U_char, W_char)
        c_char = c(A_char, B_char, M_char, O_char, U_char, W_char)
        TEMP2 <- c()
        for (i in 1:(length(d1$to))){TEMP <- rep(d1$to[i], each = length(list_char[[i]])); TEMP2 = c(TEMP2, TEMP)}
d2 <- data.frame(from=TEMP2,to=paste("subgroup", seq(1,length(c_char)), sep="_"))  

edges <- rbind(d1, d2)
name <- unique(c(as.character(edges$from), as.character(edges$to)))

        PC_incl <- names(table(mydata$PrimaryCode))
        TEMP <- data.frame(name=name, group=c("GDPPR", six_cat,c_char))
        TEMP <- TEMP[TEMP$group %in% PC_incl,]
        # length(mydata$SNOMED_conceptId_description[mydata$PrimaryCode %in% c(TEMP$group)]) # Of course, all Snomed concepts are linked to the A-Z [not 0-9,99].
        TEMP3 <- c()
        for (i in 1:(length(TEMP$group))){
                TEMP2 <- rep( TEMP$name[i], each = length(mydata$SNOMED_conceptId_description[mydata$PrimaryCode== TEMP$group[i]] ));
                TEMP3 = c(TEMP3, TEMP2)}
d3 <- data.frame(from=TEMP3,to=paste("subgroup2", seq(1,length(mydata$SNOMED_conceptId_description)), sep="_"))  

edges <- rbind(d1, d2, d3)

#=============================================================================================================#
#' 2.1.2 Later we include extra info by creating a data-frame names "vertices" from the rbind object "edges": #
#=============================================================================================================#
#  The variables group and cluster will be prepared before:
   #group = 
   AZ_order = c_char[c_char %in% PC_incl]
   mydata$PrimaryCode_ord <- factor(mydata$PrimaryCode,levels=AZ_order)
   mydata_ord_PrimaryCode <- mydata[order(mydata$PrimaryCode_ord),]
   #cluster = 
   abc <- letters[1:(length(AZ_order))]
   length_abc<- table(mydata_ord_PrimaryCode$PrimaryCode_ord)
   TEMP2 <- c()
   for (i in 1:(length(abc))){
        TEMP  <- rep( abc[i], each = length_abc[i] );
        TEMP2 = c(TEMP2, TEMP)}
#  name = List all origin, group, subgroup, subgroup2;  -> and then create the data-frame "vertices"
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(name=name,
                       group=c("GDPPR", six_cat,c_char,mydata_ord_PrimaryCode$SNOMED_conceptId_description), 
                       cluster=c(rep("aa", c(length(name) - length(TEMP2))), TEMP2 ),
                       value=c( rep(NA, c(length(name) - length(TEMP2))), mydata_ord_PrimaryCode$n_id_distinct ) )
#=============#
#' 2.1.3 PLOT #
#=============#
#PLOT
mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
        geom_node_text(aes(label=vertices$group, filter=leaf, color=cluster),size=2.9, angle=90, hjust=1, nudge_y = -0.01) + #text
        ylim(-4.9, NA) +
        theme(legend.position="none") +
        geom_edge_diagonal() #geom_edge_link() #for straight line

#gsave("C:/Users/martapm/Desktop/temp.tiff", width = 45, height = 10, units = "in" , dpi=780)


#===============================================================#
#' 2.1.4:  SNOMED CODES with minimum one patient + n(freq)	    #
#' Continuation from 2.2 (i.e., using its variables, etc.)	    #
#' !RUN 2.1 FIRST!  							    #
#===============================================================# 
vertices$freq<- round((vertices$value/sum(mydata$n,na.rm=T))*100,5)

 temp   <- str_c(vertices$group, ": ",vertices$value, " (",  round((vertices$value/sum(mydata$n,na.rm=T))*100,3),"%)" )


mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
        geom_node_text(aes(label=temp, filter=leaf, color=cluster),size=2.9, angle=90, hjust=1, nudge_y = -0.01) + #text
        ylim(-7.9, NA) +
        theme(legend.position="none") +
        geom_edge_diagonal() #geom_edge_link() #for straight line

#ggsave("C:/Users/martapm/Desktop/temp_n_freq.tiff", width = 40, height = 10, units = "in" , dpi=780)





#===================-----------------===========================#
#' 2.2:  SNOMED CODES with minimum one patient			    #
#      									    #
#===============================================================#
mydata2 <- mydata[mydata$n_id_distinct!=0,]
PC_incl <- names(table(mydata$PrimaryCode))
c_char = c(A_char, B_char, M_char, O_char, U_char, W_char)
AZ_order = c_char[c_char %in% PC_incl]
c_char = c_char[c_char %in% PC_incl]
mydata2$PrimaryCode_ord <- factor(mydata2$PrimaryCode,levels=AZ_order)
mydata2 <- mydata2[order(mydata2$PrimaryCode_ord),]
#=================================================#
#' 2.2.1 First we make the structure of the tree: #
#=================================================#
six_cat = names(table(mydata2$six_cat))
d1 <- data.frame(from="origin", to=paste("group", seq(1,length(six_cat)), sep=""))    

        list_char = list(A_char[A_char %in% PC_incl], B_char[B_char %in% PC_incl], M_char[M_char %in% PC_incl], O_char[O_char %in% PC_incl], U_char[U_char %in% PC_incl], W_char[W_char %in% PC_incl])
        #c_char = c(A_char, B_char, M_char, O_char, U_char, W_char) #got before and excluding 
        TEMP2 <- c()
for (i in 1:(length(d1$to))){TEMP <- rep(d1$to[i], each = length(list_char[[i]])); TEMP2 = c(TEMP2, TEMP)}
d2 <- data.frame(from=TEMP2,to=paste("subgroup", seq(1,length(c_char)), sep="_"))  

edges <- rbind(d1, d2)
name <- unique(c(as.character(edges$from), as.character(edges$to)))

        TEMP <- data.frame(name=name, group=c("GDPPR", six_cat,c_char))
        TEMP <- TEMP[TEMP$group %in% PC_incl,]
        TEMP3 <- c()
        for (i in 1:(length(TEMP$group))){
                TEMP2 <- rep( TEMP$name[i], each = length(mydata2$SNOMED_conceptId_description[mydata2$PrimaryCode== TEMP$group[i]] ));
                TEMP3 = c(TEMP3, TEMP2)  }
d3 <- data.frame(from=TEMP3,to=paste("subgroup2", seq(1,length(mydata2$SNOMED_conceptId_description)), sep="_"))  
edges <- rbind(d1, d2, d3)
#=============================================================================================================#
#' 2.2.2 Later we include extra info by creating a data-frame names "vertices" from the rbind object "edges": #
#=============================================================================================================#
#' The variables group and cluster will be prepared before:
#'	group =  ##got before##
#'	AZ_order = c_char[c_char %in% PC_incl]       
#'	mydata2$PrimaryCode_ord <- factor(mydata2$PrimaryCode,levels=AZ_order)
#'	mydata_ord_PrimaryCode <- mydata[order(mydata$PrimaryCode_ord),] #not necessary, now 'mydata2'
#cluster = 
 abc <- letters[1:(length(AZ_order))]
 length_abc<- table(mydata2$PrimaryCode_ord)
 TEMP2 <- c()
for (i in 1:(length(abc))){
        TEMP  <- rep( abc[i], each = length_abc[i] );
        TEMP2 = c(TEMP2, TEMP)}
#  name = List all origin, group, subgroup, subgroup2;  -> and then create the data-frame "vertices"
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(name=name,
                       group=c("GDPPR", six_cat,c_char,mydata2$SNOMED_conceptId_description), 
                       cluster=c(rep("aa", c(length(name) - length(TEMP2))), TEMP2 ),
                       value=c( rep(NA, c(length(name) - length(TEMP2))), mydata2$n_id_distinct ) )
#=============#
#' 2.2.3 PLOT #
#=============#
mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
        geom_node_text(aes(label=vertices$group, filter=leaf, color=cluster),size=1.5, angle=90, hjust=1, nudge_y = -0.01) + #text
        ylim(-7.9, NA) +
        theme(legend.position="none") +
        geom_edge_diagonal() #geom_edge_link() #for straight line

ggsave("C:/Users/martapm/Desktop/temp.tiff", width = 40, height = 10, units = "in" , dpi=780)

#===============================================================#
#' 2.2.4:  SNOMED CODES with minimum one patient + n(freq)	    #
#' Continuation from 2.2 (i.e., using its variables, etc.)	    #
#' !RUN 2.2 FIRST!  							    #
#===============================================================# 
vertices$freq<- round((vertices$value/sum(mydata2$n,na.rm=T))*100,5)

 temp   <- str_c(vertices$group, ": ",vertices$value, " (",  round((vertices$value/sum(mydata2$n,na.rm=T))*100,3),"%)" )


mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
        geom_node_text(aes(label=temp, filter=leaf, color=cluster),size=1.5, angle=90, hjust=1, nudge_y = -0.01) + #text
        ylim(-7.9, NA) +
        theme(legend.position="none") +
        geom_edge_diagonal() #geom_edge_link() #for straight line

ggsave("C:/Users/martapm/Desktop/temp3.tiff", width = 40, height = 10, units = "in" , dpi=780)



