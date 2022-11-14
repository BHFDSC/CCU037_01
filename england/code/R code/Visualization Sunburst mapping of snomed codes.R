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
#library(collapsibleTree)

# Library for manaing strings
library(stringr)

# Libraries for vertical tree (non interactive/collapsible) [saved in tiff]
#library(ggraph)
#library(igraph)
library(tidyverse)


#remotes::install_github("timelyportfolio/sunburstR")

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
# 1.0 Sunburst plot - All 255 snomed codes (the ones with at least one individual)
#--------------------------------------------------------------------------------------------------#
mydata <- all_data
mydata <- mydata[c(2,5:7,12)]
mydata <- mydata[mydata$n > 0,]

library(sunburstR); packageVersion("sunburstR")

# From sis_cat to A-Z
mydata$pathString <- paste(mydata$six_cat, mydata$PrimaryCode, sep = "-")
SPC = sunburst(data = data.frame(xtabs(mydata$n~pathString,mydata )), legend = FALSE)
SPCinteractive = sund2b(data = data.frame(xtabs(mydata$n~pathString,mydata )), showLabels = TRUE, width="100%")

# From sis_cat to SNOMED
mydata$SNOMED = str_replace(mydata$SNOMED_conceptId_description, "-", "/")
mydata$SNOMED = str_replace(mydata$SNOMED, " - ", "/")
pathString <- paste(mydata$six_cat, mydata$PrimaryCode,mydata$SNOMED, sep = " - ")
SS = sunburst(data = data.frame(xtabs(mydata$n~pathString,mydata )), legend = TRUE)
SSinteractive <- sund2b(data = data.frame(xtabs(mydata$n~pathString,mydata )), showLabels = TRUE,  width="100%", height="900px") #don't wotk: rootLabel = 'GDPPR',elementId=TRUE,tooltip =  "bottom right" ,
SSinteractive 


#-------------------------------#
# 1.1 Asian codes Sunburst plot #
#-------------------------------#
subdata <- mydata[mydata$six_cat == "Asian or Asian British",]

subdata$SNOMED = str_replace(subdata$SNOMED_conceptId_description, "-", "/")
subdata$SNOMED = str_replace(subdata$SNOMED, " - ", "/")
pathString <- paste(subdata$six_cat, subdata$PrimaryCode,subdata$SNOMED, sep = " - ")
Ainteractive <- sund2b(data = data.frame(xtabs(subdata$n~pathString,subdata )), showLabels = TRUE,  width="100%", height="900px") 
Ainteractive

#-------------------------------#
# 1.2 Black codes Sunburst plot #
#-------------------------------#
subdata <- mydata[mydata$six_cat == "Black or Black British",]

subdata$SNOMED = str_replace(subdata$SNOMED_conceptId_description, "-", "/")
subdata$SNOMED = str_replace(subdata$SNOMED, " - ", "/")
pathString <- paste(subdata$six_cat, subdata$PrimaryCode,subdata$SNOMED, sep = " - ")
Binteractive <- sund2b(data = data.frame(xtabs(subdata$n~pathString,subdata )), showLabels = TRUE,  width="100%", height="900px") 
Binteractive

#-------------------------------#
# 1.3 Mixed codes Sunburst plot #
#-------------------------------#
subdata <- mydata[mydata$six_cat == "Mixed",]

subdata$SNOMED = str_replace(subdata$SNOMED_conceptId_description, "-", "/")
subdata$SNOMED = str_replace(subdata$SNOMED, " - ", "/")
pathString <- paste(subdata$six_cat, subdata$PrimaryCode,subdata$SNOMED, sep = " - ")
Minteractive <- sund2b(data = data.frame(xtabs(subdata$n~pathString,subdata )), showLabels = TRUE,  width="100%", height="900px") 
Minteractive

#-------------------------------#
# 1.4 Other codes Sunburst plot #
#-------------------------------#
subdata <- mydata[mydata$six_cat == "Other Ethnic Groups",]

subdata$SNOMED = str_replace(subdata$SNOMED_conceptId_description, "-", "/")
subdata$SNOMED = str_replace(subdata$SNOMED, " - ", "/")
pathString <- paste(subdata$six_cat, subdata$PrimaryCode,subdata$SNOMED, sep = " - ")
Ointeractive <- sund2b(data = data.frame(xtabs(subdata$n~pathString,subdata )), showLabels = TRUE,  width="100%", height="900px") 
Ointeractive

#---------------------------------#
# 1.5 Unknown codes Sunburst plot #
#---------------------------------#
subdata <- mydata[mydata$six_cat == "Unknown",]

subdata$SNOMED = str_replace(subdata$SNOMED_conceptId_description, "-", "/")
subdata$SNOMED = str_replace(subdata$SNOMED, " - ", "/")
pathString <- paste(subdata$six_cat, subdata$PrimaryCode,subdata$SNOMED, sep = " - ")
Uinteractive <- sund2b(data = data.frame(xtabs(subdata$n~pathString,subdata )), showLabels = TRUE,  width="100%", height="900px") 
Uinteractive

#-------------------------------#
# 1.6 White codes Sunburst plot #
#-------------------------------#
subdata <- mydata[mydata$six_cat == "White",]

subdata$SNOMED = str_replace(subdata$SNOMED_conceptId_description, "-", "/")
subdata$SNOMED = str_replace(subdata$SNOMED, " - ", "/")
pathString <- paste(subdata$six_cat, subdata$PrimaryCode,subdata$SNOMED, sep = " - ")
Winteractive <- sund2b(data = data.frame(xtabs(subdata$n~pathString,subdata )), showLabels = TRUE,  width="100%", height="900px") 
Winteractive



#==================#
#   try to color   #
#==================#
names(table(colour1))
(table(colour1))

colour1 <- paste(mydata$six_cat, mydata$PrimaryCode, sep = "-")
colour1 <- colour1[order(colour1)]

#blue <- c("#d8e7e8","#c9dedf","#b9d5d5","#aacbcc","#9bc2c3","#7cafb1","#6da6a8","#5e9c9e","#558d8f","#4c7e80","#426f70","#305051") #12 Asian or Asian British-H 
#orange <- c("#fee9c1","#ffdda1","#ffd280","#ffc65f","#ffbb3f","#ffaf1e","#ffa706","#ec9900","#d48900") #9 Asian or Asian British-J

colors = c("#6b5b95", "#feb236", "#d64161", "#ff7b25","#aacbcc","#ffdda1")

#19 levels (six_cat to A-Z)
df = data.frame(xtabs(mydata$n~pathString,mydata))
sunburst(data = df, legend = FALSE, color = list(range = colors, domain = names(df)))



######################################
#		Other attempts		 #
######################################

library(plotly)

fig <- plot_ly(
  labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
  values = c(65, 14, 12, 10, 2, 6, 6, 4, 4),
  type = 'sunburst',
  branchvalues = 'total'
)

fig



#Try plotly
library(plotly)

fig <- plot_ly(
  labels = mydata$SNOMED_conceptId_description, parents = mydata$PrimaryCode, values = mydata$n, type = 'sunburst',
  branchvalues = 'total'
)

d <- data.frame(
	    ids = c("North America", "Europe","North America - Football", "Soccer", "North America - Rugby", "1","2","3"
	 ), labels = c("North<br>America","Europe", "Football", "Soccer", "Rugby", "Football2", "Soccer2", "Rugby2"
	 ), parents = c("", "", "North America", "North America", "North America","Europe","Europe", "Europe"
	 ), stringsAsFactors = FALSE)
plot_ly(d, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst')

d <- data.frame(ids = mydata$PrimaryCode, labels = mydata$SNOMED_conceptId_description, parents = mydata$six_cat, stringsAsFactors = FALSE)
#==========================================================================================================================================#
# Sunburst with Repeated Labels by "library(plotly)" #using only the 255 codes
#
# https://plotly.com/r/sunburst-charts/
#==========================================================================================================================================#
#library(data.table)
library(plotly)
#library(sunburstR)

 mydata <- all_data
 mydata <- mydata[c(2,5:7,12)]
 mydata <- mydata[mydata$n > 0,]

 sum_indAZ = aggregate(x = mydata$n,   	# Specify data column
          	 by = list(mydata$PrimaryCode),  # Specify group indicator
          	 FUN = sum)
 names(sum_indAZ) <- c("AZ","n")

# Asian:
head = data.frame(n=c(A_sum, sum_indAZ$n[sum_indAZ$AZ %in% A_char]),IDs = c("Asian or Asian British", A_char[order(A_char)]) , Parents = c("", rep("Asian or Asian British",length(A_char)) ))
subdata <- mydata[mydata$six_cat == "Asian or Asian British",c(1:3)]; names(subdata) = c("n","IDs","Parents")
subdata <- rbind(head, subdata)
subdata$Labels <- str_replace_all(subdata$IDs, " ", "<br>")

figA = plot_ly(subdata, ids = ~IDs, labels = ~Labels, parents = ~Parents,  type = 'sunburst',values = ~n, branchvalues = 'total')

 #specify colors: 
 colors = c("#e6550d", "#9ecae1", "#fdae6b","#111111")
 figA  %>% layout(colorway = ~colors)

# Black:
head = data.frame(n=c(B_sum, sum_indAZ$n[sum_indAZ$AZ %in% B_char]),IDs = c("Black or Black British", B_char) , Parents = c("", rep("Black or Black British",length(B_char)) ))
subdata <- mydata[mydata$six_cat == "Black or Black British",c(1:3)]; names(subdata) = c("n","IDs","Parents")
subdata <- rbind(head, subdata)
subdata$Labels <- str_replace_all(subdata$IDs, " ", "<br>")

figB =  plot_ly(subdata, ids = ~IDs, labels = ~Labels, parents = ~Parents,  type = 'sunburst',values = ~n, branchvalues = 'total')

# Mixed:
head = data.frame(n=c(M_sum, sum_indAZ$n[sum_indAZ$AZ %in% M_char]),IDs = c("Mixed", M_char) , Parents = c("", rep("Mixed",length(M_char)) ))
subdata <- mydata[mydata$six_cat == "Mixed",c(1:3)]; names(subdata) = c("n","IDs","Parents")
subdata <- rbind(head, subdata)
subdata$Labels <- str_replace_all(subdata$IDs, " ", "<br>")

figM =  plot_ly(subdata, ids = ~IDs, labels = ~Labels, parents = ~Parents,  type = 'sunburst',values = ~n, branchvalues = 'total')

# Other:
head = data.frame(n=c(O_sum, sum_indAZ$n[sum_indAZ$AZ %in% O_char]),IDs = c("Other Ethnic Groups", O_char) , Parents = c("", rep("Other Ethnic Groups",length(O_char)) ))
subdata <- mydata[mydata$six_cat == "Other Ethnic Groups",c(1:3)]; names(subdata) = c("n","IDs","Parents")
subdata <- rbind(head, subdata)
subdata$Labels <- str_replace_all(subdata$IDs, " ", "<br>")

figO =  plot_ly(subdata, ids = ~IDs, labels = ~Labels, parents = ~Parents,  type = 'sunburst',values = ~n, branchvalues = 'total')


# White:
head = data.frame(n=c(W_sum, sum_indAZ$n[sum_indAZ$AZ %in% W_char]),IDs = c("White", W_char) , Parents = c("", rep("White",length(W_char)) ))
subdata <- mydata[mydata$six_cat == "White",c(1:3)]; names(subdata) = c("n","IDs","Parents")
subdata <- rbind(head, subdata)
subdata$Labels <- str_replace_all(subdata$IDs, " ", "<br>")

figW =  plot_ly(subdata, ids = ~IDs, labels = ~Labels, parents = ~Parents,  type = 'sunburst',values = ~n, branchvalues = 'total')


# Unknown:
head = data.frame(n=c(U_sum, sum_indAZ$n[sum_indAZ$AZ %in% U_char]),IDs = c("Unknown", U_char) , Parents = c("", rep("Unknown",length(U_char)) ))
subdata <- mydata[mydata$six_cat == "Unknown",c(1:3)]; names(subdata) = c("n","IDs","Parents")
subdata <- rbind(head, subdata)
subdata$Labels <- str_replace_all(subdata$IDs, " ", "<br>")

figU =  plot_ly(subdata, ids = ~IDs, labels = ~Labels, parents = ~Parents,  type = 'sunburst',values = ~n, branchvalues = 'total')

#================================#
# ALL: (figure 2)
#================================#
map_PC_6cat = unique(mydata[c(3,4)])

head = data.frame(   n = c(all_patients, A_sum,B_sum,M_sum,O_sum,U_sum,W_sum, sum_indAZ$n    ),
 			 IDs = c("GDPPR", names(table(mydata$six_cat)), pAZ_list),
		   Parents = c("", rep("GDPPR", length(names(table(mydata$six_cat)))),  rep("", length(sum_indAZ$n)))
			)
head [c(8:26),3] <- left_join(head[c(8:26),], map_PC_6cat, by=c("IDs"="PrimaryCode"))[4]

subdata <- mydata[,c(1:3)]; names(subdata) = c("n","IDs","Parents")
subdata <- rbind(head, subdata)
subdata$Labels <- str_replace_all(subdata$IDs, " ", "<br>")

fig =  plot_ly(subdata, ids = ~IDs, labels = ~Labels, parents = ~Parents,  type = 'sunburst',values = ~n, branchvalues = 'total')
fig



#Try

fig2 = fig <- fig %>%
  add_trace(
    ids = subdata$IDs,
    labels = subdata$Labels,
    parents = subdata$Parents,
    type = 'sunburst',
    maxdepth = 4,
    domain = list(column = 1))

fig2 %>%
  layout(
    grid = list(columns =2, rows = 1),
    margin = list(l = 0, r = 0, b = 0, t = 0),
    sunburstcolorway = c(
      "#636efa","#EF553B","#00cc96","#ab63fa","#19d3f3",
      "#e763fa", "#FECB52","#FFA15A","#FF6692","#B6E880"
    ),
    extendsunburstcolors = TRUE)

