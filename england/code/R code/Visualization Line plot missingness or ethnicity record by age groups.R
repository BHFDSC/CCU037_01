# Libraries
library(ggplot2)
library(dplyr)
#library(hrbrthemes) #  theme_ipsum() 

#Load data
  age_group = rep(c("0-17","18-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),2)
  data = c(rep("NA/Z",9),rep("Ethnicity record",9))
  n = c(251841,533559,460232,354504,326937,231314,116649,42427,13231,11268347,8673174,8667166,7659766,7878128,6407445,5160223,2843037,922590)
  freq = c(10.8,22.9,19.7,15.2,14,9.9,5,1.8,0.6,18.9,14.6,14.6,12.9,13.2,10.8,8.7,4.8,1.6)
mydata=data.frame(factor(data),factor(age_group),freq)

#Number of individuals
mydata %>%
  ggplot( aes(x=age_group, y=n, group=data, color=data)) +
    geom_line()+
    geom_point(shape=16, color="grey", size=2) +
  ggtitle("Ethnicity records and missingness in GDPPR by age groups") + 
  xlab("Age groups")+ ylab("Number of individuals") +
  theme_ipsum() 

#Frequency of individuals (absolute values)
p1 = mydata %>%
	  ggplot( aes(x=age_group, y=freq, group=data, color=data)) +
	    geom_line(size = 1.15)+
	    geom_point(shape=16, color="grey", size=5) +
	  ggtitle("Ethnicity records and missingness in GDPPR by age groups") + 
	  xlab("Age groups")+ ylab("Frequency of individuals (%)") +
	  theme_bw()

p2 = p1 + theme(plot.title = element_text(size = 18, face = "bold")) +
	 theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="italic"))

p2 + scale_color_manual(values = c("#1b98e0", "grey35")) #blue #1b98e0 ¦and¦ black #353436

#color=c("#69b3a2","red") #type of green-blue-ish



#Frequency of individuals (ratio absolute values)
  age_group = c("0-17","18-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
  freq_NA = c(10.8,22.9,19.7,15.2,14,9.9,5,1.8,0.6)
  freq_Eth = c(18.9,14.6,14.6,12.9,13.2,10.8,8.7,4.8,1.6)
  n_NA  = c(251841,533559,460232,354504,326937,231314,116649,42427,13231)
  n_Eth = c(11268347,8673174,8667166,7659766,7878128,6407445,5160223,2843037,922590)

mydata2=data.frame(factor(age_group),freq_NA,freq_Eth, n_NA, n_Eth)

mydata2$ratio = mydata2$freq_NA/mydata2$freq_Eth
mydata2$no_eth_percentage = 100*n_NA/(n_NA + n_Eth)

p1 = mydata2 %>%
	  ggplot( aes(x=age_group, y=no_eth_percentage, group=1)) +
	    geom_line(size = 1.15, color = "grey35")+
	    geom_point(shape=16, color="grey", size=5) +
	  ggtitle("Percentage of individuals with NA/Z records in GDPPR by age groups") + 
	  xlab("Age groups")+ ylab("Percentage of missingness in NA/Z (%)") +
	  theme_bw()
p2 = p1 + theme(plot.title = element_text(size = 18, face = "bold")) +
	 theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="italic"))
p2



p1 = mydata2 %>%
	  ggplot( aes(x=age_group, y=ratio, group=1)) +
	    geom_line(size = 1.15, color = "#69b3a2" )+
	    geom_point(shape=16, color="grey", size=5) +
	  ggtitle("Ratio of missingness in Ethnicity records in GDPPR by age groups") + 
	  xlab("Age groups")+ ylab("Ratio NA/Z vs Ethnicity") +
	  theme_bw()
p2 = p1 + theme(plot.title = element_text(size = 18, face = "bold")) +
	 theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="italic"))
p2


