# Library
library(networkD3)
library(dplyr)
library(RColorBrewer) #install.packages("RColorBrewer")

#--------------------------------------------------------------------------------------------------#
# Load Data
#--------------------------------------------------------------------------------------------------#
 rm(list = ls())
 setwd("C:") 	#Disc of data location
 # A connection data frame is a list of flows with intensity for each flow
 links <- read.csv("C:/Users/martapm/Desktop/Marta/CVD-COVID-UK [HDRUK]/4. Results/sankey_gdppr_data.csv",fill = TRUE, header = TRUE)
 head(links)

names(links)[c(6,7)]<- c("source","target")
save_links = links

#--------------------------------------------------------------------------------------------------#
# Most basic Sankey Diagram (not linked from snomed to possibility since we dont have the mark)
#--------------------------------------------------------------------------------------------------#
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$Snomed_code),as.character(links$source), 
  as.character(links$target)) %>% unique()
)

#Snomed to curren_mapping (now 'source')
  links_snomed = save_links
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links_snomed$IDsource <- match(links_snomed$Snomed_code, nodes$name)-1 
  links_snomed$IDtarget <- match(links_snomed$source, nodes$name)-1
#curren_mapping (now 'source') to Potentiall_different_mapping (now 'target')
  links_source = save_links
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links_source$IDsource <- match(links_source$source, nodes$name)-1 
  links_source$IDtarget <- match(links_source$target, nodes$name)-1
#Merge all to "links"
links = rbind(links_snomed,links_source)

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal([
"white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white",
"#ff9896","#cedb9c","#17becf","grey50","#9c9ede","#d62728","#a1d99b","#cedb9c","#17becf","yellow","#ff9896","#9ecae1","purple","#31a354","pink","#9edae5","grey50","#d62728"
])'


# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Order", NodeID = "name", colourScale=my_color,
              sinksRight=FALSE)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))

#============================================================================================================================================================================================================================================================#
# COLOR ATTEMPTS:
RUN = "NOT"
if(RUN == "YES"){
my_color <- 'd3.scaleOrdinal() .domain(["EAfricAsian/Indo-Carib(NMO)(ethnicgroup)","BlackEastAfricanAsian/Indo-Caribbean(ethnicgroup)","EastAfricanAsian(NMO)(ethnicgroup)","BlackEastAfricanAsian(ethnicgroup)",
							"BlackIndo-Caribbean(ethnicgroup)","Indo-Caribbean(NMO)(ethnicgroup)","BlackWestIndian(ethnicgroup)","WestIndian(NMO)(ethnicgroup)","Race:Westindian(ethnicgroup)",
							"MixedAsian-ethniccategory2001census(finding)","MixedBlack-ethniccategory2001census(finding)","Black-other,mixed(ethnicgroup)","RomanianRoma(ethnicgroup)","Roma(ethnicgroup)",
							"PolishRoma(ethnicgroup)","AsianandChinese-ethniccategory2001census(finding)","ChineseandWhite-ethniccategory2001census(finding)","Britishethnicminorityspecified(NMO)(ethnicgroup)",
							"NorthAfricanArab(NMO)(ethnicgroup)","Gypsies(ethnicgroup)","Britishethnicminorityunspecified(NMO)(ethnicgroup)","NewZealandEuropean(ethnicgroup)","OtherEuropeaninNewZealand(ethnicgroup)",
							"SlovakRoma(ethnicgroup)","HungarianRoma(ethnicgroup)","CzechRoma(ethnicgroup)","BulgarianRoma(ethnicgroup)",
							"L(OtherAsian)","M(Caribbean)","G(OtherMixed)","T(Gypsy/Traveller)","S(Anyotherethnicgroup)","C(OtherWhite)","P(OtherBlack)","M(Caribbean)","G(OtherMixed)","N(African)",
							"L(OtherAsian)","Roma(census2021)","R(Chinese)","F(WhiteandAsian)","A(British)","W(Arab)","T(Gypsy/Traveller)","C(OtherWhite)"]) 
					.range(["white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white",
"#ff9896","#cedb9c","#17becf","grey50","#9c9ede","#d62728","#a1d99b","#cedb9c","#17becf","yellow","#ff9896","#9ecae1","purple","#31a354","pink","#9edae5","grey50","#d62728"
							])'

#"red20","lime","lightblue","grey50","blue","red70","green","lime","lightblue","yellow","red20","grey30","purple","green20","pink","lightblue40","grey50","red70"])'
#"#ff9896","#cedb9c","#17becf","grey50","#9c9ede","#d62728","#a1d99b","#cedb9c","#17becf","yellow","#ff9896","grey30","purple","#31a354","pink","#9edae5","grey50","#d62728"])'

#"white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white"
}
#============================================================================================================================================================================================================================================================#


#--------------------------------------------------------------------------------------------------#
# Most basic Sankey Diagram (not linked from snomed to possibility since we dont have the mark)
#--------------------------------------------------------------------------------------------------#
links = save_links

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$Snomed_code),as.character(links$source), 
  as.character(links$target)) %>% unique()
)


#Snomed to curren_mapping (now 'source')
  links_snomed = save_links
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links_snomed$IDsource <- match(links_snomed$source, nodes$name)-1 
  links_snomed$IDtarget <- match(links_snomed$Snomed_code, nodes$name)-1
#curren_mapping (now 'source') to Potentiall_different_mapping (now 'target')
  links_source = save_links
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links_source$IDsource <- match(links_source$Snomed_code, nodes$name)-1 
  links_source$IDtarget <- match(links_source$target, nodes$name)-1
#Merge all to "links"
links = rbind(links_snomed,links_source)

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal([
"white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white",
"#ff9896","#cedb9c","#17becf","grey50","#9c9ede","#d62728","#a1d99b","yellow","#17becf","pink","#2ca02c","#9ecae1","purple","#31a354","#bcbd22","#9edae5","grey50","#d62728"
])'


# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget", Value = "value", #"Order",
              NodeID = "name", colourScale=my_color, fontSize=15,fontFamily = "bold",
              sinksRight=TRUE)
p

# save the widget (requires Rstudio)
# library(htmlwidgets)
 saveWidget(p, file=paste0( getwd(), "/sankeyBasic_gdppr.html"))

