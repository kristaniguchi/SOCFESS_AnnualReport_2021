#SOC FESS Level 2: Biologically-relevant flow alteration assessment for all subbasins modeled in LSCP (Laguna Canyon, Aliso Creek, Oso Creek, Salt Creek, Horno Creek, Prima Deshecha Creek, and Segunda Deshecha Creek)
#This code loops takes the alteration assessment summary table and generates alteration maps for each FFM, synthesis alteration across flow components
#Source code for Figure 12 in main text of progress report SOC FESS

#Install packages - only need to install once
#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
#install.packages("ggspatial")

#to install spDataLarge
#devtools::install_github("robinlovelace/geocompr")
library(spData)
#install.packages("spDataLarge")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

#load libaries
library(spDataLarge)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(ggspatial)
library(mapview)
library(spData)      
library(spDataLarge)
library(geosphere)
library(rgeos)

#################################################
#set up directories and load in data

#directory where Level 2 data is saved 
level2.dir <- "C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/"
#fname <- paste0(level2.dir, "09_metric_suitability_tally_condensed.csv") #update to directory
fname <- paste0(level2.dir, "09_metric_suitability_tally_condensed_aliso_oso_small_creeks.csv") #all lspc subbasins

#read in level 2 synthesis summary data - csv saved at https://ocgov.box.com/s/sgezzvcrwn5fu4kh6uyumgzlu33ynfvb (SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary.csv)
synthesis.summary <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/AnnualReport_2020/Flow Ecology Level 2/Overall Prioritization (Combining CSCI and ASCI)/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary.csv")

#read in a reformatted level 2 synthesis summary data used for CSCI/ASCI plots - saved in GitHub repository
subset <- read.csv("C:/Users/KristineT/Documents/Git/SOCFESS_2021AnnualReport/summary_bioflowalteartion.csci.asci.csv")

#set output directory where maps and data are saved in [update to your local directory where you want outputs to be saved]
out.dir <- paste0(level2.dir, "Suitability_Maps/", "prob50_75time/")

#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile - read in the subbasin shapefile saved here: https://ocgov.box.com/s/7likwoezsqrmnwfqd7rs24uu5mrdyitj, update path where you save shapefiles
basins <- st_read("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/AnnualReport_2020/Flow Ecology Level 1/Subbasin Shapefiles/Subbasin Boundaries/Subbasins_Boundaries_Source.shp", quiet = T)
#add in matching column to join with component alteration
basins$New_Name <- basins$Subbasin

#reach polylines shapefile can be downloaded here: https://ocgov.box.com/s/rni13od1uai7r351xrt1qbp13wg0sd5o (Reach_Shapefile.zip), unzip and save on local computer, update directory below
reaches <- st_read('L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/New_Subbasins_forSCCWRP/reaches_forSCCWRP.shp', quiet = T)

#read in model source LSPC or Wildermuth - saved in GitHub repository
source <- read.csv("C:/Users/KristineT/Documents/Git/SOCFESS_2021AnnualReport/Subbasins_inmodel_source.csv")

############################################################
#Biologically-Relevant Flow Alteration Maps for CSCI and ASCI 
#Code to produce maps for CSCI (https://ocgov.box.com/s/tml6irenozlqiwc8aw73z4bjf65jw43f) and ASCI (https://ocgov.box.com/s/175a0rqvknhn11g7av3iyrj2jqioga37 )
#Suitability maps for 50% probability of achieving healthy bio threshold, for 75% of time


#colors for suitability categories for map
colors <- c("#ca0020", "#0571b0", "white")
alteration <- c("Altered",  "Unaltered", NA)
categories <- c("Likely Altered", "Likely Unaltered", "Not evaluated")
lookup <- data.frame(cbind(colors, alteration, categories))

#loop through synthesis summary table and create plots for CSCI and ASCI
indices <- c("ASCI", "CSCI")

for(z in indices){
  #subset either csci or asci
  subset.index <- subset[subset$Biol == z,]
  
  #merge with basins
  subset.join <- subset.index %>% 
    full_join(basins, by = c('New_Name'))
  
  #title
  title <- paste0(z)
  subtitle <- "Biologically-Relevant Flow Alteration"
  
  #plot
  #Set up base map 
  study <- ggplot(basins) + 
    geom_sf(color = "lightgrey", fill="white") +
    labs(title=title, subtitle = subtitle, x ="", y = "")  + 
    annotation_scale() +
    annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                           width = unit(.8, "cm")) +
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          plot.title = element_text(size=20)) 
  study
  
  #subset lookup categories and tables
  lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration...Biology`),]
  
  #save as factor for legend ordering
  lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
  subset.join$`Alteration...Biology` <- factor(subset.join$`Alteration...Biology`, levels = unique(lookup.sub$alteration))
  
  
  #synthesis map for CSCI or ASCI
  syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=`Alteration...Biology`, geometry = geometry)) +
    scale_fill_manual(name = "Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  #print
  print(syn.plot)
  
  #write plot
  out.filename <- paste0(out.dir, z, "_alteration_map_Aliso_Oso_small_creeks.jpg")
  ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
  
}
  


############################################################
#Figure 12 in main text
#create overall prioritization maps combining CSCI and ASCI
#prioritization for additional analysis


#colors for suitability categories
colors <- c("#ca0020", "#fdae61","#0571b0", "white")
priority <- c("High Priority",  "Medium Priority","Low Priority", NA)
categories <- c("High (Alteration: CSCI & ASCI)", "Medium (Alteration: CSCI or ASCI)","Low (Alteration: None)","Not evaluated")
lookup <- data.frame(cbind(colors, priority, categories))

#merge with basins
subset.join <- synthesis.summary %>% 
  full_join(basins, by = c('New_Name')) 

source2 <- synthesis.summary %>% 
  inner_join(basins, by = c('New_Name')) 


  #plot
  #Set up base map 
  study <- ggplot(basins) + 
    labs(title="Prioritization for Additional Analysis", subtitle = "Based on Biologically-Relevant Flow Alteration",x ="", y = "")  + 
    geom_sf(color = "lightgrey", fill="white") +
    annotation_scale() +
    annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                           width = unit(.8, "cm")) +
    #labs(x ="", y = "")  + 
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8)) 
  study
  
  #subset lookup categories and tables
  lookup.sub <- lookup[lookup$priority %in% unique(subset.join$synthesis_alteration),]
  
  #save as factor
  lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
  subset.join$synthesis_alteration <- factor(subset.join$synthesis_alteration, levels = unique(lookup.sub$priority))
  
  #synthesis map
  syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=synthesis_alteration, geometry = geometry)) +
    scale_fill_manual(name = "Priority based on Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  
  #add in model source
  syn.plot2 <- syn.plot + geom_sf(data = source2, size = 1, fill = NA, aes(color=Source, geometry = geometry)) +
    scale_color_manual(name = "Model Source", labels = c("LSPC", "GSFLOW"), values=c("black", "hotpink")) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  
  #print
  print(syn.plot)
  print(syn.plot2)
  
  #write plot
  out.filename <- paste0(out.dir, "Synthesis_Prioritization_map_Aliso_Oso_small_creeks.jpg")
  ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)
  




