#01_Flow Ecology Level 1 - Alteration Assessment based on current deviation from reference
  #This code evaluates alteration of functional flow metrics (FFM) based on comparison of current FFM to reference ranges
    #and summarizes component alteration

#install libraries - only need to do this once
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("purrr")
#install.packages("plyr")
#install.packages("tidyverse")

#load libraries
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#set the output directory where the datasets will be saved, change to appropriate file path
out.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/"

#Read in FFM annual and percentile data for all subbasins and scenarios
#read in final annual FFM values under current, reference, and future water conservation scenarios
FFM.annual <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/SOC_deltaH_supp_final.csv")
#find unique sites to loop through
sites <- unique(FFM.annual$site)

#read in final FFM percentiles calculated across period of record under each model scenario
FFM.percentiles <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/SOC_percentiles_final.csv")

#Read in the FFM lookup table - saved in git repository
#Functional flow metric names and labels for plots
ffm.labels <- read.csv("./all_metric_def_list_FFMs_v2.csv")
ffm.labels$metric <- ffm.labels$flow_metric
#exclude peak magnitude metrics, will not loop through it as it only has 1 value calculated
ffm.labels <- ffm.labels[ffm.labels$flow_metric != "Peak_2",]
ffm.labels <- ffm.labels[ffm.labels$flow_metric != "Peak_5",]
ffm.labels <- ffm.labels[ffm.labels$flow_metric != "Peak_10",]
#unique FFM to evaluate
ffm.list <- unique(ffm.labels$flow_metric)

####################################################################################
###Alteration Assessment:

##Alteration ruleset:
###Only run alteration assessment if >10 metric values calculated across period of record for current and reference, if <10 values then "Not enough data"
###Likely unaltered: if current median falls within p10-p90 reference range and > 50% of current values fall within reference range
###Likely altered: if current median falls outside of p10-p90 reference range 
###Indeterminate: if current median falls within p10-p90 reference range and < 50% of current values fall within reference range

#create an empty data frame for alteration determination and direction
alteration.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=11))
#set column names of df
names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "comid.notes", "region", "scenario", "dataset", "alteration.notes")


#loop to determine alteration status for current condition and water conservation

for (i in 1:length(sites)){
  #subset annual results to site i
  FFM.annual.sub <- FFM.annual[FFM.annual$site == sites[i],]
  #subset percentiles to site i
  FFM.percentiles.sub <- FFM.percentiles[FFM.percentiles$site == sites[i],]
  
  #empty vectors to be filled in with alteration data
  alteration.status <- NA
  alteration.direction <- NA
  
  #loop through each FFM for evaluation
  for(j in 1:length(ffm.list)){
    #subset annual to FFM j
    FFM.annual.sub.ffm <- FFM.annual.sub[FFM.annual.sub$flow_metric == ffm.list[j],]
    
    #subset percentiles to site i
    FFM.percentiles.sub.ffm <- FFM.percentiles.sub[FFM.percentiles.sub$flow_metric == ffm.list[j],]
    
    #if current or ref has < 10 metric values, then skip alteration assessment and add to alteration notes
    if(FFM.percentiles.sub.ffm$metric_count[FFM.percentiles.sub.ffm$dataset == "current"] < 10 |
       FFM.percentiles.sub.ffm$metric_count[FFM.percentiles.sub.ffm$dataset == "reference"] < 10) {
      #create a new row with alteration category "not enough data" and save into overall df
      out.row <- c("", sites[i], "", ffm.list[j], "Not enough values", "", "", unique(FFM.percentiles.sub.ffm$region), unique(FFM.percentiles.sub.ffm$scenario), "current", "Less than 10 metric values, cannot do assessment")
      #save in output df
      alteration.df.overall <- rbind(alteration.df.overall, out.row)
    }else{
      #else, if >=10 values, then run alteration assessment
      
      #model results and median for ffm i
      model.curr.ffms.i <- FFM.annual.sub.ffm$current_value_final[!is.na(FFM.annual.sub.ffm$current_value_final)]
      model.curr.i.med <- FFM.percentiles.sub.ffm$p50[FFM.percentiles.sub.ffm$dataset == "current"]
      #find 10-90th ref percentiles
      model.ref.i.90 <-  FFM.percentiles.sub.ffm$p90[FFM.percentiles.sub.ffm$dataset == "reference"]
      model.ref.i.10 <-  FFM.percentiles.sub.ffm$p10[FFM.percentiles.sub.ffm$dataset == "reference"]
      
      #if NA values for current or reference ffms, then all are NA
      if(is.na(model.curr.i.med) |  is.na(model.ref.i.90)){
        alteration.status[j] <- NA
        alteration.direction[j] <- NA
      }else{
        #else, if median falls outside of 10-90, likely altered
        if(model.curr.i.med > model.ref.i.90 | model.curr.i.med < model.ref.i.10){
          alteration.status[j] <- "likely_altered"
          #if it is altered determine direction of alteration high or low
          if(model.curr.i.med > model.ref.i.90){
            #direction is high when current median is larger than the ref 90th percentile
            alteration.direction[j] <- "high"
          }else{
            #else, direction will be low
            alteration.direction[j] <- "low"
          }
        }else{
          #if median falls within 10-90th and >50% falls within the range, then indeterminate or <50% falls within range (likely unaltered)
          #since not altered, no alteration direction
          alteration.direction[j] <- "none_found"
          #determine how many values fall in the reference 10-90th range
          count.in.range <- length(which(model.curr.ffms.i <= model.ref.i.90 & model.curr.ffms.i >= model.ref.i.10))
          percent.inrange <- count.in.range/length(na.omit(model.curr.ffms.i))
          #if more than half in the range, likely unaltered
          if(percent.inrange > 0.5){
            alteration.status[j] <- "likely_unaltered"
            #else, if < half is in the range, indeterminate  
          }else{
            alteration.status[j] <- "indeterminate"
          }
        }
      }
      #create a new row with alteration category " and save into overall df
      out.row2 <- c("", sites[i], "", ffm.list[j], alteration.status[j], alteration.direction[j], "", unique(FFM.percentiles.sub.ffm$region), unique(FFM.percentiles.sub.ffm$scenario), "current", "")
      #save in output df
      alteration.df.overall <- rbind(alteration.df.overall, out.row2)
    }
  }
}

#remove the first row of NA values
alteration.df.overall <- alteration.df.overall[2:length(alteration.df.overall$ffm),]

############################################################
#lookup table to convert site for model output to subbasin names, saved in github repository
subbasin_lookup <- read.csv("./site_name_lookupletternumbers.csv")

#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- alteration.df.overall$subbasin.model

for(z in 1:length(new.subbasinname)){
  new.subbasinname.z <- as.character(new.subbasinname[z])
  
  #check first character and replace with
  if(str_sub(new.subbasinname.z, 1,1) == "1"){
    letter <- "I"
  }else{
    if(str_sub(new.subbasinname.z, 1,1) == "2"){
      letter <- "J"
    }else{
      if(str_sub(new.subbasinname.z, 1,1) == "3"){
        letter <- "K"
      }else{
        if(str_sub(new.subbasinname.z, 1,1) == "4"){
          letter <- "L"
        }else{
          if(str_sub(new.subbasinname.z, 1,1) == "5"){
            letter <- "M"
          }
        }
      }
    }
  }
  
  #new name, use first letter, digits 2-3, dash, digits 3-6
  new.subbasinname[z] <- paste0(letter, str_sub(new.subbasinname.z, 2, 3), "-", str_sub(new.subbasinname.z, 4,6))
}

#save subbasin into alteration df
alteration.df.overall$subbasin <- new.subbasinname

#write alteration.df.overall to output directory
write.csv(alteration.df.overall, file=paste0(out.dir, "ffm_alteration_current_all.csv"), row.names = FALSE)


####################################################################################################################################################################################################
#Functional Flow Component Alteration
#Rule: if one metric in component is altered, whole component is considered altered


#find unique sites from alteration DF
unique.sites <- unique(alteration.df.overall$subbasin.model)
#if first site is NA, remove first row
if(is.na(unique.sites[1])){
  alteration.df.overall <- alteration.df.overall[2:length(alteration.df.overall$COMID),]
}
#join the alteration df with the ffm table
ffm.labels$ffm <- as.character(ffm.labels$metric)
alteration.df.overall$ffm <- as.character(alteration.df.overall$ffm)
unique.ffm <- unique(ffm.labels$ffm)
#join to ffm.labels
alteration.df.overall.join <- full_join(alteration.df.overall, ffm.labels, by="ffm")

#write overall alteration FFM to alteration directory
write.csv(alteration.df.overall.join, file=paste0(out.dir, "ffm_alteration_current_all.csv"), row.names=FALSE)

#synthesis alteration
#summarize the number of altered metrics for each subbasin FFM
summary <- aggregate(alteration.df.overall.join, by= alteration.df.overall.join[c('subbasin.model', 'subbasin','flow_component', 'alteration.status')], length) 
unique.sites <- unique(summary$subbasin.model)
unique.components <- unique(summary$flow_component)

#create empty data frame for component alteration to append to
component.alt.df <- data.frame(matrix(data=NA, nrow=1, ncol=4))
#set column names of df
names(component.alt.df) <- c("subbasin.model", "subbasin", "flow_component", "component_alteration")

#loop through summary table to determine if component alteration should be likely altered (1 altered), likely unaltered (all unaltered), indeterminate (all indeterminate), not enough data (not enough data all)

for(a in 1:length(unique.sites)){
  summary.sub <- summary[summary$subbasin.model == unique.sites[a],]
  
  #for every flow component, determine category based on count in each category
  for(b in 1:length(unique.components)){
    summary.sub.comp <- summary.sub[summary.sub$flow_component == unique.components[b],]
    
    #find index of likely altered, indeterminate, likely unaltered, not enough data - to determine length of each
    ind.indet <- grep("indeterminate", summary.sub.comp$alteration.status)
    ind.altered <- grep("likely_altered", summary.sub.comp$alteration.status)
    ind.unaltered <- grep("likely_unaltered", summary.sub.comp$alteration.status)
    ind.notenough <- grep("Not enough values", summary.sub.comp$alteration.status)
    #if at least 1 metric likely altered, component likely altered
    if(length(ind.altered) > 0){
      component_alteration <- "likely_altered"
    }else{
      #else if only likely_unaltered (all other categories have length 0)
      if(length(ind.unaltered) > 0 & length(ind.altered) == 0 & length(ind.notenough) == 0 & length(ind.indet) == 0){
        component_alteration <- "likely_unaltered"
      }else{
        #else if all not enough data, then not enough data
        if(length(ind.unaltered) == 0 & length(ind.altered) == 0 & length(ind.notenough) > 0 & length(ind.indet) == 0) {
          component_alteration <- "not_enough_data"
        }else{
          #else if all indeterminate, then indeterminate
          if(length(ind.unaltered) == 0 & length(ind.altered) == 0 & length(ind.notenough) == 0 & length(ind.indet) > 0) {
            component_alteration <- "indeterminate"
          }else{
            component_alteration <- "indeterminate"
          }
        }
      }
      
    }
    #create output row
    output.row <- c(summary.sub.comp[1,1:3], component_alteration)
    names(output.row)[4] <- "component_alteration"
    #save into output df
    component.alt.df <- rbind(component.alt.df, output.row)
  }
}

#omit first row NA
component.alt.df <- component.alt.df[2:length(component.alt.df$subbasin),]

#write csv with component alteration categories
write.csv(component.alt.df, file=paste0(out.dir, "component_alteration_current_all.csv"), row.names=FALSE)







