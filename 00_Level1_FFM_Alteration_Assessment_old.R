#SOC FESS Level 1: Alteration assessment for all subbasins modeled in LSCP (Laguna Canyon, Aliso Creek, Oso Creek, Salt Creek, Horno Creek, Prima Deshecha Creek, and Segunda Deshecha Creek)
  #This code loops through each output file from LSPC and calculates functional flow metrics (FFM) based on current and reference outputs
  #Once current and ref FFM are calculated using the functional flows calculator (FFC), an alteration assessment is conducted

#Notes:
  #Directories will need to be updated based on where model outputs are saved on your local computer. Download current and reference outputs at:
    #Current flow: https://ocgov.box.com/s/o8t0eszroe812meur2z5clo1aa69waci (Model_Output_1993-2019.zip, unzip and save to local drive)
    #Reference flow: https://ocgov.box.com/s/ec61ciawebsd3u5tweoapypm1ryxoxng (Reference_Condition_Model_Output_1993-2019.zip, unzip and save to local drive)
  #This script utilizes the FFC API package developed for CEFF: https://github.com/ceff-tech/ffc_api_client, see github link for additional details on installation and use

#install libraries - only need to do this once
#install.packages("devtools")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("purrr")
#install.packages("plyr")
#install.packages("tidyverse")

#install ffcAPIClient - package to calculate FFM - only need to install once
#library("devtools")
#install.packages("ffcAPIClient")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient') #if error, make sure using latest version of devtools
#Retrieve token: In Firefox or Chrome, log into https://eflows.ucdavis.edu. Once logged in, make sure you are on your user profile page at https://eflows.ucdavis.edu/profile and then press F12 on your keyboard to bring up the Inspector, then switch to the Console tab.
  #In the console, type localStorage.getItem('ff_jwt') - you may need to type it in yourself instead of pasting (or follow Firefox's instructions to enable pasting - it will tell you how after you try to paste). Hit Enter to send the command.
  #Your browser will place text on the line below the command you typed - this is your "token". Save this value and copy it to your clipboard and we'll use it below. This value should stay private - if other people knew the value, they could use it to access your account on eflows.ucdavis.edu!


#load library
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#to uninstall ffcAPIClient package and reinstall (if updates to R package were made)
#install.packages("devtools")
#library("devtools")
#remove.packages("ffcAPIClient") #uninstall then restart R session
#restart R session
#library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
#install.packages("ffcAPIClient")
#library("ffcAPIClient")


#save your token for FFC API Client in parentheses - example token used below
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"
#set token
set_token(mytoken)

#### Save directories where the current and reference LSPC flow outputs are saved on your local drive ####

#Directories current and reference output data (examples below are for Oso and other smaller creeks)
curr.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Existing_Conditions/Model_Output_WY94-19/"
ref.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Reference_Condition/WY94-19/"

#alteration directory name - where the alteration summary data will be saved
#create alteration folder directory on local drive (outside of R)
#create the name of subfolder within alteration directory below (in R)
alt.dir.name <- "Oso_SmallCreeks/"
#alteration directory (change directory in parentheses to your alteration folder, will be pasted with the subfolder name)
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)
#create subfolder directory if does not already exist
dir.create(alteration.dir)

#### Read in csv file ####
#read in lookup table information on model subbasin, NHD COMID.  This csv is called: SOC_FESS_Subbasin_NHD.COMID_Lookup_Table.csv and saved in GitHub directory
basin_comid_lookup <- read.csv("C:/Users/KristineT/Documents/Git/SOCFESS_2021AnnualReport/SOC_FESS_Subbasin_NHD.COMID_Lookup_Table.csv") #change this to your local directory



##############################
######loop to run through each subbasin output file, calculate FFM and assess alteration based on reference condition model scenario [instead of CEFF's statewide reference model outputs]
#Functional flow metric names and labels for plots - this csv is saved in GitHub directory
filename <- ("C:/Users/KristineT/Documents/Git/SOCFESS_2021AnnualReport/all_metric_def_list_FFMs_v2.csv") #change to your local directory where file is saved
ffm.labels <- read.csv(filename)

#list of file names to loop through
fnames <- list.files(curr.dir, pattern = "\\.out$")

#create an empty data frame for alteration determination and direction
alteration.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=7))
#set column names of df
names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "comid.notes")

#create an empty vector to save potential errors from the FFC
ffc.errors <- NA

#loop through all file names
for (i in 1:length(fnames)){
    
  #get data for subbasin i
  subbasin.model <- gsub(".out","", fnames[i])
  #subset lookup table to get comid, etc
  sub <- basin_comid_lookup[basin_comid_lookup$new.subbasinname == subbasin.model,] 
  gage.name <- sub$Gage
  subbasin <- as.character(sub$Subbasin)
  #COMID to run the FFC, FFC uses the COMID to get the hydrologic stream class and associated parameters to calculate FFM in that stream class
    #NOTE: the FFC will evaluate alteration comparing input flow file and statewide reference model however, this evaluate will not use statewide reference model and alteration status since LSPC reference model was developed
  COMID <- sub$COMID_forcalc
  
  ################################################
  ####LSPC current data
  #when reading in data, skip the first 23 rows that provides model descriptions, only want timeseries data
  skip = 23
  
  #load in hourly model current predictions
  curr <- read.table(paste0(curr.dir,fnames[i]), skip=skip)
  #save names of the  columns
  names(curr) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "av.depth", "av.vel","flow.cfs")
  #format date
  #add leading zero to hour
  MONTH <- sprintf("%02d",curr$month)
  DAY <- sprintf("%02d",curr$day)
  date <- paste(MONTH, DAY, curr$year, sep="/")
  curr$date <- date
  unique.dates <- unique(date)
  #format Q to be numeric
  curr$flow.cfs <- as.numeric(as.character(curr$flow.cfs))
  ################
  
  #calc mean daily flow for current predicted data - FFC uses mean daily flow as input
  mean.daily.curr <- curr %>% 
    group_by(date) %>% 
    summarize(flow = mean(flow.cfs, ra.rm = TRUE)) %>% 
    ungroup()
  
  #create new data frame with date and mean daily flow to go into FFC
  data.curr <- data.frame(cbind(mean.daily.curr$date, mean.daily.curr$flow))
  names(data.curr) <- c("date", "flow")
  data.curr$flow <- as.numeric(data.curr$flow )
  
  #if it's the first iteration, create directories for ref and current daily flow and FFM
  if(i == 1){
    dir.create(paste0(curr.dir,"daily/"))
    dir.create(paste0(curr.dir,"daily/FFMs/"))
    dir.create(paste0(ref.dir,"daily/"))
  }
  
  #create daily output file name in the current directory (curr.dir)
  fname <- paste0(curr.dir,"daily/", subbasin.model,"_curr_daily.txt")
  #save current daily flow 
  write.table(data.curr, fname, row.names = FALSE, sep = ",")
  
  ################
  #calc FFMs and alteration for current data
  #create new directory to save ffm outputs
  dir.new <- paste0(curr.dir,"daily/FFMs/",subbasin.model)
  dir.create(dir.new)
  
  #Try to catch errors in evaluate alteration, if errors, write the error and skip to the next subbasin
  tryCatch({
    #Run data.currframe through FFC online with my own model data data.curr
    #new FFC api set up
    results.curr <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    results.curr$fail_years_data <- 1
    #setup the run timeseries data, token, COMID
    results.curr$set_up(timeseries=data.curr,
                        token=mytoken,
                        comid = COMID)
    #set to rain and seasonal groundwater "RGW" stream class for all --> every COMID is in this class in SOC
    results.curr$stream_class <- "RGW"
    
    #then run to get FFC results
    results.curr$run()
    
    #FFC results, LSPC current
    #annual FFM from LSPC current
    curr.results.ffm.all <- results.curr$ffc_results
    curr.results.ffm.all$type <- "curr" #save type as current scenario
    #summary percentile FFM calculated across period of record (POR)
    curr.percentiles.all <- results.curr$ffc_percentiles
    curr.percentiles.all$source2 <- rep("LSPC\nCurrent", length(curr.percentiles.all$p10))
    #write outputs to directory dir.new
    write.csv(curr.percentiles.all, file=paste0(dir.new,"/curr.percentiles.all.csv"), row.names=FALSE)
    write.csv(curr.results.ffm.all, file=paste0(dir.new,"/curr.results.ffm.all.csv"), row.names=FALSE)

    
    ################################################
    ####LSPC reference data######
    
    #load in reference LSPC model output for same subbasin.model
    #read in ref data
    ref <- read.table(paste0(ref.dir,fnames[i]), skip=skip)
    names(ref) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "av.depth", "av.vel","flow.cfs")
    #format date
    #add leading zero to hour
    MONTH2 <- sprintf("%02d",ref$month)
    DAY2 <- sprintf("%02d",ref$day)
    date2 <- paste(MONTH2, DAY2, ref$year, sep="/")
    ref$date <- date2
    unique.dates2 <- unique(date2)
    #format Q to be numeric
    ref$flow.cfs <- as.numeric(as.character(ref$flow.cfs))
    ################
    
    #calc mean daily flow for reference model output LSPC
    mean.daily.ref <- ref %>% 
      group_by(date) %>% 
      summarize(flow = mean(flow.cfs, ra.rm = TRUE)) %>% 
      ungroup()
    
    #if NaN, replace with NA
    flow.ref<- as.numeric(sub("NaN", "NA", mean.daily.ref$flow))
    #create new data frame with date and mean daily flow to go into FFC
    data.ref <- data.frame(cbind(mean.daily.ref$date, flow.ref))
    names(data.ref) <- c("date", "flow")
    data.ref$flow <- as.numeric(data.ref$flow)
    
    #write daily output file
    fname2 <- paste0(ref.dir,"daily/", subbasin.model,"_ref_daily.txt")
    dir.create(paste0(ref.dir,"daily/", subbasin.model,"/")) #create directory
    write.table(data.ref, fname2, row.names = FALSE, sep = ",")
    
    ################
    #calc FFMs and alteration for reference data
    
    #create new directory to save ffm outputs
    dir.new2 <- paste0(ref.dir,"daily/FFMs/",subbasin.model)
    dir.create(paste0(ref.dir,"daily/FFMs/"))
    dir.create(dir.new2)
    
    #Run data frame through FFC online with reference data data.ref
    #FFC api set up
    results.ref <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    results.ref$fail_years_data <- 1
    #setup ffcAPI with data, token, COMID
    results.ref$set_up(timeseries=data.ref,
                        token=mytoken,
                        comid = COMID)
    #set to "RGW" stream class for all --> every COMID is in this class in SOC
    results.ref$stream_class <- "RGW"
    #then run
    results.ref$run()
    
    #save output df
    #annual FFM results for reference scenario
    ref.results.ffm.all <- results.ref$ffc_results
    ref.results.ffm.all$type <- "ref"
    #summary percentiles for FFM reference scenario
    ref.percentiles.all <- results.ref$ffc_percentiles
    ref.percentiles.all$source2 <- rep("LSCP\nReference", length(ref.percentiles.all$p10))

    #write outputs to dir
    write.csv(ref.percentiles.all, file=paste0(dir.new2,"/ref.percentiles.all.csv"), row.names=FALSE)
    write.csv(ref.results.ffm.all, file=paste0(dir.new2,"/ref.results.ffm.all.csv"), row.names=FALSE)

    ############################
    ###Alteration Determination between LSPC current and ref FFM 
    
    #Loop through all FFM to determine alteration status and direction of alteration
    ffm <- as.character(ffm.labels$metric)
    #empty vectors to be filled in with alteration data
    alteration.status <- NA
    alteration.direction <- NA

    for(l in 1:length(ffm)){

      #model results and median for ffm i
      ind.ffm.l <- grep(ffm[l], names(curr.results.ffm.all))
      model.curr.ffms.i <- as.numeric(as.character(curr.results.ffm.all[,ind.ffm.l]))
      model.curr.i.med <- curr.percentiles.all$p50[curr.percentiles.all$metric==ffm[l]]
      #find 10-90th ref percentiles
      model.ref.i.90 <-  ref.percentiles.all$p90[ref.percentiles.all$metric == ffm[l]]
      model.ref.i.10 <-  ref.percentiles.all$p10[ref.percentiles.all$metric == ffm[l]]
      
      #if NA values for current or reference ffms, then all are NA
      if(is.na(model.curr.i.med) |  is.na(model.ref.i.90)){
        alteration.status[l] <- NA
        alteration.direction[l] <- NA
      }else{
        #else, if median falls outside of 10-90, likely altered
        if(model.curr.i.med > model.ref.i.90 | model.curr.i.med < model.ref.i.10){
          alteration.status[l] <- "likely_altered"
          #if it is altered determine direction of alteration high or low
          if(model.curr.i.med > model.ref.i.90){
            #direction is high when current median is larger than the ref 90th percentile
            alteration.direction[l] <- "high"
          }else{
            #else, direction will be low
            alteration.direction[l] <- "low"
          }
        }else{
          #if median falls within 10-90th and >50% falls within the range, then indeterminate or <50% falls within range (likely unaltered)
          #since not altered, no alteration direction
          alteration.direction[l] <- "none_found"
          #determine how many values fall in the reference 10-90th range
          count.in.range <- length(which(model.curr.ffms.i <= model.ref.i.90 & model.curr.ffms.i >= model.ref.i.10))
          percent.inrange <- count.in.range/length(na.omit(model.curr.ffms.i))
          #if more than half in the range, likely unaltered
          if(percent.inrange > 0.5){
            alteration.status[l] <- "likely_unaltered"
          #else, if < half is in the range, indeterminate  
          }else{
            alteration.status[l] <- "indeterminate"
          }
        }
      }
    }
    
    #save into alteration data frame
    comid.notes <- as.character(sub$Notes)
    alteration.comparison.df <- data.frame(cbind(COMID, subbasin.model, subbasin, ffm, alteration.status, alteration.direction, comid.notes))
    #save alteration for subbain in output directory
    write.csv(alteration.comparison.df, file=paste0(dir.new,"/",subbasin.model, "_alteration_comparison_lspcref_statewide.csv"),row.names=FALSE)
    
    #save alteration in overall df (that contains all subbasins)
    alteration.df.overall <- data.frame(rbind(alteration.df.overall, alteration.comparison.df))
  #end loop, if error print out FFC error and skip to next subbasin
  }, error = function(e) {
    print(paste0(i, " FFC Error"))
    ffc.errors <- c(ffc.errors, subbasin.model)
  })
}


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
#add column for component.alteration, use the alteration status column and replace with appropriate labels
alteration.df.overall.join$component_alteration <- alteration.df.overall.join$alteration.status
#if metric is likely unaltered, put NA in component alteration column
alteration.df.overall.join$component_alteration <- gsub("likely_unaltered", NA, alteration.df.overall.join$component_alteration)
#if metric is indeterminate, put NA
alteration.df.overall.join$component_alteration <- gsub("indeterminate", NA, alteration.df.overall.join$component_alteration)

#write overall alteration FFM to alteration directory
write.csv(alteration.df.overall.join, file=paste0(alteration.dir, "ffm_alteration.df.overall.join.csv"), row.names=FALSE)


#synthesis component alteration
#find rows where component alteration is NA
ind.NA <- which(is.na(alteration.df.overall.join$component_alteration))
#subset to only include likely altered components
component.alteration.subset <- alteration.df.overall.join[-ind.NA,]
#write separate csv with just altered components
write.csv(component.alteration.subset, file=paste0(alteration.dir, "component.alteration.df.overall.join.csv"), row.names=FALSE)





