# 0X_Level3_Willow_Flow_Range_Determination
# Author: Kris Taniguchi-Quan, SCCWRP

# Find willow flow ranges based on ruleset:
  # Wet-season baseflow (adult):
    # Lower limit: Q at 3cm depth in main channel
    # Upper limit: flow at capacity (limit overbank inundation)
  # Dry-season baseflow (adult): same as wet
    # Lower limit: Q at 3cm depth in main channel
    # Upper limit: flow at capacity (limit overbank inundation) 
  # Spring recession flow (adult and seedling):
    # Lower limit: Q at 10 cm depth inundation in overbanks (capacity + 10cm)
    # Upper limit: no upper limit, use ref 90th percentile


####################################
# Install libraries (only need to do once, uncomment or remove # to run)
# install.packages("plyr")
# install.packages("tidyverse")

# Load libraries
library("plyr")
library("tidyverse")

####################################
# Read in datasets

#read in lookup table with subbasin and X_SECT_ID, filter
lookup <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/nearest_XS_pourpoints_final.csv") %>% 
  rename(Reach.ID = Subbasin) %>% 
  select(X_SECT_ID, Reach.ID)


#XS geometry raw data for every XS in OC survey database
data1 <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/X_Sections_3D_Elevations.csv")
data2 <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/X_Sections_3D_Elevations2.csv")
data.all <- data.frame(rbind(data1, data2)) %>% 
  merge(lookup, by = "X_SECT_ID")
#make sure all distance between pts is 0.1524 (0.5 ft)
unique(data.all$Distance_M)

#read in channel split info for each XS, main channel slice, and capacity station info
split <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/XS_plots/XS_plots_updated_axis_labels/lookup_reach_manningsn_channeltype_splits_10192020.csv")
#split column indices
split.col.ind <- grep("split", names(split))
#split subset to merege with percentiles to get Reach.ID and X_SECT_ID
lookup.2 <- split %>% 
  select(Reach.ID, X_SECT_ID, LSPC.ID)

#read in the percentiles at each site - use ref percentiles for some limits
#final FFM percentiles calculated across period of record for reference scenario
FFM.percentiles <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/SOC_percentiles_final_12012021.csv") %>% 
  filter(dataset == "reference") %>% 
  mutate(LSPC.ID = site) %>% 
  merge(lookup.2, by = "LSPC.ID")
#find unique FFM sites (where we have modeled flows)
unique.FFM.sites <- unique(FFM.percentiles$Reach.ID)
#Note: all percentiles are in cfs, convert to cms for flow ranges


#read in example flow range dataframe used for boxplots, format output the same
example.flowrange <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Synthesis/flow_ranges_alisoSTP_09212021.csv",stringsAsFactors = FALSE)

####################################
# Set directories

#directory and files for rating tables (used to translate hydraulics to flow to determine flow ranges)
rating.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/rating_curve_data/"
list.files.rating <- list.files(rating.dir, full.names = TRUE, pattern ="\\.csv$")


####################################
# Loop through each unique site where we will get flow ranges (all but fully concrete-lined)
  # Determine flow limits based on hydraulic ruleset outline at top

#list the unique sites to find flow ranges for, excluding the sites where main.channel.slice is NA (concrete)
unique.sites <- split$Reach.ID[-which(is.na(split$main.channel.slice))] 
#also exclude sites that don't have flow modeled
unique.sites <- unique.sites[unique.sites %in% unique.FFM.sites]

#create output datasets that are empty and will be filled in, same format as example.flowrange
flow.ranges <- data.frame(matrix(NA, 1, 15))
names(flow.ranges) <- names(example.flowrange)

for(i in 1:length(unique.sites)) {
  # subset datasets to unique.sites[i]
  #channel geometry
  geom.i <- data.all[data.all$Reach.ID == unique.sites[i],] %>% 
    #make sure it is sorted by OBJECTID
    arrange(ï..OBJECTID) %>% 
    mutate(station_m = cumsum(Distance_M) - 0.1524) #calc distance across, first value should be 0, subtract 0.15 from all
  
  #ref percentiles
  ref.percentiles.i <- FFM.percentiles %>% 
    filter(Reach.ID == unique.sites[i])

  #split 
  split.i <- split %>% 
    filter(Reach.ID == unique.sites[i]) 
  #pivot longer split.i, to get column of split stations
  split.sub <- split.i %>% 
    pivot_longer(names(split[,split.col.ind])) %>% 
    as.matrix(nrow = length(split.col.ind), ncol=2) %>% 
    data.frame()
  #determine if splits or no splits (if first split value is NA then no)
  if(is.na(split.sub$value[1])){
    split.y.n <- "N"
    split.num <- 0
  }else{
    split.y.n <- "Y"
    
    #split stations listed into a vector
    split.stations.all <- as.numeric(na.omit(split.sub$value))
    split.num <- length(split.stations.all)
    #find actual station value closes to split.stations.all
    split.stations.actual <- NA
    
    for(l in 1:split.num){
      #find index of station closest to split.stations.all
      ind.station <- which(abs(geom.i$station_m - split.stations.all[l])==min(abs(geom.i$station_m - split.stations.all[l])))
      split.stations.actual[l] <- geom.i$station_m[ind.station] 
    }
    #find all of the breaks to subsection each off of (add the first and last stations to list), will use to subset channel later on
    split.stations.to.subset <- c(0, split.stations.actual, geom.i$station_m[length(geom.i$station_m)])
  }
  
  #rating table
  #find index of file in rating.dir
  ind <- grep(split.i$X_SECT_ID, list.files.rating)
  #read in rating table
  rating.i <- read.csv(file = list.files.rating[ind])
  
  ###############
  #work with geom, get split stations and WSE
  
  #find thalweg elevation from geom
  thalweg_elev <- min(geom.i$ELEVATION_M)
  
  #find the lowest bank elevation that was used as the WSE_max
  #max WSE station: find index of closest station
  WSE.max.station.estimate <- split.i$station_WSE_Max
  #find index of wse.max.station
  ind.wse.max.station <- which(abs(geom.i$station_m - WSE.max.station.estimate)==min(abs(geom.i$station_m - WSE.max.station.estimate)))
  #closest station to the max WSE (lowest bank elev)
  WSE.max.station <- geom.i$station_m[ind.wse.max.station] 
  #elevation to the lowest bank at WSE.max.station
  bank_elev_min <- geom.i$ELEVATION_M[ind.wse.max.station]
  #bank_elev_min <- geom.i$ELEVATION_M[geom.i$station_m == WSE.max.station]
  
  #need to update split.stations.to.subset depending on L or R replace with WSE.max.station
  #if WSE.max.station is on left then WSE.max.station is first value, else it is the last
  diff.left <- abs(WSE.max.station - geom.i$station_m[1]) 
  diff.right <- abs(geom.i$station_m[length(geom.i$station_m)] - WSE.max.station)
  #if on left bank (diff.left < diff.right), change first split to WSE.max.station
  if(diff.left < diff.right){
    split.stations.to.subset[1] <-  WSE.max.station
  }else{
    #else it is on the right bank, change last split to WSE.max.station
    split.stations.to.subset[length(split.stations.to.subset)] <-  WSE.max.station
  }
  
  #list of water surface elevations that are associated with the rating table data
  wse_length <- 200
  wse_m <- seq(thalweg_elev,bank_elev_min, length.out = wse_length)
  #combine wse_m with rating data
  rating.i <- cbind(rating.i, wse_m)
  
  ###################################################
  # Willow flow range determination:
  
  # Wet-season baseflow (adult):
  # Lower limit: Q at 3cm depth in main channel (3 cm + thalweg)
  wet.lower.WSE <- thalweg_elev + 0.03
  #interpolate to find the Q associated with wet.lower.WSE
  wet.lower.Q <- approx(rating.i$wse_m, rating.i$q.cms, wet.lower.WSE)$y
  
  # Upper limit: flow at capacity (limit overbank inundation)
  #interpolate geom to find the elevation associated with capacity 
  capacity.WSE <-  approx(geom.i$station_m, geom.i$ELEVATION_M, split.i$capacity.station)$y
  #interpolate WSE and Q to find Q at capacity
  wet.upper.Q <- approx(rating.i$wse_m, rating.i$q.cms, capacity.WSE)$y

  # Dry-season baseflow: Willow adult same as wet-season baseflow ranges

  # Spring recession (adult and seedling):
    # Lower limit: Q at 10 cm depth inundation in overbanks (capacity + 10cm)
    #find the overbank inundation WSE of 10 cm (capacity+0.1)
    overbank.inun.WSE <- capacity.WSE + .1
    #interpolate WSE and Q to find Q at capacity
    spring.lower.Q <- approx(rating.i$wse_m, rating.i$q.cms, overbank.inun.WSE)$y
    
    # Upper limit: no upper limit, use ref 90th percentile
    spring.upper.Q <- ref.percentiles.i %>% 
      filter(flow_metric == "SP_Mag") %>% 
      select(p90) %>% 
      as.numeric() 
    #percentile in cfs, convert to cms
    spring.upper.Q <- spring.upper.Q*0.028316847
    
  ###################################################
  # save flow ranges into output dataframe
    
  # create dummy output row to fill in the flow ranges for
    row.i <- flow.ranges[i,]
    #save the reach data and species that doesn't change
    row.i$Node <- split.i$LSPC.ID
    row.i$Reach <- split.i$Reach.ID
    row.i$Species1 <- "Willow"
    
  # save in data for willow adult - wet-season baseflow
    wet.i <- row.i
    wet.i$LifeStage <- "Adult"
    wet.i$Lower.Limit <- wet.lower.Q
    wet.i$Upper.Limit <- wet.upper.Q
    wet.i$Seasonal_Component <- "Wet-Season Baseflow"
    wet.i$metric <- "Wet_BFL_Mag_10"
    wet.i$Notes <- "Q at 3 cm main channel depth to channel capacity"
    
  # save in data for willow adult - dry-season baseflow (same as wet flow ranges) 
    dry.i <- wet.i
    dry.i$LifeStage <- "Adult"
    dry.i$Seasonal_Component <- "Dry-Season Baseflow"
    dry.i$metric <- "DS_Mag_50"
    
  # save in data for willow adult and seedling, spring recession flow
    spring.i <- row.i
    spring.i$LifeStage <- "Adult & Seedling"
    spring.i$Lower.Limit <- spring.lower.Q
    #for spring upper limit, if spring lower limit is > upper, put NA as upper limit, else use p90 ref spring.upper.Q
    if(spring.lower.Q > spring.upper.Q){
      spring.i$Upper.Limit <- NA
    }else{
      spring.i$Upper.Limit <- spring.upper.Q
    }
    spring.i$Seasonal_Component <- "Spring Recession Flow"
    spring.i$metric <- "SP_Mag"
    spring.i$Notes <- "Q at 10 cm inundation overbank to ref p90 (if > lower limit)"
    
  #combine all rows
    rows.all <- rbind(wet.i, dry.i, spring.i)
  
  #save rows.all into output df
    flow.ranges <- rbind(flow.ranges, rows.all)

}

#####Post processing flow.ranges for export
#remove first row NA
flow.ranges <- flow.ranges[2:length(flow.ranges$Node),]

#create Species_Label and Species columns by pasting others
#also convert to cfs for lower and upper limits
flow.ranges <- flow.ranges %>% 
  mutate(Species_Label = paste(Species1, LifeStage),
         Species = paste(Species1, "-", LifeStage),
         lowerlimit_cfs = Lower.Limit*35.314666212661,
         upperlimit_cfs = Upper.Limit*35.314666212661)

#####write csv
#filename, update path where data to be saved
filename <- paste0("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier3_analysis", "/Willow_Flow_Ranges_Ruleset.csv")
write.csv(flow.ranges, file = filename, row.names=FALSE)
