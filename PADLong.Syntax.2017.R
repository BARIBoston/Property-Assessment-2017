pal17_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.Record.2017.csv"
pal16_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.Record.2016.csv"
pac17_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADCross.Record.2017.csv"
pal_ct_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.CT.2017.csv"
pal_bg_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.CBG.2017.csv"
P17shp_path = "/Users/henrygomory/Downloads/Parcels2017DataFull/"  # from boston open data
P17shp_name = "Parcels2017DataFull"  # from boston open data
IDconnectorPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/IDConnector.2017.csv"
propertiesPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/Properties.2017.csv"
parcelsPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"

pal_ct_shp_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/Tract Shp Long/"
pal_ct_shp_name = "PADLong.CT.2017"

pal_bg_shp_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/BG Shp Long/"
pal_bg_shp_name = "PADLong.CBG.2017"

pal16_b = read.csv(pal16_path,stringsAsFactors = FALSE)
PA17_b <- read.csv(pac17_path,stringsAsFactors = FALSE)
ctsShp_b = getTractsShp()
bgsShp_b = getBGsShp()
P17shp_b = readOGR(P17shp_path,P17shp_name,stringsAsFactors=F)
IDconnector_b = read.csv(IDconnectorPath, stringsAsFactors=F)
properties_b = read.csv(propertiesPath, stringsAsFactors=F)
parcels_b = read.csv(parcelsPath, stringsAsFactors=F)

pal16 = pal16_b
PA17 = PA17_b
ctsShp = ctsShp_b
bgsShp = bgsShp_b
P17shp = P17shp_b
IDconnector = IDconnector_b
properties = properties_b
parcels = parcels_b

###  adding 2017 data ####
#making sure that  any record that is a duplicated parcel_num is also a duplicated on these other variables
# to ensure we are not losing any information
sum(duplicated(PA17$parcel_num))
sum(duplicated(PA17[,c("parcel_num","LU","AV_TOTAL","OWN_OCC")]))

PA17.1 <- PA17[!duplicated(PA17$parcel_num),c("parcel_num","LU","AV_TOTAL","OWN_OCC")]


names(PA17.1)<-c("parcel_num","FY2017.LU","FY2017.AV","FY2017.RESEX")
pal = merge(pal16,PA17.1,by="parcel_num",all=TRUE)

sum(is.na(pal$FY2016.LU) & !is.na(pal$FY2017.LU))
# 2049 new rows

##############
# add geo data 
##############

#first remove old geo data
geoData = c("X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")
  
for (var in c(geoData,"Location_ID")) {
  pal[,var] = NULL
}

# for those in the property assessment 2017, we can just get the geo directly from the properties file
# for those not in the property assessment 2017, their parcel_nums have been matched to GIS_IDs that are in the land parcels file
# so we have to get their geo data from the land parcels file rather than the properties file
# it would be easier to just get both from the land parcels file, but it's more precise to get data from the properties file for those that we can (although in practice it probably only affects a handful of cases)
pal1 = pal[ pal$parcel_num %in% properties$parcel_num,]
pal1 = merge(pal1, properties[,c("parcel_num","GIS_ID",geoData)],by="parcel_num",all.x=T)

pal2 = pal[! pal$parcel_num %in% properties$parcel_num,]
pal2 = merge(pal2,IDconnector[!duplicated(IDconnector$parcel_num),c("parcel_num","GIS_ID","Land_Parcel_ID")],by="parcel_num",all.x=T)
#we have to write out geoData here because we need TLID_1 not TLID
pal2 = merge(pal2, parcels[,c("Land_Parcel_ID","X","Y","TLID_1","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")],by="Land_Parcel_ID",all.x=T)
pal2 = rename(pal2, TLID= TLID_1)
pal = rbind(pal1,pal2)



#--------------------------------#
#       Adding Extra Vars        #
#--------------------------------#
pal.3 = pal
# Modify Assessed Value Variables ####
lastyear=2017
# ----------------------------------------------------------------
# Where assessed value is $0, set it to be NA
for (year in c(2000:lastyear)) {
  av = paste("FY",paste(year,".AV",sep=""),sep="")
  pal.3[,av] = ifelse(pal.3[,av] >0 & !is.na(pal.3[,av]),pal.3[,av],NA)
  if (!is.na(lastyear)) {
    diff = paste("DiffAV",year,sep="")
    avly = paste("FY",paste(lastyear,".AV",sep=""),sep="")
    pal.3[,diff] = pal.3[,av]-pal.3[,avly]
    perc = paste("PercChangeAV",year,sep="")
    pal.3[,perc] = pal.3[,diff]/pal.3[,avly]
  }
  lastyear = year
}


# ----------------------------------------------------------------
# Calculate change during growth, crash, and recovery periods
#  -- Calculate median value in Boston for each year
apply(pal.3[,paste(paste("FY",c(2000:lastyear),sep=""),".AV",sep="")], 2,median, na.rm=TRUE)            
# Results:
# min = 2000
# max = 2007
# 2nd min = 2011
# Therefore, 
# growth years = 2000 - 2007
# crash years = 2007 - 2011
# recovery years = 2011 - 2017

# Step 3 -- Create variables for value difference in each time period
pal.3$GrowthDiffAV <- pal.3$FY2007.AV-pal.3$FY2000.AV
pal.3$GrowthPercChangeAV <- (pal.3$FY2007.AV-pal.3$FY2000.AV)/pal.3$FY2000.AV * 100
pal.3$CrashDiffAV <- pal.3$FY2011.AV-pal.3$FY2007.AV
pal.3$CrashPercChangeAV <- (pal.3$FY2011.AV-pal.3$FY2007.AV)/pal.3$FY2007.AV * 100
pal.3$RecoveryDiffAV <- pal.3$FY2017.AV-pal.3$FY2011.AV
pal.3$RecoveryPercChangeAV <- (pal.3$FY2017.AV-pal.3$FY2011.AV)/pal.3$FY2011.AV * 100



# ----------------------------------------------------------------
# Broader Land Use Categories ####

# Group land use codes into 4 simplified categories####

#Res= R1, R2, R3, R4, RL, A, CD, CP
#Comm= CC, C, CL, CM, RC
#Ind= I, AH
#Exem= E, EA

variables_ordered = c("parcel_num","CM_ID","ST_NUM","ST_NAME","ST_NAME_SUF","UNIT_NUM","ZIPCODE")
for (year in c(2000:lastyear)) {
  fourcat = paste(paste("LU",year,sep=""),"FourCat",sep="")
  lu = paste("FY",paste(year,".LU",sep=""),sep="")
  pal.3[,fourcat] = ifelse(!is.na(match(pal.3[,lu],c("R1","R2","R3","R4","RL","A","CD","CP"))),"Res",
                           ifelse(!is.na(match(pal.3[,lu],c("CC","C","CL","CM","RC"))),"Comm",
                                  ifelse(!is.na(match(pal.3[,lu],c("I","AH"))),"Ind",
                                         ifelse(!is.na(match(pal.3[,lu],c("E","EA"))),"Exem",NA))))
  av = paste("FY",paste(year,".AV",sep=""),sep="")
  resex = paste("FY",paste(year,".RESEX",sep=""),sep="")
  if (year != 2000) {
    diffav = paste("DiffAV",year,sep="")
    percChange = paste("PercChangeAV",year,sep="")
    variables_ordered = c(variables_ordered,lu,av,resex,diffav,percChange,fourcat)
  }
  else {
    variables_ordered = c(variables_ordered,lu,av,resex,fourcat)
    
  }
}
variables_ordered = c(variables_ordered,
                      "GrowthDiffAV","GrowthPercChangeAV","CrashDiffAV","CrashPercChangeAV",
                      "RecoveryDiffAV","RecoveryPercChangeAV",
                      "X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")
#ordering variables
pal.3 = pal.3[,variables_ordered]

write.csv(pal.3, pal17_path, row.names=F)


#make new condo vars
for (year in c(2001:lastyear)) {
  nc = paste("NewCondo",year,sep="")
  prevyear = year -1
  thisyearLU = paste(paste("FY",year,sep=""),".LU",sep="")
  prevyearLU = paste(paste("FY",prevyear,sep=""),".LU",sep="")
  thisyearmatch = match(pal.3[,thisyearLU],c("CD","CP","CM"))
  prevyearmatch = match(pal.3[,prevyearLU],c("CD","CP","CM"))
  pal.3[,nc] = ifelse(!is.na(thisyearmatch) & is.na(prevyearmatch),1,
                      ifelse(!is.na(prevyearmatch) & is.na(thisyearmatch), -1, 0))
}



## Aggregate sum change in assessed value over each year by census tract
pal.3.CT <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),as.numeric(DiffAV2003),as.numeric(DiffAV2004),as.numeric(DiffAV2005),
                            as.numeric(DiffAV2006),as.numeric(DiffAV2007),as.numeric(DiffAV2008),as.numeric(DiffAV2009),as.numeric(DiffAV2010),
                            as.numeric(DiffAV2011),as.numeric(DiffAV2012),as.numeric(DiffAV2013),as.numeric(DiffAV2014),as.numeric(DiffAV2015),
                            as.numeric(DiffAV2016),as.numeric(DiffAV2017),
                            NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, NewCondo2005, 
                            NewCondo2006, NewCondo2007, NewCondo2008, NewCondo2009, NewCondo2010, 
                            NewCondo2011, NewCondo2012, NewCondo2013, NewCondo2014, NewCondo2015,
                            NewCondo2016, NewCondo2017) ~ CT_ID_10, pal.3, sum, na.rm = TRUE)
names(pal.3.CT)[1:(length(c(2001:lastyear))+1)] <- c("CT_ID_10",
                                                 "SumDiffAV2001","SumDiffAV2002","SumDiffAV2003","SumDiffAV2004","SumDiffAV2005",
                                                 "SumDiffAV2006","SumDiffAV2007","SumDiffAV2008","SumDiffAV2009","SumDiffAV2010",
                                                 "SumDiffAV2011","SumDiffAV2012","SumDiffAV2013","SumDiffAV2014","SumDiffAV2015",
                                                 "SumDiffAV2016","SumDiffAV2017")


## Aggregate median percentage change in assessed value over each year by census tract
Median_PercentValueChangeYear_CT_Yearly <- aggregate(cbind(PercChangeAV2001,PercChangeAV2002,PercChangeAV2003,PercChangeAV2004,PercChangeAV2005,
                                                           PercChangeAV2006,PercChangeAV2007,PercChangeAV2008,PercChangeAV2009,PercChangeAV2010,
                                                           PercChangeAV2011,PercChangeAV2012,PercChangeAV2013,PercChangeAV2014,PercChangeAV2015,
                                                           PercChangeAV2016,PercChangeAV2017,
                                                           GrowthPercChangeAV, CrashPercChangeAV, RecoveryPercChangeAV) ~ CT_ID_10, pal.3, median, na.rm=TRUE)
pal.3.CT <- merge(pal.3.CT,Median_PercentValueChangeYear_CT_Yearly,by = "CT_ID_10")

#BG
pal.3.BG <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),as.numeric(DiffAV2003),as.numeric(DiffAV2004),as.numeric(DiffAV2005),
                            as.numeric(DiffAV2006),as.numeric(DiffAV2007),as.numeric(DiffAV2008),as.numeric(DiffAV2009),as.numeric(DiffAV2010),
                            as.numeric(DiffAV2011),as.numeric(DiffAV2012),as.numeric(DiffAV2013),as.numeric(DiffAV2014),as.numeric(DiffAV2015),
                            as.numeric(DiffAV2016),as.numeric(DiffAV2017),
                            NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, NewCondo2005, 
                            NewCondo2006, NewCondo2007, NewCondo2008, NewCondo2009, NewCondo2010, 
                            NewCondo2011, NewCondo2012, NewCondo2013, NewCondo2014, NewCondo2015,
                            NewCondo2016,NewCondo2017) ~ BG_ID_10, pal.3, sum, na.rm = TRUE)
names(pal.3.BG)[1:(length(c(2001:lastyear))+1)]  <- c("BG_ID_10",
                                                  "SumDiffAV2001","SumDiffAV2002","SumDiffAV2003","SumDiffAV2004","SumDiffAV2005",
                                                  "SumDiffAV2006","SumDiffAV2007","SumDiffAV2008","SumDiffAV2009","SumDiffAV2010",
                                                  "SumDiffAV2011","SumDiffAV2012","SumDiffAV2013","SumDiffAV2014","SumDiffAV2015",
                                                  "SumDiffAV2016","SumDiffAV2017")

#BG
Median_PercentValueChangeYear_BG_Yearly <- aggregate(cbind(PercChangeAV2001,PercChangeAV2002,PercChangeAV2003,PercChangeAV2004,PercChangeAV2005,
                                                           PercChangeAV2006,PercChangeAV2007,PercChangeAV2008,PercChangeAV2009,PercChangeAV2010,
                                                           PercChangeAV2011,PercChangeAV2012,PercChangeAV2013,PercChangeAV2014,PercChangeAV2015,
                                                           PercChangeAV2016,PercChangeAV2017,
                                                           GrowthPercChangeAV, CrashPercChangeAV, RecoveryPercChangeAV) ~ BG_ID_10, pal.3, median, na.rm=TRUE)
pal.3.BG <- merge(pal.3.BG,Median_PercentValueChangeYear_BG_Yearly,by = "BG_ID_10")


#keeping only certain vars (so... didn't need to make condo vars? )
pal.3.CT = pal.3.CT[,c(
  "CT_ID_10",
  "SumDiffAV2001","PercChangeAV2001","NewCondo2001" ,
  "SumDiffAV2002","PercChangeAV2002","NewCondo2002" ,
  "SumDiffAV2003","PercChangeAV2003","NewCondo2003" ,
  "SumDiffAV2004","PercChangeAV2004","NewCondo2004" ,
  "SumDiffAV2005","PercChangeAV2005","NewCondo2005" ,
  "SumDiffAV2006","PercChangeAV2006","NewCondo2006" ,
  "SumDiffAV2007","PercChangeAV2007","NewCondo2007" ,
  "SumDiffAV2008","PercChangeAV2008","NewCondo2008" ,
  "SumDiffAV2009","PercChangeAV2009","NewCondo2009" ,
  "SumDiffAV2010","PercChangeAV2010","NewCondo2010" ,
  "SumDiffAV2011","PercChangeAV2011","NewCondo2011" ,
  "SumDiffAV2012","PercChangeAV2012","NewCondo2012" ,
  "SumDiffAV2013","PercChangeAV2013","NewCondo2013" ,
  "SumDiffAV2014","PercChangeAV2014","NewCondo2014" ,
  "SumDiffAV2015","PercChangeAV2015","NewCondo2015" ,
  "SumDiffAV2016","PercChangeAV2016","NewCondo2016" ,
  "SumDiffAV2017","PercChangeAV2017","NewCondo2017" ,
  "GrowthPercChangeAV","CrashPercChangeAV" , "RecoveryPercChangeAV" )]


pal.3.BG = pal.3.BG[,c(
  "BG_ID_10",
  "SumDiffAV2001","PercChangeAV2001","NewCondo2001" ,
  "SumDiffAV2002","PercChangeAV2002","NewCondo2002" ,
  "SumDiffAV2003","PercChangeAV2003","NewCondo2003" ,
  "SumDiffAV2004","PercChangeAV2004","NewCondo2004" ,
  "SumDiffAV2005","PercChangeAV2005","NewCondo2005" ,
  "SumDiffAV2006","PercChangeAV2006","NewCondo2006" ,
  "SumDiffAV2007","PercChangeAV2007","NewCondo2007" ,
  "SumDiffAV2008","PercChangeAV2008","NewCondo2008" ,
  "SumDiffAV2009","PercChangeAV2009","NewCondo2009" ,
  "SumDiffAV2010","PercChangeAV2010","NewCondo2010" ,
  "SumDiffAV2011","PercChangeAV2011","NewCondo2011" ,
  "SumDiffAV2012","PercChangeAV2012","NewCondo2012" ,
  "SumDiffAV2013","PercChangeAV2013","NewCondo2013" ,
  "SumDiffAV2014","PercChangeAV2014","NewCondo2014" ,
  "SumDiffAV2015","PercChangeAV2015","NewCondo2015" ,
  "SumDiffAV2016","PercChangeAV2016","NewCondo2016" ,
  "SumDiffAV2017","PercChangeAV2017","NewCondo2017" ,
  "GrowthPercChangeAV","CrashPercChangeAV" , "RecoveryPercChangeAV" )]



write.csv(pal.3.CT,pal_ct_path , row.names=F)

write.csv(pal.3.BG, pal_bg_path, row.names=F)


pal.3.CT.shp = merge(ctsShp,pal.3.CT,by="CT_ID_10",all.x=T)
writeOGR(pal.3.CT.shp,pal_ct_shp_path,pal_ct_shp_name,driver="ESRI Shapefile",overwrite_layer=TRUE)


pal.3.BG.shp = merge(bgsShp,pal.3.BG,by="BG_ID_10",all.x=T)
writeOGR(pal.3.BG.shp,pal_bg_shp_path,pal_bg_shp_name,driver="ESRI Shapefile",overwrite_layer=TRUE)


