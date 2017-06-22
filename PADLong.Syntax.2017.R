# INPUT PATHS
pal16_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.Record.2016.csv"
pac17_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADCross.Record.2017.csv"
blockGroupsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Block Groups 2015/"
blockGroupsShpName = "Census Block Groups"
tractsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Tracts/"
tractsShpName = "Tracts_Boston_2010_BARI"
IDconnectorPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/IDConnector.2017.csv"
landParcelsPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"


# OUTPUT PATHS
pal17_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.Record.2017.csv"
pal_ct_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.CT.2017.csv"
pal_ct_shp_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/Tract Shp Long/"
pal_ct_shp_name = "PADLong.CT.2017"

pal_bg_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.CBG.2017.csv"
pal_bg_shp_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/BG Shp Long/"
pal_bg_shp_name = "PADLong.CBG.2017"


# READ IN FILES
pal16 = read.csv(pal16_path,stringsAsFactors = FALSE)
PA17 <- read.csv(pac17_path,stringsAsFactors = FALSE)
bgsShp = readOGR(blockGroupsShpPath,blockGroupsShpName)
ctsShp = readOGR(tractsShpPath,tractsShpName)

IDconnector = read.csv(IDconnectorPath, stringsAsFactors=F)
landParcels = read.csv(landParcelsPath, stringsAsFactors=F)



#  adding 2017 data 
PA17.toAdd <- PA17[!duplicated(PA17$parcel_num),c("parcel_num","LU","AV_TOTAL","OWN_OCC",
                                              "X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")]

names(PA17.toAdd)<-c("parcel_num","FY2017.LU","FY2017.AV","FY2017.RESEX",
                 "X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")

# remove old geographic data
for (var in c("X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME","Location_ID")) {
  pal16[,var] = NULL
}
# merge on 2017 data
pal = merge(pal16,PA17.toAdd,by="parcel_num",all=TRUE)

sum(is.na(pal$FY2016.LU) & !is.na(pal$FY2017.LU))
# 2049 new rows

# add geo data for old TAL, this is to include any changes that might have been made to the GI
# for the old TAL rows, their parcel_nums have been matched to GIS_IDs that are in the land parcels file
# first we merge onto the IDConnector,then the land parcels
landParcels = rename(landParcels, TLID = TLID_1)
pal = merge(
  merge(pal,IDconnector[!duplicated(IDconnector$parcel_num),c("parcel_num","GIS_ID","Land_Parcel_ID")],by="parcel_num",all.x=T),
  landParcels[,c("X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")], 
  by.x="Land_Parcel_ID.y",by.y = "Land_Parcel_ID",all.x=T)
# adds in the merged in geographic data, but keeps the original data for those that did not need the merged data
for (var in c("X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")) {
  pal[,var] = ifelse(!is.na(pal[,paste(var,".x",sep="")]),
                     pal[,paste(var,".x",sep="")],
                     pal[,paste(var,".y",sep="")])
  pal[,paste(var,".x",sep="")] = NULL
  pal[,paste(var,".y",sep="")] = NULL
}


#--------------------------------#
#       Adding Extra Vars        #
#--------------------------------#
# Modify Assessed Value Variables ####
lastyear=2017

# calculates the change in valuation year to year 
# Where assessed value is $0, set it to be NA
for (year in c(2000:lastyear)) {
  av = paste("FY",paste(year,".AV",sep=""),sep="")
  pal[,av] = ifelse(pal[,av] >0 & !is.na(pal[,av]),pal[,av],NA)
  if (!is.na(lastyear)) {
    diff = paste("DiffAV",year,sep="")
    avly = paste("FY",paste(lastyear,".AV",sep=""),sep="")
    pal[,diff] = pal[,av]-pal[,avly]
    perc = paste("PercChangeAV",year,sep="")
    pal[,perc] = pal[,diff]/pal[,avly]
  }
  lastyear = year
}


# Calculate change during growth, crash, and recovery periods
#  -- Calculate median value in Boston for each year
apply(pal[,paste(paste("FY",c(2000:lastyear),sep=""),".AV",sep="")], 2,median, na.rm=TRUE)            
# Results:
# min = 2000
# max = 2007
# 2nd min = 2011
# Therefore, 
# growth years = 2000 - 2007
# crash years = 2007 - 2011
# recovery years = 2011 - 2017

# Step 3 -- Create variables for value difference in each time period
pal$GrowthDiffAV <- pal$FY2007.AV-pal$FY2000.AV
pal$GrowthPercChangeAV <- (pal$FY2007.AV-pal$FY2000.AV)/pal$FY2000.AV * 100
pal$CrashDiffAV <- pal$FY2011.AV-pal$FY2007.AV
pal$CrashPercChangeAV <- (pal$FY2011.AV-pal$FY2007.AV)/pal$FY2007.AV * 100
pal$RecoveryDiffAV <- pal$FY2017.AV-pal$FY2011.AV
pal$RecoveryPercChangeAV <- (pal$FY2017.AV-pal$FY2011.AV)/pal$FY2011.AV * 100



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
  pal[,fourcat] = ifelse(!is.na(match(pal[,lu],c("R1","R2","R3","R4","RL","A","CD","CP"))),"Res",
                           ifelse(!is.na(match(pal[,lu],c("CC","C","CL","CM","RC"))),"Comm",
                                  ifelse(!is.na(match(pal[,lu],c("I","AH"))),"Ind",
                                         ifelse(!is.na(match(pal[,lu],c("E","EA"))),"Exem",NA))))
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
                      "X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")
#ordering variables
pal = pal[,variables_ordered]

# write pal17
write.csv(pal, pal17_path, row.names=F)


#make new condo vars for aggregation
for (year in c(2001:lastyear)) {
  nc = paste("NewCondo",year,sep="")
  prevyear = year -1
  thisyearLU = paste(paste("FY",year,sep=""),".LU",sep="")
  prevyearLU = paste(paste("FY",prevyear,sep=""),".LU",sep="")
  thisyearmatch = match(pal[,thisyearLU],c("CD","CP","CM"))
  prevyearmatch = match(pal[,prevyearLU],c("CD","CP","CM"))
  pal[,nc] = ifelse(!is.na(thisyearmatch) & is.na(prevyearmatch),1,
                      ifelse(!is.na(prevyearmatch) & is.na(thisyearmatch), -1, 0))
}



## Aggregate sum change in assessed value over each year by census tract
pal.CT <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),as.numeric(DiffAV2003),as.numeric(DiffAV2004),as.numeric(DiffAV2005),
                            as.numeric(DiffAV2006),as.numeric(DiffAV2007),as.numeric(DiffAV2008),as.numeric(DiffAV2009),as.numeric(DiffAV2010),
                            as.numeric(DiffAV2011),as.numeric(DiffAV2012),as.numeric(DiffAV2013),as.numeric(DiffAV2014),as.numeric(DiffAV2015),
                            as.numeric(DiffAV2016),as.numeric(DiffAV2017),
                            NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, NewCondo2005, 
                            NewCondo2006, NewCondo2007, NewCondo2008, NewCondo2009, NewCondo2010, 
                            NewCondo2011, NewCondo2012, NewCondo2013, NewCondo2014, NewCondo2015,
                            NewCondo2016, NewCondo2017) ~ CT_ID_10, pal, sum, na.rm = TRUE)
names(pal.CT)[1:(length(c(2001:lastyear))+1)] <- c("CT_ID_10",
                                                 "SumDiffAV2001","SumDiffAV2002","SumDiffAV2003","SumDiffAV2004","SumDiffAV2005",
                                                 "SumDiffAV2006","SumDiffAV2007","SumDiffAV2008","SumDiffAV2009","SumDiffAV2010",
                                                 "SumDiffAV2011","SumDiffAV2012","SumDiffAV2013","SumDiffAV2014","SumDiffAV2015",
                                                 "SumDiffAV2016","SumDiffAV2017")


## Aggregate median percentage change in assessed value over each year by census tract
Median_PercentValueChangeYear_CT_Yearly <- aggregate(cbind(PercChangeAV2001,PercChangeAV2002,PercChangeAV2003,PercChangeAV2004,PercChangeAV2005,
                                                           PercChangeAV2006,PercChangeAV2007,PercChangeAV2008,PercChangeAV2009,PercChangeAV2010,
                                                           PercChangeAV2011,PercChangeAV2012,PercChangeAV2013,PercChangeAV2014,PercChangeAV2015,
                                                           PercChangeAV2016,PercChangeAV2017,
                                                           GrowthPercChangeAV, CrashPercChangeAV, RecoveryPercChangeAV) ~ CT_ID_10, pal, median, na.rm=TRUE)
pal.CT <- merge(pal.CT,Median_PercentValueChangeYear_CT_Yearly,by = "CT_ID_10")


#keeping only certain vars (so... didn't need to make condo vars? )
pal.CT = pal.CT[,c(
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

# write csv
write.csv(pal.CT,pal_ct_path , row.names=F)

# merge onto ct shp file
pal.CT.shp = merge(ctsShp,pal.CT,by="CT_ID_10",all.x=T)

# save shp file
writeOGR(pal.CT.shp,pal_ct_shp_path,pal_ct_shp_name,driver="ESRI Shapefile",overwrite_layer=TRUE)


#aggregate to BG
pal.BG <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),as.numeric(DiffAV2003),as.numeric(DiffAV2004),as.numeric(DiffAV2005),
                            as.numeric(DiffAV2006),as.numeric(DiffAV2007),as.numeric(DiffAV2008),as.numeric(DiffAV2009),as.numeric(DiffAV2010),
                            as.numeric(DiffAV2011),as.numeric(DiffAV2012),as.numeric(DiffAV2013),as.numeric(DiffAV2014),as.numeric(DiffAV2015),
                            as.numeric(DiffAV2016),as.numeric(DiffAV2017),
                            NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, NewCondo2005, 
                            NewCondo2006, NewCondo2007, NewCondo2008, NewCondo2009, NewCondo2010, 
                            NewCondo2011, NewCondo2012, NewCondo2013, NewCondo2014, NewCondo2015,
                            NewCondo2016,NewCondo2017) ~ BG_ID_10, pal, sum, na.rm = TRUE)
names(pal.BG)[1:(length(c(2001:lastyear))+1)]  <- c("BG_ID_10",
                                                  "SumDiffAV2001","SumDiffAV2002","SumDiffAV2003","SumDiffAV2004","SumDiffAV2005",
                                                  "SumDiffAV2006","SumDiffAV2007","SumDiffAV2008","SumDiffAV2009","SumDiffAV2010",
                                                  "SumDiffAV2011","SumDiffAV2012","SumDiffAV2013","SumDiffAV2014","SumDiffAV2015",
                                                  "SumDiffAV2016","SumDiffAV2017")


Median_PercentValueChangeYear_BG_Yearly <- aggregate(cbind(PercChangeAV2001,PercChangeAV2002,PercChangeAV2003,PercChangeAV2004,PercChangeAV2005,
                                                           PercChangeAV2006,PercChangeAV2007,PercChangeAV2008,PercChangeAV2009,PercChangeAV2010,
                                                           PercChangeAV2011,PercChangeAV2012,PercChangeAV2013,PercChangeAV2014,PercChangeAV2015,
                                                           PercChangeAV2016,PercChangeAV2017,
                                                           GrowthPercChangeAV, CrashPercChangeAV, RecoveryPercChangeAV) ~ BG_ID_10, pal, median, na.rm=TRUE)
pal.BG <- merge(pal.BG,Median_PercentValueChangeYear_BG_Yearly,by = "BG_ID_10")



pal.BG = pal.BG[,c(
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

# write bg csv
write.csv(pal.BG, pal_bg_path, row.names=F)

# merge to bg shpfile
pal.BG.shp = merge(bgsShp,pal.BG,by="BG_ID_10",all.x=T)

# write bg shpfile
writeOGR(pal.BG.shp,pal_bg_shp_path,pal_bg_shp_name,driver="ESRI Shapefile",overwrite_layer=TRUE)


