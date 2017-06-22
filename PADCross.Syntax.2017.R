# INPUT PATHS
PA17path =      "/Users/henrygomory/Downloads/property-assessment-fy2017.csv"
properties2017Path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/Properties.2017.csv"
blockGroupsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Block Groups 2015/"
blockGroupsShpName = "Census Block Groups"
tractsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Tracts/"
tractsShpName = "Tracts_Boston_2010_BARI"


# OUTPUT PATHS
PA17_final_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADCross.Record.2017.csv"

pa_ct_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADCross.CT.2017.csv"
pa_ctshp_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/Tract Shp/"
pa_ctshp_name = "PADCross.CT.2017"

pa_bg_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADCross.CBG.2017.csv"
pa_bgshp_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/BG Shp/"
pa_bgshp_name = "PADCross.CBG.2017"

### Read in data
PA17 = read.csv(PA17path,stringsAsFactors=F)
bgsShp = readOGR(blockGroupsShpPath,blockGroupsShpName)
tractsShp = readOGR(tractsShpPath,tractsShpName)
properties2017 = read.csv(properties2017Path,stringsAsFactors=F)

# fix IDs
PA17 = standardizeGeoNames(PA17)

#add on geo data from GI
PA17 = merge(PA17, properties2017[,c("parcel_num",c("X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))], by= "parcel_num",all.x=T)

#-------------------------------------------#
#       Create new vars                     #
#-------------------------------------------#
PA = PA17

PA$LU[PA$LU == 'XX'] <- "E"

## Allocating Residential Heat Type Energy Efficiency Score
PA$HEAT_SCORE <- NA
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'S', 0, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'W', 1, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'P', 2, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'F', 3, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'E', 4, PA$HEAT_SCORE) 

## Allocating age to Residential buildings
PA$YR_BUILT[PA$YR_BUILT == '' | PA$YR_BUILT == 0] <- NA
PA$YR_REMOD[PA$YR_REMOD == '' | PA$YR_REMOD == 0] <- NA
PA$YR_REMOD[!is.na(PA$YR_REMOD) & PA$YR_REMOD==995]=1995
PA$BLDG_AGE <- ifelse(is.na(PA$YR_REMOD), (2017 - PA$YR_BUILT), (2017 - PA$YR_REMOD))
PA$BLDG_AGE[PA$BLDG_AGE <=0] <- NA

##Allocating Building Age Score
PA$AGE_SCORE <- NA
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE < 50,4,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 50,3,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 100,2,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 150,1,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 200,0,PA$AGE_SCORE)

## Allocating Residential Air Conditioner Energy Efficiency Score
PA$COOL_SCORE <- NA
PA$COOL_SCORE <- ifelse(PA$R_AC == 'C', 1, PA$COOL_SCORE)
PA$COOL_SCORE <- ifelse(PA$R_AC == 'D', 2, PA$COOL_SCORE)
PA$COOL_SCORE <- ifelse(PA$R_AC == 'N', 3, PA$COOL_SCORE)

## Aggregate Energy Efficiency score at building level
PA$EE_SCORE <- PA$AGE_SCORE+0.75*PA$HEAT_SCORE+0.75*PA$COOL_SCORE
PA = PA
#PA[!(PA$LU =='R1'| PA$LU == 'R2'| PA$LU == 'R3') ,c("HEAT_SCORE","COOL_SCORE","EE_SCORE")]=c(NA,NA,NA)

#BLDG PER SF missing: AH, CC, CL, CM, CP, RL (many E)
#TOTAL PER SF missing: AH, CC, CL, CM, CP, RL (many E)
#HEAT, COOL, EE missing: all but R1-3
#AGE missing: AH, CC, CL, CP, RL (many E)

#And "AV_BLDG_PER_SF" will give us the assessed value per square foot of a building:
PA <- transform(PA, AV_BLDG_PER_SF = ifelse((AV_BLDG != 0 & GROSS_AREA != 0), AV_BLDG / GROSS_AREA, NA))
PA <- transform(PA, AV_LAND_PER_SF =  ifelse((AV_LAND != 0 & LAND_SF != 0), AV_TOTAL / LAND_SF,NA))



simplify_LU <- function(LU) {
  if (LU %in% c("R1", "R2", "R3", "R4", "RL", "A")) {
    return("RESIDENTIAL")
  } else if (LU %in% c("CM", "CP")) {
    return("CONDO")
  } else if (LU == "CD") {
    return("CONDO_UNIT")
  } else if (LU == "RC") {
    return("MIX_RC")
  } else if (LU %in% c("CC", "C", "CL")) {
    return("COMMERCIAL")
  } else if (LU == "AH") {
    return("AGRICULTURAL")
  } else if (LU == "I") {
    return("INDUSTRIAL")
  } else if (LU == "E") {
    return("TAX_EXEMPT")
  } else if (LU == "EA") {
    return("TAX_EXEMPT_BRA")
  } else {
    return(NA)
  }
}

#Create a new column by applying the simplifyLU function
PA <- transform(PA, SIMPLIFIED_LU = sapply(LU, simplify_LU))


varnames = c("parcel_num","CM_ID","GIS_ID" , "ST_NUM","ST_NAME","ST_NAME_SUF","UNIT_NUM","ZIPCODE","PTYPE","LU","OWN_OCC",
             "OWNER","MAIL_ADDRESSEE", "MAIL_ADDRESS","MAIL.CS","MAIL_ZIPCODE","AV_LAND","AV_BLDG","AV_TOTAL","GROSS_TAX","LAND_SF",
             "YR_BUILT","YR_REMOD","GROSS_AREA","NUM_FLOORS","STRUCTURE_CLASS","R_BLDG_STYL","R_ROOF_TYP","R_EXT_FIN","R_TOTAL_RMS",
             "R_BDRMS","R_FULL_BTH","R_HALF_BTH","R_BTH_STYLE","R_BTH_STYLE2","R_BTH_STYLE3","R_KITCH","R_KITCH_STYLE","R_KITCH_STYLE2","R_KITCH_STYLE3",
             "R_HEAT_TYP","R_AC","R_FPLACE","R_EXT_CND","R_OVRALL_CND","R_INT_CND","R_INT_FIN","R_VIEW",    
             "S_NUM_BLDG","S_BLDG_STYL","S_UNIT_RES","S_UNIT_COM","S_UNIT_RC","S_EXT_FIN",  "S_EXT_CND"  ,
             "U_BASE_FLOOR","U_NUM_PARK","U_CORNER","U_ORIENT","U_TOT_RMS","U_BDRMS",
             "U_FULL_BTH","U_HALF_BTH",  "U_BTH_STYLE","U_BTH_STYLE2","U_BTH_STYLE3","U_KITCH_TYPE","U_KITCH_STYLE",
             "U_HEAT_TYP","U_AC","U_FPLACE","U_INT_FIN","U_INT_CND","U_VIEW",   
             "LIVING_AREA","AV_BLDG_PER_SF","AV_LAND_PER_SF", "SIMPLIFIED_LU","EE_SCORE","BLDG_AGE",
             "X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"            )


setdiff(names(PA),varnames)
PA = PA[,varnames]

write.csv(PA,PA17_final_path,row.names=F)



#### AGGREGATE ####

PA = PA

#HEAT, COOL, EE missing: all but R1-3
#BLDG/LAND PER SF missing: AH, CC, CL, CM, CP, RL (many E)
#AGE missing: AH, CC, CL, CP, RL (many E)

#HEAT/COOL - only R1-3 (but syntax is in same form as below so everything is clear)
ee_res = PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3"
PA$EE_SCORE.res[ee_res]=PA$EE_SCORE[ee_res]


#VALUATIONS PER SF - separate for residential and non-res, for land usages which values consistently exist
valuation_res = (PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" | PA$LU == "CD")
PA$AV_LAND_PER_SF.res[valuation_res] = PA$AV_LAND_PER_SF[valuation_res]
PA$AV_BLDG_PER_SF.res[valuation_res] =  PA$AV_BLDG_PER_SF[ valuation_res]

valuation_nonres = (PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" | PA$LU == "RC" )
PA$AV_LAND_PER_SF.nonres[valuation_nonres] = PA$AV_LAND_PER_SF[valuation_nonres]
PA$AV_BLDG_PER_SF.nonres[valuation_nonres] =  PA$AV_BLDG_PER_SF[ valuation_nonres]

#AGE  - separate for residential and non-res, for land usages which values consistently exist
PA$YR_BUILT_REMOD = ifelse(is.na(PA$YR_REMOD),PA$YR_BUILT,PA$YR_REMOD)
PA$DEC_BUILT_REMOD = floor(PA$YR_BUILT_REMOD/10)*10

age_res = (PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" | PA$LU == "CD" | PA$LU == "CM")
PA$YR_BUILT_REMOD.res[age_res] = PA$YR_BUILT_REMOD[age_res]
PA$DEC_BUILT_REMOD.res[age_res] = PA$DEC_BUILT_REMOD[age_res]

age_nonres = (PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" | PA$LU == "RC" )
PA$YR_BUILT_REMOD.nonres[age_nonres] = PA$YR_BUILT_REMOD[age_nonres]
PA$DEC_BUILT_REMOD.nonres[age_nonres] = PA$DEC_BUILT_REMOD[age_nonres]



# aggregate by census tract
PA.agg.CT.mean = aggregate(cbind(EE_SCORE.res, AV_LAND_PER_SF.res,AV_LAND_PER_SF.nonres,AV_BLDG_PER_SF.res,AV_BLDG_PER_SF.nonres,
                                 YR_BUILT_REMOD.res,YR_BUILT_REMOD.nonres)~CT_ID_10,
                           data=PA,FUN=mean,na.action=na.pass, na.rm=T)

PA.agg.CT.mode = aggregate(cbind(DEC_BUILT_REMOD.res,DEC_BUILT_REMOD.nonres)~CT_ID_10,
                           data=PA,FUN=Mode,na.action=na.pass)

PA.agg.CT = merge(PA.agg.CT.mean,PA.agg.CT.mode,by="CT_ID_10",all=T)

# write census tract aggregation
write.csv( PA.agg.CT,ct_path,row.names=F)

# rename the tract aggregates so the names are short enough for the shape file
PA.agg.CT.rename = rename(PA.agg.CT,EESR = EE_SCORE.res, ALPSFR = AV_LAND_PER_SF.res, ALPSFN = AV_LAND_PER_SF.nonres,
                          ABPSFR = AV_BLDG_PER_SF.res, ABPSFN = AV_BLDG_PER_SF.nonres, YBRR = YR_BUILT_REMOD.res, YBRN = YR_BUILT_REMOD.nonres, DBRR = DEC_BUILT_REMOD.res, DBRN = DEC_BUILT_REMOD.nonres)

# merge to tract shapefile 
PA.agg.CT.shp = merge(tractsShp, PA.agg.CT.rename, by="CT_ID_10",all.x=T)
writeOGR(PA.agg.CT.shp,ctshp_path,ctshp_name,driver="ESRI Shapefile",overwrite_layer=TRUE)

# aggregate be census block group
PA.agg.CBG.mean = aggregate(cbind(EE_SCORE.res, AV_LAND_PER_SF.res,AV_LAND_PER_SF.nonres,AV_BLDG_PER_SF.res,AV_BLDG_PER_SF.nonres,
                                  YR_BUILT_REMOD.res,YR_BUILT_REMOD.nonres)~BG_ID_10,
                            data=PA,FUN=mean,na.action=na.pass, na.rm=T)

PA.agg.CBG.mode = aggregate(cbind(DEC_BUILT_REMOD.res,DEC_BUILT_REMOD.nonres)~BG_ID_10,
                            data=PA,FUN=Mode,na.action=na.pass)

PA.agg.CBG = merge(PA.agg.CBG.mean,PA.agg.CBG.mode,by="BG_ID_10",all=T)

# write cbg file
write.csv( PA.agg.CBG,bg_path,row.names=F)

# rename so that variable names are not cut off in shape file
PA.agg.CBG.rename = rename(PA.agg.CBG,EESR = EE_SCORE.res, ALPSFR = AV_LAND_PER_SF.res, ALPSFN = AV_LAND_PER_SF.nonres,
                           ABPSFR = AV_BLDG_PER_SF.res, ABPSFN = AV_BLDG_PER_SF.nonres, YBRR = YR_BUILT_REMOD.res, 
                           YBRN = YR_BUILT_REMOD.nonres, DBRR = DEC_BUILT_REMOD.res, DBRN = DEC_BUILT_REMOD.nonres)

PA.agg.CBG.shp = merge(bgsShp, PA.agg.CBG.rename, by="BG_ID_10",all.x=T)

# write cbg shapefile
writeOGR(PA.agg.CBG.shp,bgshp_path,bgshp_name,driver="ESRI Shapefile",overwrite_layer=TRUE)

