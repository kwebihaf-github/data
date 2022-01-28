## generating Indicator code list for DATIM4U coding, Start
FacilityCodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:jKdHXpBfWop",stringsAsFactors = F,sep=',')
AGYW_PREV_CodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:mbdbMiLZ4AA",stringsAsFactors = F,sep=',')
CommunityCodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:qzVASYuaIey",stringsAsFactors = F,sep=',')

NATCodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:aUmlPChIptc",stringsAsFactors = F,sep=',')
SUBNATCodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:ctKXzmv2CVu",stringsAsFactors = F,sep=',')

# Add conditional column for reporting frequency
FacilityCodeList$ReportingFreq <- ifelse(grepl("HTS|TX_NEW|TX_CURR|TX_RTT|TX_ML|TX_PVLS|TX_ML|VMMC_CIRC|PMTCT|TB_ART|TB_STAT", FacilityCodeList$dataelement) , "Quarterly", 
                                         ifelse(grepl("TX_TB|TB_PREV|KP_PREV|PP_PREV|AGYW_PREV|PrEP_CURR|PrEP_NEW|OVC_SERV|OVC_HIVSTAT|SC_ARVDISP|SC_CURR|CXCA_TX|CXCA_SCRN", FacilityCodeList$dataelement) , "SemiAnnual", "Annual"))

CommunityCodeList$ReportingFreq <- ifelse(grepl("HTS|TX_NEW|TX_CURR|TX_RTT|TX_ML|TX_PVLS|TX_ML|VMMC_CIRC|PMTCT|TB_ART|TB_STAT", CommunityCodeList$dataelement) , "Quarterly", 
                                         ifelse(grepl("TX_TB|TB_PREV|KP_PREV|PP_PREV|AGYW_PREV|PrEP_CURR|PrEP_NEW|OVC_SERV|OVC_HIVSTAT|SC_ARVDISP|SC_CURR|CXCA_TX|CXCA_SCRN", CommunityCodeList$dataelement) , "SemiAnnual", "Annual"))

# Filter rows by subsetting. Pick only rows that have only Quarterly & SemiAnnual
#FacilityCodeList_Quarterly_SemiAnnual <- subset(FacilityCodeList,!grepl("NA", ReportingFreq))

#CommunityCodeList_Quarterly_SemiAnnual <- subset(CommunityCodeList,!grepl("NA", ReportingFreq))

FacilityCodeList_Quarterly_SemiAnnual <-FacilityCodeList
CommunityCodeList_Quarterly_SemiAnnual <- CommunityCodeList

# separate into DSD, TA and NoApp
code_DSD_Facility <- subset(FacilityCodeList_Quarterly_SemiAnnual, grepl("_DSD", code))
rownames(code_DSD_Facility) <- c()
#code_DSD_Facility$RowIndex <- seq.int(nrow(code_DSD_Facility))

code_DSD_Community <- subset(CommunityCodeList_Quarterly_SemiAnnual, grepl("_DSD", code))
rownames(code_DSD_Community) <- c()
#code_DSD_Community$RowIndex <- seq.int(nrow(code_DSD_Community))


code_TA_Facility <- subset(FacilityCodeList_Quarterly_SemiAnnual, grepl("_TA", code))
rownames(code_TA_Facility) <- c()
#code_TA_Facility$RowIndex <- seq.int(nrow(code_TA_Facility))

code_TA_Community <- subset(CommunityCodeList_Quarterly_SemiAnnual, grepl("_TA", code))
rownames(code_TA_Community) <- c()
#code_TA_Community$RowIndex <- seq.int(nrow(code_TA_Community))


code_NoApp_Facility <- subset(FacilityCodeList_Quarterly_SemiAnnual, grepl("_NoApp", code))
rownames(code_NoApp_Facility) <- c()
#code_NoApp_Facility$RowIndex <- seq.int(nrow(code_NoApp_Facility))


# Indicators in the other
#HRH_CURR only, advise is to remove DSD from HRH_CURR naming


# code_NoApp_Community <- subset(CommunityCodeList_Quarterly_SemiAnnual, grepl("_NoApp", code))
# rownames(code_NoApp_Community) <- c()
# code_NoApp_Community$RowIndex <- seq.int(nrow(code_NoApp_Community))


# codeFiles <- "/home/rstudio/files/data/FY2020-CommunityBased-CodeList.csv"
# codeData<-read.csv(codeFiles,stringsAsFactors = F,sep=',')

# code_DSD <- subset(subset(codeData, grepl("HTS_SELF", dataelement)),grepl("DSD", dataelement))
# rownames(code_DSD) <- c() #https://www.nesono.com/node/456, removing row name indexes
# 
# code_DSD$RowIndex <- seq.int(nrow(code_DSD)) #https://stackoverflow.com/questions/23518605/add-an-index-numeric-id-column-to-large-data-frame/23518737
# 
# code_TA <- subset(subset(codeData, grepl("HTS_SELF", dataelement)),grepl("TA", dataelement))
# rownames(code_TA) <- c()
# code_TA$RowIndex <- seq.int(nrow(code_TA))


#
# code_DSD_Facility$RowIndex <- NULL
# code_TA_Facility$RowIndex <- NULL

# connecting DSD and TA, both together, starts here

#code_DSD_Facility$Indi_Code<-gsub( " .*$", "", code_DSD_Facility$dataelement) # getting indicator code e.g CXCA_SCRN
#code_TA_Facility$Indi_Code<-gsub( " .*$", "", code_TA_Facility$dataelement) # getting indicator code e.g CXCA_SCRN

#code_DSD_Community$Indi_Code<-gsub( " .*$", "", code_DSD_Community$dataelement) # getting indicator code e.g CXCA_SCRN
#code_TA_Community$Indi_Code<-gsub( " .*$", "", code_TA_Community$dataelement) # getting indicator code e.g CXCA_SCRN

## multi find and replace, start
mgsub <-function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

#
code_DSD_Facility$de <- mgsub("DSD", "TA",code_DSD_Facility$dataelement) # very important part. connection starts from here
code_DSD_Community$de <- mgsub("DSD", "TA",code_DSD_Community$dataelement) # very important part. connection starts from here

library(dplyr)
code_DSD_Facility <- code_DSD_Facility %>% 
 # tidyr::unite(Concatenate_1, "Indi_Code","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(Concatenate_1, "de","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(RETURN_FIELD_DSD, "dataelementuid","categoryoptioncombouid", sep=";", remove=FALSE)

code_TA_Facility <- code_TA_Facility %>% 
  #tidyr::unite(Concatenate_1, "Indi_Code","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(Concatenate_1, "dataelement","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(RETURN_FIELD_TA, "dataelementuid","categoryoptioncombouid", sep=";", remove=FALSE)


code_DSD_Community <- code_DSD_Community %>% 
  #tidyr::unite(Concatenate_1, "Indi_Code","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(Concatenate_1, "de","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(RETURN_FIELD_DSD, "dataelementuid","categoryoptioncombouid", sep=";", remove=FALSE)

code_TA_Community <- code_TA_Community %>% 
  #tidyr::unite(Concatenate_1, "Indi_Code","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(Concatenate_1, "dataelement","categoryoptioncombouid", sep=";", remove=FALSE) %>%
  tidyr::unite(RETURN_FIELD_TA, "dataelementuid","categoryoptioncombouid", sep=";", remove=FALSE)

# HRH_CURR for both facility and community
HRH_CURR_Facility <- code_DSD_Facility %>%
  dplyr::filter(grepl("HRH_CURR",dataelement))
  
HRH_CURR_Community <- code_DSD_Community %>%
  dplyr::filter(grepl("HRH_CURR",dataelement))

# row bind facility and community code list
HRH_CURR_Facility_and_Community <-rbind(HRH_CURR_Facility,HRH_CURR_Community)





# merge DSD & TA dataframes by a common RowIndex column
#code_DSD_TA_Facility<-merge(code_DSD_Facility,code_TA_Facility, by="RowIndex", all.x=TRUE)
code_DSD_TA_Facility<-merge(code_DSD_Facility,code_TA_Facility, by="Concatenate_1")

#code_DSD_TA_Community<-merge(code_DSD_Community,code_TA_Community, by="RowIndex", all.x=TRUE)
code_DSD_TA_Community<-merge(code_DSD_Community,code_TA_Community, by="Concatenate_1")


#code_DSD_TA<-merge(code_DSD,code_TA, by="RowIndex", all.x=TRUE)

# select wanted columns 
code_DSD_TA_Facility <-dplyr::select(code_DSD_TA_Facility,
"dataset.x","dataelement.x","dataelementuid.x","categoryoptioncombo.x","categoryoptioncombouid.x","RETURN_FIELD_DSD","dataelement.y","dataelementuid.y","categoryoptioncombo.y","categoryoptioncombouid.y","RETURN_FIELD_TA", "ReportingFreq.x")

# code_DSD_TA_Community <-dplyr::select(code_DSD_TA_Community,
#                                      "dataset.x","dataelement.x","dataelementuid.x","categoryoptioncombo.x","categoryoptioncombouid.x","dataelement.y","dataelementuid.y","categoryoptioncombo.y","categoryoptioncombouid.y", "ReportingFreq.x")

code_DSD_TA_Community <-dplyr::select(code_DSD_TA_Community,
                                     "dataset.x","dataelement.x","dataelementuid.x","categoryoptioncombo.x","categoryoptioncombouid.x","RETURN_FIELD_DSD","dataelement.y","dataelementuid.y","categoryoptioncombo.y","categoryoptioncombouid.y","RETURN_FIELD_TA", "ReportingFreq.x")

code_NoApp_Facility <-dplyr::select(code_NoApp_Facility,
                                     c(1:9))

HRH_CURR_Facility_and_Community <-dplyr::select(HRH_CURR_Facility_and_Community,
                                      "dataset","dataelement","dataelementuid","categoryoptioncombo","categoryoptioncombouid","RETURN_FIELD_DSD","ReportingFreq")


# code_DSD_TA <-dplyr::select(code_DSD_TA,
#                             "dataset.x","dataelement.x","dataelementuid.x","categoryoptioncombo.x","categoryoptioncombouid.x","dataelement.y","dataelementuid.y","categoryoptioncombo.y","categoryoptioncombouid.y")

#rename columns
code_DSD_TA_Facility <-dplyr::rename(code_DSD_TA_Facility,
dataset=dataset.x,
dataelement.DSD=dataelement.x,
dataelementuid.DSD=dataelementuid.x,
categoryoptioncombo.DSD=categoryoptioncombo.x,
categoryoptioncombouid.DSD=categoryoptioncombouid.x,
dataelement.TA=dataelement.y,
dataelementuid.TA=dataelementuid.y,
categoryoptioncombo.TA=categoryoptioncombo.y,
categoryoptioncombouid.TA=categoryoptioncombouid.y,
ReportingFreq=ReportingFreq.x
)

# code_DSD_TA_Community <-dplyr::rename(code_DSD_TA_Community,
#                                      dataset=dataset.x,
#                                      dataelement.DSD=dataelement.x,
#                                      dataelementuid.DSD=dataelementuid.x,
#                                      categoryoptioncombo.DSD=categoryoptioncombo.x,
#                                      categoryoptioncombouid.DSD=categoryoptioncombouid.x,
#                                      dataelement.TA=dataelement.y,
#                                      dataelementuid.TA=dataelementuid.y,
#                                      categoryoptioncombo.TA=categoryoptioncombo.y,
#                                      categoryoptioncombouid.TA=categoryoptioncombouid.y,
#                                      ReportingFreq=ReportingFreq.x
# )

code_DSD_TA_Community <-dplyr::rename(code_DSD_TA_Community,
                                     dataset=dataset.x,
                                     dataelement.DSD=dataelement.x,
                                     dataelementuid.DSD=dataelementuid.x,
                                     categoryoptioncombo.DSD=categoryoptioncombo.x,
                                     categoryoptioncombouid.DSD=categoryoptioncombouid.x,
                                     dataelement.TA=dataelement.y,
                                     dataelementuid.TA=dataelementuid.y,
                                     categoryoptioncombo.TA=categoryoptioncombo.y,
                                     categoryoptioncombouid.TA=categoryoptioncombouid.y,
                                     ReportingFreq=ReportingFreq.x
)
# Concatenate two string columns with ";"


#--http://www.datasciencemadesimple.com/concatenate-two-columns-of-dataframe-in-r-2

# code_DSD_TA_Facility$RETURN_FIELD_DSD = paste(code_DSD_TA_Facility$`dataelementuid.DSD`,";",code_DSD_TA_Facility$`categoryoptioncombouid.DSD`)
# code_DSD_TA_Facility$RETURN_FIELD_TA = paste(code_DSD_TA_Facility$`dataelementuid.TA`,";",code_DSD_TA_Facility$`categoryoptioncombouid.TA`)

# code_DSD_TA_Community$RETURN_FIELD_DSD = paste(code_DSD_TA_Community$`dataelementuid.DSD`,";",code_DSD_TA_Community$`categoryoptioncombouid.DSD`)
# code_DSD_TA_Community$RETURN_FIELD_TA = paste(code_DSD_TA_Community$`dataelementuid.TA`,";",code_DSD_TA_Community$`categoryoptioncombouid.TA`)

code_NoApp_Facility$RETURN_FIELD_DSD = paste(code_NoApp_Facility$`dataelementuid`,";",code_NoApp_Facility$`categoryoptioncombouid`)

AGYW_PREV_CodeList$RETURN_FIELD_DSD = paste(AGYW_PREV_CodeList$`dataelementuid`,";",AGYW_PREV_CodeList$`categoryoptioncombouid`)

NATCodeList$RETURN_FIELD_DSD = paste(NATCodeList$`dataelementuid`,";",NATCodeList$`categoryoptioncombouid`)
SUBNATCodeList$RETURN_FIELD_DSD = paste(SUBNATCodeList$`dataelementuid`,";",SUBNATCodeList$`categoryoptioncombouid`)

# code_DSD_TA$RETURN_FIELD_DSD = paste(code_DSD_TA$`dataelementuid.DSD`,";",code_DSD_TA$`categoryoptioncombouid.DSD`)
# code_DSD_TA$RETURN_FIELD_TA = paste(code_DSD_TA$`dataelementuid.TA`,";",code_DSD_TA$`categoryoptioncombouid.TA`)

# add columns category & tech_area
# code_DSD_TA$category <- "Testing"
# code_DSD_TA$tech_area <- "HTS"

# rename the dataframe
# HTS_SELF_Community <-code_DSD_TA
# 
# # combine dataframes
# IndicatorCodeList_Facility_FY2020Q1 <-rbind(VMMC_CIRC,
# HTS_TST_Facility,HTS_INDEX_Facility,HTS_RECENT_Facility,HTS_SELF_Facility,
# TX_NEW_Facility,TX_CURR_Facility,TX_RTT_Facility,TX_ML_Facility,TX_PVLS_Facility,
# PMTCT_STAT_Facility,PMTCT_EID_Facility,PMTCT_HEI_POS_Facility,PMTCT_ART_Facility,
# TB_ART_Facility,TB_STAT_Facility)
# 
# 
# IndicatorCodeList_Community_FY2020Q1 <-rbind(HTS_INDEX_Community,
# HTS_RECENT_Community,
# HTS_SELF_Community,
# HTS_TST_Community)

# check for duplicates
code_DSD_TA_Facility %>% 
  group_by(dataset,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dataelement.TA,dataelementuid.TA,categoryoptioncombo.TA,categoryoptioncombouid.TA,RETURN_FIELD_TA,ReportingFreq) %>% 
  dplyr::filter(n()>1)

# add column for Row indexes, i.e CODE column having incrementing numbers
code_DSD_TA_Facility$Indicator_code <- seq.int(nrow(code_DSD_TA_Facility))

NATCodeList$Indicator_code <- seq.int(nrow(NATCodeList))
SUBNATCodeList$Indicator_code <- seq.int(nrow(SUBNATCodeList))

#code_DSD_TA_Community$Indicator_code <- seq.int(from=2193, to=2874) # manually indicate the custom from and to ranges

code_DSD_TA_Community$Indicator_code <- seq.int(from=nrow(code_DSD_TA_Facility)+1, to=(nrow(code_DSD_TA_Facility)+1+nrow(code_DSD_TA_Community)-1)) # 2875 ## manually indicate the custom from and to ranges

#code_DSD_TA_Community$Indicator_code <- NULL 



code_NoApp_Facility$Indicator_code <- seq.int(nrow(code_NoApp_Facility))

AGYW_PREV_CodeList$Indicator_code <- seq.int(nrow(AGYW_PREV_CodeList))

HRH_CURR_Facility_and_Community$Indicator_code <- seq.int(nrow(HRH_CURR_Facility_and_Community))

# IndicatorCodeList_Facility_FY2020Q1$CODE <- seq.int(nrow(IndicatorCodeList_Facility_FY2020Q1))
# 
# IndicatorCodeList_Community_FY2020Q1$CODE <- seq.int(nrow(IndicatorCodeList_Community_FY2020Q1))


# remove white spaces in a string
# --https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string
code_DSD_TA_Facility$RETURN_FIELD_DSD <-gsub(" ", "", code_DSD_TA_Facility$RETURN_FIELD_DSD, fixed = TRUE)
code_DSD_TA_Facility$RETURN_FIELD_TA <-gsub(" ", "", code_DSD_TA_Facility$RETURN_FIELD_TA, fixed = TRUE)

code_DSD_TA_Community$RETURN_FIELD_DSD <-gsub(" ", "", code_DSD_TA_Community$RETURN_FIELD_DSD, fixed = TRUE)
code_DSD_TA_Community$RETURN_FIELD_TA <-gsub(" ", "", code_DSD_TA_Community$RETURN_FIELD_TA, fixed = TRUE)


code_NoApp_Facility$RETURN_FIELD_DSD <-gsub(" ", "", code_NoApp_Facility$RETURN_FIELD_DSD, fixed = TRUE)

AGYW_PREV_CodeList$RETURN_FIELD_DSD <-gsub(" ", "", AGYW_PREV_CodeList$RETURN_FIELD_DSD, fixed = TRUE)

HRH_CURR_Facility_and_Community$RETURN_FIELD_DSD <-gsub(" ", "", HRH_CURR_Facility_and_Community$RETURN_FIELD_DSD, fixed = TRUE)


# IndicatorCodeList_Facility_FY2020Q1$RETURN_FIELD_DSD <-gsub(" ", "", IndicatorCodeList_Facility_FY2020Q1$RETURN_FIELD_DSD, fixed = TRUE)
# IndicatorCodeList_Facility_FY2020Q1$RETURN_FIELD_TA <-gsub(" ", "", IndicatorCodeList_Facility_FY2020Q1$RETURN_FIELD_TA, fixed = TRUE)
# 
# IndicatorCodeList_Community_FY2020Q1$RETURN_FIELD_DSD <-gsub(" ", "", IndicatorCodeList_Community_FY2020Q1$RETURN_FIELD_DSD, fixed = TRUE)
# IndicatorCodeList_Community_FY2020Q1$RETURN_FIELD_TA <-gsub(" ", "", IndicatorCodeList_Community_FY2020Q1$RETURN_FIELD_TA, fixed = TRUE)



# saving the file

# row bind facility and community code list
code_DSD_TA_Facility_and_Community <-rbind(code_DSD_TA_Facility,code_DSD_TA_Community)

#write.csv(code_DSD_TA_Facility,file=paste0((here::here("files","_DATIM4U_IndicatorCodeList_Facility_and_Community_FY2020Q2_")), format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)
write.csv(code_NoApp_Facility,file=paste0((here::here("files","_DATIM4U_IndicatorCodeList_SC_FY2020Q2_")), format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)
write.csv(AGYW_PREV_CodeList,file=paste0((here::here("files","_DATIM4U_IndicatorCodeList_AGYW_PREV_FY2020Q2_")), format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)

write.csv(code_DSD_TA_Facility_and_Community,file=paste0((here::here("files","_DATIM4U_IndicatorCodeList_Facility_and_Community_FY2020Q2_")), format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)

write.csv(NATCodeList,file=paste0("/home/fred/Downloads/DATIM4U/FY2020/Q4/Metadata/Coding/_DATIM4U_IndicatorCodeList_NAT_FY2020Q4_", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)
write.csv(SUBNATCodeList,file=paste0("/home/fred/Downloads/DATIM4U/FY2020/Q4/Metadata/Coding/_DATIM4U_IndicatorCodeList_SUBNAT_FY2020Q4_", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)


write.csv(code_DSD_TA_Facility_and_Community,file=paste0("/home/fred/Downloads/DATIM4U/FY2020/Q4/Metadata/Coding/_03_DSD_and_TA_DATIM4U_IndicatorCodeList_Facility_and_Community_FY2020Q4", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)
write.csv(HRH_CURR_Facility_and_Community,file=paste0("/home/fred/Downloads/DATIM4U/FY2020/Q4/Metadata/Coding/_Other_DATIM4U_HRH_CURR_Facility_and_Community_FY2020Q4", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)

# write.csv(IndicatorCodeList_Facility_FY2020Q1,file="/home/rstudio/files/data/DATIM4U_IndicatorCodeList_Facility_FY2020Q1.csv", row.names = FALSE)
# write.csv(IndicatorCodeList_Community_FY2020Q1,file="/home/rstudio/files/data/DATIM4U_IndicatorCodeList_Community_FY2020Q1.csv", row.names = FALSE)

# list objects matching a pattern, in R environment
# ls (pattern="_Community")

## generating Indicator code list for DATIM4U coding, End