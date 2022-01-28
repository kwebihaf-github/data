# read csv from url
FacilityCodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:zL8TlPVzEBZ",stringsAsFactors = F,sep=',')
CommunityCodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:TBcmmtoaCBC",stringsAsFactors = F,sep=',')

AGYW_PREV_CodeList <-read.csv("https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:qHyrHc4zwx4",stringsAsFactors = F,sep=',')

# Add conditional column for reporting frequency
FacilityCodeList$ReportingFreq <- ifelse(grepl("HTS|TX_NEW|TX_CURR|TX_RTT|TX_ML|TX_PVLS|TX_ML|VMMC_CIRC|PMTCT|TB_ART|TB_STAT", FacilityCodeList$dataelement) , "Quarterly", 
                                         ifelse(grepl("TX_TB|TB_PREV|KP_PREV|PP_PREV|AGYW_PREV|PrEP_CURR|PrEP_NEW|OVC_SERV|OVC_HIVSTAT|SC_ARVDISP|SC_CURR|CXCA_TX|CXCA_SCRN", FacilityCodeList$dataelement) , "SemiAnnual", "Annual"))

CommunityCodeList$ReportingFreq <- ifelse(grepl("HTS|TX_NEW|TX_CURR|TX_RTT|TX_ML|TX_PVLS|TX_ML|VMMC_CIRC|PMTCT|TB_ART|TB_STAT", CommunityCodeList$dataelement) , "Quarterly", 
                                          ifelse(grepl("TX_TB|TB_PREV|KP_PREV|PP_PREV|AGYW_PREV|PrEP_CURR|PrEP_NEW|OVC_SERV|OVC_HIVSTAT|SC_ARVDISP|SC_CURR|CXCA_TX|CXCA_SCRN", CommunityCodeList$dataelement) , "SemiAnnual", "Annual"))

# separate into DSD, TA and NoApp
code_DSD_Facility <- subset(FacilityCodeList, grepl("_DSD", code))
rownames(code_DSD_Facility) <- c()

code_DSD_Community <- subset(CommunityCodeList, grepl("_DSD", code))
rownames(code_DSD_Community) <- c()

#
code_TA_Facility <- subset(FacilityCodeList, grepl("_TA", code))
rownames(code_TA_Facility) <- c()

code_TA_Community <- subset(CommunityCodeList, grepl("_TA", code))
rownames(code_TA_Community) <- c()

code_NoApp_Facility <- subset(FacilityCodeList, grepl("_NoApp", code))
rownames(code_NoApp_Facility) <- c()

code_CS_Facility <- subset(FacilityCodeList, grepl("_CS_", code)) # CS means Centrally Supported
rownames(code_CS_Facility) <- c()

code_CS_Community <- subset(CommunityCodeList, grepl("_CS_", code))
rownames(code_CS_Community) <- c()

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


# merge DSD & TA dataframes by a common RowIndex column
code_DSD_TA_Facility<-merge(code_DSD_Facility,code_TA_Facility, by="Concatenate_1")

code_DSD_TA_Community<-merge(code_DSD_Community,code_TA_Community, by="Concatenate_1")

# select wanted columns 
code_DSD_TA_Facility <-dplyr::select(code_DSD_TA_Facility,
                                     "dataset.x","dataelement.x","dataelementuid.x","categoryoptioncombo.x","categoryoptioncombouid.x","RETURN_FIELD_DSD","dataelement.y","dataelementuid.y","categoryoptioncombo.y","categoryoptioncombouid.y","RETURN_FIELD_TA", "ReportingFreq.x")

code_DSD_TA_Community <-dplyr::select(code_DSD_TA_Community,
                                      "dataset.x","dataelement.x","dataelementuid.x","categoryoptioncombo.x","categoryoptioncombouid.x","RETURN_FIELD_DSD","dataelement.y","dataelementuid.y","categoryoptioncombo.y","categoryoptioncombouid.y","RETURN_FIELD_TA", "ReportingFreq.x")

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
code_NoApp_Facility$RETURN_FIELD_DSD = paste(code_NoApp_Facility$`dataelementuid`,";",code_NoApp_Facility$`categoryoptioncombouid`)

code_CS_Facility$RETURN_FIELD_DSD = paste(code_CS_Facility$`dataelementuid`,";",code_CS_Facility$`categoryoptioncombouid`)
code_CS_Community$RETURN_FIELD_DSD = paste(code_CS_Community$`dataelementuid`,";",code_CS_Community$`categoryoptioncombouid`)

AGYW_PREV_CodeList$RETURN_FIELD_DSD = paste(AGYW_PREV_CodeList$`dataelementuid`,";",AGYW_PREV_CodeList$`categoryoptioncombouid`)
# check for duplicates
code_DSD_TA_Facility %>% 
  group_by(dataset,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dataelement.TA,dataelementuid.TA,categoryoptioncombo.TA,categoryoptioncombouid.TA,RETURN_FIELD_TA,ReportingFreq) %>% 
  dplyr::filter(n()>1)

# add column for Row indexes, i.e CODE column having incrementing numbers
code_DSD_TA_Facility$Indicator_code <- seq.int(nrow(code_DSD_TA_Facility))

code_DSD_TA_Community$Indicator_code <- seq.int(from=nrow(code_DSD_TA_Facility)+1, to=(nrow(code_DSD_TA_Facility)+1+nrow(code_DSD_TA_Community)-1)) # 2875 ## manually indicate the custom from and to ranges

code_NoApp_Facility$Indicator_code <- seq.int(nrow(code_NoApp_Facility))

code_CS_Facility$Indicator_code <- seq.int(nrow(code_CS_Facility))
code_CS_Community$Indicator_code <- seq.int(from=nrow(code_CS_Facility)+1, to=(nrow(code_CS_Facility)+1+nrow(code_CS_Community)-1))

AGYW_PREV_CodeList$Indicator_code <- seq.int(nrow(AGYW_PREV_CodeList))
# remove white spaces in a string
code_DSD_TA_Facility$RETURN_FIELD_DSD <-gsub(" ", "", code_DSD_TA_Facility$RETURN_FIELD_DSD, fixed = TRUE)
code_DSD_TA_Facility$RETURN_FIELD_TA <-gsub(" ", "", code_DSD_TA_Facility$RETURN_FIELD_TA, fixed = TRUE)

code_DSD_TA_Community$RETURN_FIELD_DSD <-gsub(" ", "", code_DSD_TA_Community$RETURN_FIELD_DSD, fixed = TRUE)
code_DSD_TA_Community$RETURN_FIELD_TA <-gsub(" ", "", code_DSD_TA_Community$RETURN_FIELD_TA, fixed = TRUE)

code_NoApp_Facility$RETURN_FIELD_DSD <-gsub(" ", "", code_NoApp_Facility$RETURN_FIELD_DSD, fixed = TRUE)

code_CS_Facility$RETURN_FIELD_DSD <-gsub(" ", "", code_CS_Facility$RETURN_FIELD_DSD, fixed = TRUE)
code_CS_Community$RETURN_FIELD_DSD <-gsub(" ", "", code_CS_Community$RETURN_FIELD_DSD, fixed = TRUE)

AGYW_PREV_CodeList$RETURN_FIELD_DSD <-gsub(" ", "", AGYW_PREV_CodeList$RETURN_FIELD_DSD, fixed = TRUE)

# saving the file

# row bind facility and community code list
code_DSD_TA_Facility_and_Community <-rbind(code_DSD_TA_Facility,code_DSD_TA_Community)

code_CS_Facility_and_Community <-rbind(code_CS_Facility,code_CS_Community)

#
write.csv(code_DSD_TA_Facility_and_Community,file=paste0("/home/fred/Downloads/DATIM4U/FY2021/Q1/Metadata/Coding/DATIM4U_IndicatorCodeList_Facility_and_Community_FY2021Q1_DSDTA_", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)

write.csv(code_NoApp_Facility,file=paste0("/home/fred/Downloads/DATIM4U/FY2021/Q1/Metadata/Coding/DATIM4U_IndicatorCodeList_NoApp_FY2021_", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)

write.csv(code_CS_Facility_and_Community,file=paste0("/home/fred/Downloads/DATIM4U/FY2021/Q2/Metadata/Coding/DATIM4U_IndicatorCodeList_CS_FY2021Q1_", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)

write.csv(AGYW_PREV_CodeList,file=paste0("/home/fred/Downloads/DATIM4U/FY2021/Q1/Metadata/Coding/DATIM4U_IndicatorCodeList_AGYW_PREV_FY2021_", format(Sys.time(), format = "%F_%R_%Z", tz = "Africa/Kampala"), ".csv"), row.names = FALSE)
