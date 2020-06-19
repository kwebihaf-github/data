# mapping, start

# example used is , hts_index_facility. mapping IMPACT indicators to DATIM4U indicators


#Install Packages

.packages <- c("here", "dplyr","tidyr", "fuzzyjoin","stringdist")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)


# read csv file. metadata
IndicatorCodeList_FY2020Q2_datim4u <-read.csv(here::here("files","for-mappings","FY2020","Q2","_DATIM4U_IndicatorCodeList_Facility_and_Community_FY2020Q2_2020-05-28_13_22_EAT.csv"),
                            header = TRUE, 
                            sep = ",",
                            stringsAsFactors = FALSE,
                            quote="\"")

IndicatorCodeList_FY2020Q2_impact <-read.csv(here::here("files","for-mappings","FY2020","Q2","Revised-MER-2.4-Community-and-Facility-IndicatorCodeList.csv"),
                                              header = TRUE, 
                                             sep = ",", 
                                             stringsAsFactors = FALSE,
                                             quote="\"")

# add concatenate columns
IndicatorCodeList_FY2020Q2_datim4u <- IndicatorCodeList_FY2020Q2_datim4u %>%
                                          tidyr::unite(Concatenate_1, "dataelement.DSD","categoryoptioncombo.DSD", sep=";", remove=FALSE)

# filter for the required indicators

# hts_index_facility
IndicatorCodeList_FY2020Q2_impact <- IndicatorCodeList_FY2020Q2_impact %>% 
  tidyr::unite(Concatenate_1, "dataelement","categoryoptioncombo", sep=";", remove=FALSE) %>% 
  dplyr::filter(dataset=="MER v24 Results 2019 Civilian" & grepl("HTS_INDEX",dataelement))


IndicatorCodeList_FY2020Q2_datim4u <- IndicatorCodeList_FY2020Q2_datim4u %>% 
  tidyr::unite(Concatenate_1, "dataelement.DSD","categoryoptioncombo.DSD", sep=";", remove=FALSE) %>% 
  dplyr::filter(dataset=="MER Results: Facility Based" & grepl("HTS_INDEX",dataelement.DSD))


# subsetting indicator categories. real mapping takes place here. do one sub-category at a time.

# offered for hts_index
joined <-IndicatorCodeList_FY2020Q2_impact %>% 
  fuzzyjoin::stringdist_left_join(IndicatorCodeList_FY2020Q2_datim4u, 
                                  by=c(categoryoptioncombo="categoryoptioncombo.DSD"),
                                  distance_col="dist", method="jaccard", 
                                  max_dist=0.6, ignore_case = TRUE) %>%  # adjust max_dist appropriately
  dplyr::select(dataset.x,dataelement, deuid,categoryoptioncombo,cocuid,dataset.y,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dist) %>% 
  dplyr::filter(grepl("Offered Testing",categoryoptioncombo) & grepl("Offered",dataelement.DSD))
 

joined_female_<-joined %>% 
                    dplyr::filter(grepl(", Female",categoryoptioncombo) & grepl(", Female",categoryoptioncombo.DSD) & dist<=0.48)

joined_female_lessthan_1yr<-joined %>% 
  dplyr::filter(grepl("<1 yr, Female",categoryoptioncombo) & grepl("<1, Female",categoryoptioncombo.DSD) & dist<=0.5)
 
joined_female <-rbind(joined_female_,joined_female_lessthan_1yr)

joined_male_<-joined %>% 
  dplyr::filter(grepl(", Male",categoryoptioncombo) & grepl(", Male",categoryoptioncombo.DSD) & dist<=0.53)

joined_male_lessthan_1yr<-joined %>% 
  dplyr::filter(grepl("<1 yr, Male",categoryoptioncombo) & grepl("<1, Male",categoryoptioncombo.DSD) & dist<=0.56)

joined_male <-rbind(joined_male_,joined_male_lessthan_1yr)


# accepted for hts_index
joined_accepted <-IndicatorCodeList_FY2020Q2_impact %>% 
  fuzzyjoin::stringdist_left_join(IndicatorCodeList_FY2020Q2_datim4u, 
                                  by=c(categoryoptioncombo="categoryoptioncombo.DSD"),
                                  distance_col="dist", method="jaccard", 
                                  max_dist=0.6, ignore_case = TRUE) %>% 
  dplyr::select(dataset.x,dataelement, deuid,categoryoptioncombo,cocuid,dataset.y,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dist) %>% 
  dplyr::filter(grepl("Accepted Testing",categoryoptioncombo) & grepl("Accepted",dataelement.DSD))


joined_female_<-joined_accepted %>% 
  dplyr::filter(grepl(", Female",categoryoptioncombo) & grepl(", Female",categoryoptioncombo.DSD) & dist<=0.5)

joined_female_lessthan_1yr<-joined_accepted %>% 
  dplyr::filter(grepl("<1 yr, Female",categoryoptioncombo) & grepl("<1, Female",categoryoptioncombo.DSD) & dist<=0.53)

joined_female <-rbind(joined_female_,joined_female_lessthan_1yr)

joined_male_<-joined_accepted %>% 
  dplyr::filter(grepl(", Male",categoryoptioncombo) & grepl(", Male",categoryoptioncombo.DSD) & dist<=0.53)

joined_male_lessthan_1yr<-joined_accepted %>% 
  dplyr::filter(grepl("<1 yr, Male",categoryoptioncombo) & grepl("<1, Male",categoryoptioncombo.DSD) & dist<=0.56)

joined_male <-rbind(joined_male_,joined_male_lessthan_1yr)

# elicited for hts_index
joined_elicited <-IndicatorCodeList_FY2020Q2_impact %>% 
  fuzzyjoin::stringdist_left_join(IndicatorCodeList_FY2020Q2_datim4u, 
                                  by=c(categoryoptioncombo="categoryoptioncombo.DSD"),
                                  distance_col="dist", method="jaccard", 
                                  max_dist=0.6, ignore_case = TRUE) %>% 
  dplyr::select(dataset.x,dataelement, deuid,categoryoptioncombo,cocuid,dataset.y,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dist) %>% 
  dplyr::filter(grepl("_ELICITED",dataelement) & grepl("Age Aggregated",dataelement.DSD))


joined_female_<-joined_elicited %>% 
  dplyr::filter(grepl(", Female",categoryoptioncombo) & grepl(", Female",categoryoptioncombo.DSD) & dist<=0.2)

joined_male_<-joined_elicited %>% 
  dplyr::filter(grepl(", Male",categoryoptioncombo) & grepl(", Male",categoryoptioncombo.DSD) & dist<=0.2)

# known positive for hts_index
joined_known_positive <-IndicatorCodeList_FY2020Q2_impact %>% 
  fuzzyjoin::stringdist_left_join(IndicatorCodeList_FY2020Q2_datim4u, 
                                  by=c(categoryoptioncombo="categoryoptioncombo.DSD"),
                                  distance_col="dist", method="jaccard", 
                                  max_dist=0.6, ignore_case = TRUE) %>% 
  dplyr::select(dataset.x,dataelement, deuid,categoryoptioncombo,cocuid,dataset.y,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dist) %>% 
  dplyr::filter(grepl("Known Positive",categoryoptioncombo) & grepl("Known at Entry Positive",categoryoptioncombo.DSD))


joined_female_<-joined_known_positive %>% 
  dplyr::filter(grepl(", Female",categoryoptioncombo) & grepl(", Female",categoryoptioncombo.DSD) & dist==0.0)

 joined_female_unknown<-joined_known_positive %>% 
   dplyr::filter(grepl("Unknown Age, Female",categoryoptioncombo) & grepl("Unknown Age,",categoryoptioncombo.DSD) & dist<=0.1)

 joined_female <-rbind(joined_female_,joined_female_unknown)

joined_male_<-joined_known_positive %>% 
  dplyr::filter(grepl(", Male",categoryoptioncombo) & grepl(", Male",categoryoptioncombo.DSD) & dist<=0.0)

 joined_male_unknown<-joined_known_positive %>% 
  dplyr::filter(grepl("Unknown Age, Male",categoryoptioncombo) & grepl("Unknown Age,",categoryoptioncombo.DSD) & dist<=0.12)
 
 joined_male <-rbind(joined_male_,joined_male_unknown)
 
 # newly tested positive for hts_index
 joined_newly_tested_positive <-IndicatorCodeList_FY2020Q2_impact %>% 
   fuzzyjoin::stringdist_left_join(IndicatorCodeList_FY2020Q2_datim4u, 
                                   by=c(categoryoptioncombo="categoryoptioncombo.DSD"),
                                   distance_col="dist", method="jaccard", 
                                   max_dist=0.6, ignore_case = TRUE) %>% 
   dplyr::select(dataset.x,dataelement, deuid,categoryoptioncombo,cocuid,dataset.y,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dist) %>% 
   dplyr::filter(grepl("Newly Identified Positive",categoryoptioncombo) & grepl("Newly Identified Positive",categoryoptioncombo.DSD))
 
 
 joined_female_<-joined_newly_tested_positive %>% 
   dplyr::filter(grepl(", Female",categoryoptioncombo) & grepl(", Female",categoryoptioncombo.DSD) & dist<=0.048 & !grepl("Index",categoryoptioncombo))
 
 joined_female_lessthan_1yr<-joined_newly_tested_positive %>% 
   dplyr::filter(grepl("<1 yr, Female",categoryoptioncombo) & grepl("<1, Newly Identified Positive, Female",categoryoptioncombo.DSD) )
 
 joined_female <-rbind(joined_female_,joined_female_lessthan_1yr)
 
 joined_male_<-joined_newly_tested_positive %>% 
   dplyr::filter(grepl(", Male",categoryoptioncombo) & grepl(", Male",categoryoptioncombo.DSD) & dist<=0.048 & !grepl("Index",categoryoptioncombo))
 
 joined_male_lessthan_1yr<-joined_newly_tested_positive %>% 
   dplyr::filter(grepl("<1 yr, Male",categoryoptioncombo) & grepl("<1, Newly Identified Positive, Male",categoryoptioncombo.DSD) )
 
 joined_male <-rbind(joined_male_,joined_male_lessthan_1yr)
 
 
 # newly tested negative for hts_index
 joined_newly_tested_negative <-IndicatorCodeList_FY2020Q2_impact %>% 
   fuzzyjoin::stringdist_left_join(IndicatorCodeList_FY2020Q2_datim4u, 
                                   by=c(categoryoptioncombo="categoryoptioncombo.DSD"),
                                   distance_col="dist", method="jaccard", 
                                   max_dist=0.6, ignore_case = TRUE) %>% 
   dplyr::select(dataset.x,dataelement, deuid,categoryoptioncombo,cocuid,dataset.y,dataelement.DSD,dataelementuid.DSD,categoryoptioncombo.DSD,categoryoptioncombouid.DSD,RETURN_FIELD_DSD,dist) %>% 
   dplyr::filter(grepl("Newly Identified Negative",categoryoptioncombo) & grepl("Newly Identified Negative",categoryoptioncombo.DSD))
 
 
 joined_female_<-joined_newly_tested_negative %>% 
   dplyr::filter(grepl(", Female",categoryoptioncombo) & grepl("Newly Identified Negative, Female",categoryoptioncombo.DSD) & dist<=0.06 & !grepl("Index",categoryoptioncombo) )
 
 joined_male_<-joined_newly_tested_negative %>% 
   dplyr::filter(grepl(", Male",categoryoptioncombo) & grepl("Newly Identified Negative, Male",categoryoptioncombo.DSD) & dist<=0.06 & !grepl("Index",categoryoptioncombo))
 
  
# binding together the split categories
hts_index_offered_facility <-rbind(joined_female,joined_male)
hts_index_accepted_facility <-rbind(joined_female,joined_male)
hts_index_elicited_facility <-rbind(joined_female_,joined_male_)
hts_index_known_positive_facility <-rbind(joined_female,joined_male)
hts_index_newly_tested_positive_facility <-rbind(joined_female,joined_male)
hts_index_newly_tested_negative_facility <-rbind(joined_female_,joined_male_)


# merging together the indicator categories

# hts_index_facility
 hts_index_facility <-do.call("rbind",list(hts_index_offered_facility,
                                           hts_index_accepted_facility,
                                           hts_index_elicited_facility,
                                           hts_index_known_positive_facility,
                                           hts_index_newly_tested_positive_facility,
                                           hts_index_newly_tested_negative_facility))
 
 hts_index_facility <- hts_index_facility %>%  tidyr::unite(Concatenate ,"deuid","cocuid", sep=";", remove=FALSE) # will be used in the indicator coding script or part or tool
 
 
 # mapping, end