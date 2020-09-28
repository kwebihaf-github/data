## orgunit matching, HIBRID vs DATIM, start

#  Install Packages

.packages <- c("here", "dplyr", "data.table","tidyr", "fuzzyjoin","stringdist")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# read the source files
#HIBRIDorgunitFile <-"/cloud/project/files/OrgUnitDump-byLevel (HIBRID).csv"
#DATIM4UorgunitFile <-"/cloud/project/files/A flat view of UG (DATIM4U).csv"

HIBRIDorgunitData<-read.csv(here::here("files","OrgUnitDump-byLevel (HIBRID).csv"),header = TRUE,sep = ",",stringsAsFactors = FALSE)
DATIM4UorgunitData<-read.csv(here::here("files","A flat view of UG (DATIM4U).csv"),header = TRUE,sep = ",",stringsAsFactors = FALSE)

# filtering of the datasets. filtering out blanks mainly in DATIM.uid column. This is first level
HIBRIDorgunitDataSubset <-HIBRIDorgunitData %>% 
  dplyr::filter(Level=="5" & `DATIM.uid`=="" & name != "CSO") %>% 
  dplyr::select(Level,uid,name,`DATIM.uid`,District,Subcounty) %>% 
  tidyr::unite(Concatenate_1, name, District, Subcounty, sep=";", remove=FALSE)


DATIM4UorgunitDataSubset <-DATIM4UorgunitData %>% 
  dplyr::filter(level=="7") %>% 
  dplyr::select(level,organisationunituid,name,level5name,level6name) %>% 
  tidyr::unite(Concatenate_1, name, level5name, level6name, sep=";", remove=FALSE) %>% 
  tidyr::unite(Concatenate_2, name, level5name, sep=";", remove=FALSE)

# merging of datasets
HIBRID_DATIM4U_orgunit_merge_1 <-merge(HIBRIDorgunitDataSubset, DATIM4UorgunitDataSubset, by.x="Concatenate_1", by.y="Concatenate_1", all.x = TRUE)

HIBRID_DATIM4U_orgunit_merge_1_Subset <- HIBRID_DATIM4U_orgunit_merge_1 %>% 
  dplyr::filter(is.na(level))

HIBRID_DATIM4U_orgunit_merge_1_Subset <-HIBRID_DATIM4U_orgunit_merge_1_Subset %>% 
  tidyr::unite(Concatenate_2, `name.x`, District, sep=";", remove=FALSE)

HIBRID_DATIM4U_orgunit_merge_2 <-merge(HIBRID_DATIM4U_orgunit_merge_1_Subset, DATIM4UorgunitDataSubset, by.x="Concatenate_2", by.y="Concatenate_2", all.x = TRUE)

HIBRID_DATIM4U_orgunit_merge_2_Subset <- HIBRID_DATIM4U_orgunit_merge_2 %>% 
  dplyr::filter(is.na(level.y))


# export for manual matching
write.csv(HIBRID_DATIM4U_orgunit_merge_2_Subset, file=here::here("files","HIBRID_DATIM4U_orgunit_merge_2_Subset.csv"), row.names = FALSE)

# import after manual matching
HIBRID_DATIM4U_orgunit_merge_2_Subset <-read.csv(here::here("files","HIBRID_DATIM4U_orgunit_merge_2_Subset.csv"), header = TRUE,sep = ",",stringsAsFactors = FALSE)

#matching happens here
HIBRID_DATIM4U_orgunit_matching_1 <- HIBRID_DATIM4U_orgunit_merge_1 %>% 
  dplyr::filter(!is.na(level)) %>% 
  dplyr::select(uid,organisationunituid)

HIBRID_DATIM4U_orgunit_matching_2 <- HIBRID_DATIM4U_orgunit_merge_2 %>% 
  dplyr::filter(!is.na(level.y)) %>% 
  dplyr::select(uid,organisationunituid.y)

HIBRID_DATIM4U_orgunit_matching_3 <- HIBRID_DATIM4U_orgunit_merge_2_Subset %>% 
  dplyr::filter(`DATIM.uid`!="") %>%   
  dplyr::select(uid,DATIM.uid)

#HIBRID_DATIM4U_orgunit_matching_final_2 <- dplyr::bind_rows(list(HIBRID_DATIM4U_orgunit_matching_1,HIBRID_DATIM4U_orgunit_matching_2, HIBRID_DATIM4U_orgunit_matching_3))

# merging of all the matches done above. It includes the manual matches
HIBRID_DATIM4U_orgunit_matching_final <- data.table::rbindlist(list(HIBRID_DATIM4U_orgunit_matching_1,HIBRID_DATIM4U_orgunit_matching_2, HIBRID_DATIM4U_orgunit_matching_3), use.names=FALSE)


# check for duplicates. QC on the final match
HIBRID_DATIM4U_orgunit_matching_final %>% 
  dplyr::group_by(uid) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1)

# remove the select duplicate rows
HIBRID_DATIM4U_orgunit_matching_final_deduped <- HIBRID_DATIM4U_orgunit_matching_final %>% 
  dplyr::filter(!(uid=="IWzmn1hYm2x" & organisationunituid=="u62MrMCzkK3") & !(uid=="F8g80XIDovu" & organisationunituid=="WyxfuysFWIO") & !(uid=="vzgBy9nOXUl" & organisationunituid=="v3imbfT9QWm") & !(uid=="Ya5u3rc1TSC" & organisationunituid=="FauUrMhXWJF") & !(uid=="YyX07LUAQEp" & organisationunituid=="fcoIHbbhAqd") )





# second filtering of the datasets. This time filtering out `#N/A`. This is the second level


HIBRIDorgunitDataSubset_2 <-HIBRIDorgunitData %>% 
  dplyr::filter(Level=="5" & `DATIM.uid`=="#N/A" & !grepl("CSO|Parish|School|Division", name)) %>% 
  dplyr::select(Level,uid,name,`DATIM.uid`,District,Subcounty) %>% 
  tidyr::unite(Concatenate_1, name, District, Subcounty, sep=";", remove=FALSE) 

# second merging of datasets
HIBRID_DATIM4U_orgunit_merge2_1 <-merge(HIBRIDorgunitDataSubset_2, DATIM4UorgunitDataSubset, by.x="Concatenate_1", by.y="Concatenate_1", all.x = TRUE)

#length(which(!is.na(HIBRID_DATIM4U_orgunit_merge2_1$organisationunituid))) # find how many have matched

HIBRID_DATIM4U_orgunit_merge2_1_Subset <- HIBRID_DATIM4U_orgunit_merge2_1 %>% 
  dplyr::filter(is.na(level))

HIBRID_DATIM4U_orgunit_merge2_1_Subset <-HIBRID_DATIM4U_orgunit_merge2_1_Subset %>% 
  tidyr::unite(Concatenate_2, `name.x`, District, sep=";", remove=FALSE)

HIBRID_DATIM4U_orgunit_merge2_2 <-merge(HIBRID_DATIM4U_orgunit_merge2_1_Subset, DATIM4UorgunitDataSubset, by.x="Concatenate_2", by.y="Concatenate_2", all.x = TRUE)

HIBRID_DATIM4U_orgunit_merge2_2_Subset <- HIBRID_DATIM4U_orgunit_merge2_2 %>% 
  dplyr::filter(is.na(level.y))


# second export for manual matching
#install.packages("here")
write.csv(HIBRID_DATIM4U_orgunit_merge2_2_Subset, file=here::here("files","HIBRID_DATIM4U_orgunit_merge2_2_Subset_for_matching.csv"), row.names = FALSE)


# manual matching simplified, below
# example used is sites in kampala district that need matching

mapping_hibrid_to_datim4u_orgunits <- HIBRID_DATIM4U_orgunit_merge_2_Subset %>% 
  dplyr::select(name.x,uid,District,Subcounty)


joined <-mapping_hibrid_to_datim4u_orgunits %>% 
  fuzzyjoin::stringdist_left_join(DATIM4UorgunitDataSubset, 
                                  by=c(name.x="name"),
                                  distance_col="dist", method="jaccard", 
                                  max_dist=0.6, ignore_case = TRUE) %>% 
  dplyr::select(name.x,uid,District, Subcounty,level,organisationunituid,name,level5name,level6name,dist) 


joined_3<-joined %>% 
  dplyr::filter(grepl("Kampala District",District) & grepl("Central Division",Subcounty) &
                  grepl("Kampala District",level5name) & grepl("Kampala Central Division",level6name) ) %>% 
  subset(name.x %in% unlist(mapping_hibrid_to_datim4u_orgunits$name.x) ) %>% 
  dplyr::mutate(distancecalc=stringdist::stringdist(joined_3$name.x, joined_3$name))

# vector holding facilities in kampala district to be matched
kampala <- c("Aandb Shalom HC II","Central Medical And Dental Clinic HC IInr","Dental Studio Clinic HC II","Dr. Ahmed Med. And Dental HC IInr","Dr. Jb Ntege Sengendo HC IInr","Ear, Nose And Throat Centre HC II","Eye Care Centre HC IInr","Hossana Medical Centre HC II","Hoswe And Macgeorge HC IInr","International Medical Centre-Kpc,Watoto","J And H Medical Clinic HC II Nr","Joda Clinic And Nursing Home HC IInr","Kampala Poly Clinic HC IInr","Kololo Polyclinic And X-Ray Service HC IInr","Luwum St. Dental And Surgery HC IInr")


joined_3_alternate <- joined_3 %>% 
    dplyr::filter(distancecalc %in% c(15,12,3,6,13,16) & name.x %in% kampala ) # change values appropriately


# matching
Match_Idx <-stringdist::amatch(tolower(joined_3_alternate$name.x),
                               tolower(joined_3_alternate$name),
                               method = 'lcs', maxDist = Inf)

Matches <-data.frame(joined_3_alternate$name.x, joined_3_alternate$name[Match_Idx]) # matching happens here  

Matches <-Matches %>% 
  dplyr::mutate(Distance = stringdist::stringdist(Matches$joined_3_alternate.name.x, Matches$joined_3_alternate.name.Match_Idx., method = 'lcs'))  %>%
  dplyr::filter(Distance %in% c(17,13,6,15)) %>%  # change value appropriately
  unique() %>% 
  dplyr::filter(!grepl("Hossana Medical Centre HC II", joined_3_alternate.name.x)) %>%  # change appropriately
  tidyr::unite(Concatenate, "joined_3_alternate.name.x","joined_3_alternate.name.Match_Idx.", sep=";")

# merging two together
joined_3_alternate <-joined_3_alternate %>% 
  tidyr::unite(Concatenate, "name.x","name", remove=FALSE, sep=";") 

joined_3_alternate_x <- joined_3_alternate %>%
  merge(Matches, by.x="Concatenate", by.y ="Concatenate" )

# stubborn ones, failed to be captured by the above                          
joined_3_alternate_4 <- joined_3_alternate %>%
  dplyr::filter((grepl("J And H Medical Clinic HC II Nr", name.x) & grepl("J & H Medical Clinic Health Centre II", name)) | (grepl("Kampala Poly Clinic HC IInr", name.x) & grepl("Kampala Poly Clinic Health Centre II", name))) %>% 
  tidyr::unite(Concatenate, "name.x","name", remove=FALSE, sep=";") %>% 
  dplyr::mutate(Distance = stringdist::stringdist(name.x, name, method = 'lcs'))

# merge alternative together
joined_3_alternate_all <-data.table::rbindlist(list(joined_3_alternate_x,
                                                    joined_3_alternate_4), fill=TRUE)

# select columns
joined_3_alternate_all <- joined_3_alternate_all %>% 
  dplyr::select(uid,organisationunituid)


## orgunit matching, HIBRID vs DATIM, end
