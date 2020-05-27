## orgunit matching, HIBRID vs DATIM, start

#  Install Packages

.packages <- c("here", "dplyr", "data.table")

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

# manual matching not yet finished



## orgunit matching, HIBRID vs DATIM, end