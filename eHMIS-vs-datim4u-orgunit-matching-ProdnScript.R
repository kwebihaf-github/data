# Install Packages
.packages <- c("here", "dplyr", "data.table","tidyr", "fuzzyjoin","stringdist","rstudioapi")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Urls and their endpoints
DATIM4U.dest.url<-"https://ug.datim4u.org/"
DATIM4U.dest.username<-"FKwebiha_DATIM4U"  # change your username
DATIM4U.dest.password<-rstudioapi::askForPassword() # run only once

LOCAL.url<-"https://hmis.health.go.ug/"  # it's eHMIS
LOCAL.username<-"m&e.usaidsites"
LOCAL.password<-rstudioapi::askForPassword() # run only once

# read the source files

# get from eHMIS first
rm(tmp)
tmp <- tempfile()
httr::GET(paste0(LOCAL.url,"api/organisationUnits.json","?paging=false&fields=id,level,name,parent,path"),
          httr::authenticate(LOCAL.username,LOCAL.password, type = "basic"),
          httr::content_type("application/json"),
          httr::write_disk(tmp,overwrite=TRUE),
          httr::set_config(httr::config(ssl_verifypeer = 0L)),
          httr::progress("down")
)

ehmis_orgunits_json <-jsonlite::fromJSON(tmp)

ehmis_orgUnits_ids <-ehmis_orgunits_json$organisationUnits$id
ehmis_orgUnits_level <-ehmis_orgunits_json$organisationUnits$level
ehmis_orgUnits_name <-ehmis_orgunits_json$organisationUnits$name
ehmis_orgUnits_path <-ehmis_orgunits_json$organisationUnits$path

for_merging <- data.frame(id=ehmis_orgUnits_ids,name=ehmis_orgUnits_name,level=ehmis_orgUnits_level)

test <-tidyr::separate(as.data.frame(ehmis_orgUnits_path),ehmis_orgUnits_path,
                       into=c("Blank","Country","Region","District","Subcounty","ServiceOutlet"),
                       sep="/",
                       remove=TRUE
)
test <-cbind(test,ehmis_orgUnits_level) 

Countries <- na.omit(data.frame(Country=unique(test$Country)))
Regions <- na.omit(data.frame(Region=unique(test$Region)))
Districts <-na.omit(data.frame(District=unique(test$District)))
Subcounties <- na.omit(data.frame(Subcounty=unique(test$Subcounty)))
ServiceOutlets <- na.omit(data.frame(ServiceOutlet=unique(test$ServiceOutlet)))

# add name & level columns
Countries <-merge(Countries,for_merging,
                  by.x = "Country",
                  by.y = "id",
                  all.x = TRUE)

Regions<- merge(Regions,for_merging,
                by.x = "Region",
                by.y = "id",
                all.x = TRUE)

Districts <- merge(Districts,for_merging,
                   by.x = "District",
                   by.y = "id",
                   all.x = TRUE)

Subcounties <- merge(Subcounties,for_merging,
                     by.x = "Subcounty",
                     by.y = "id",
                     all.x = TRUE)

ServiceOutlets <- merge(ServiceOutlets,for_merging,
                        by.x = "ServiceOutlet",
                        by.y = "id",
                        all.x = TRUE)

# renaming of columns before merging
colnames(Countries)[colnames(Countries)=="name"] <- "Country_name"
colnames(Regions)[colnames(Regions)=="name"] <- "Region_name"
colnames(Districts)[colnames(Districts)=="name"] <- "District_name"
colnames(Subcounties)[colnames(Subcounties)=="name"] <- "Subcounty_name"
colnames(ServiceOutlets)[colnames(ServiceOutlets)=="name"] <- "ServiceOutlet_name"


# merging
test <- test %>% 
  merge(Countries[,c("Country","Country_name")],by.x = "Country",by.y = "Country",all.x = TRUE) %>% 
  merge(Regions[,c("Region","Region_name")],by.x = "Region",by.y = "Region",all.x = TRUE) %>% 
  merge(Districts[,c("District","District_name")],by.x = "District",by.y = "District",all.x = TRUE) %>% 
  merge(Subcounties[,c("Subcounty","Subcounty_name")],by.x = "Subcounty",by.y = "Subcounty",all.x = TRUE) %>% 
  merge(ServiceOutlets[,c("ServiceOutlet","ServiceOutlet_name")],by.x = "ServiceOutlet",by.y = "ServiceOutlet",all.x = TRUE)

#eHMISorgunitData
eHMISorgunitData <- test[,!colnames(test)=="Blank"] # assign test to a proper eHMIS orgunits name


# get from DATIM4U second

rm(tmp)
tmp <- tempfile()
httr::GET(paste0(DATIM4U.dest.url,"api/organisationUnits.json","?paging=false&fields=id,level,name,parent,path"),
          httr::authenticate(DATIM4U.dest.username,DATIM4U.dest.password, type = "basic"),
          httr::content_type("application/json"),
          httr::write_disk(tmp,overwrite=TRUE),
          httr::set_config(httr::config(ssl_verifypeer = 0L)),
          httr::progress("down")
)

datim4u_orgunits_json <-jsonlite::fromJSON(tmp)

datim4u_orgUnits_ids <-datim4u_orgunits_json$organisationUnits$id
datim4u_orgUnits_level <-datim4u_orgunits_json$organisationUnits$level
datim4u_orgUnits_name <-datim4u_orgunits_json$organisationUnits$name
datim4u_orgUnits_path <-datim4u_orgunits_json$organisationUnits$path

for_merging_datim4u <- data.frame(id=datim4u_orgUnits_ids,name=datim4u_orgUnits_name,level=datim4u_orgUnits_level)


test_datim4u <-tidyr::separate(as.data.frame(datim4u_orgUnits_path),datim4u_orgUnits_path,
                               into=c("Blank","Global","Continent","Country","Region","District","Subcounty","ServiceOutlet"),
                               sep="/",
                               remove=TRUE
)

test_datim4u <-cbind(test_datim4u,datim4u_orgUnits_level) # append() is for a list not for a data frame


Countries_datim4u <- na.omit(data.frame(Country=unique(test_datim4u$Country)))
Regions_datim4u <- na.omit(data.frame(Region=unique(test_datim4u$Region)))
Districts_datim4u <-na.omit(data.frame(District=unique(test_datim4u$District)))
Subcounties_datim4u <- na.omit(data.frame(Subcounty=unique(test_datim4u$Subcounty)))
ServiceOutlets_datim4u <- na.omit(data.frame(ServiceOutlet=unique(test_datim4u$ServiceOutlet)))


# add name & level columns
Countries_datim4u <-merge(Countries_datim4u,for_merging_datim4u,
                          by.x = "Country",
                          by.y = "id",
                          all.x = TRUE)

Regions_datim4u<- merge(Regions_datim4u,for_merging_datim4u,
                        by.x = "Region",
                        by.y = "id",
                        all.x = TRUE)

Districts_datim4u <- merge(Districts_datim4u,for_merging_datim4u,
                           by.x = "District",
                           by.y = "id",
                           all.x = TRUE)

Subcounties_datim4u <- merge(Subcounties_datim4u,for_merging_datim4u,
                             by.x = "Subcounty",
                             by.y = "id",
                             all.x = TRUE)

ServiceOutlets_datim4u <- merge(ServiceOutlets_datim4u,for_merging_datim4u,
                                by.x = "ServiceOutlet",
                                by.y = "id",
                                all.x = TRUE)

# renaming of columns before merging
colnames(Countries_datim4u)[colnames(Countries_datim4u)=="name"] <- "Country_name"
colnames(Regions_datim4u)[colnames(Regions_datim4u)=="name"] <- "Region_name"
colnames(Districts_datim4u)[colnames(Districts_datim4u)=="name"] <- "District_name"
colnames(Subcounties_datim4u)[colnames(Subcounties_datim4u)=="name"] <- "Subcounty_name"
colnames(ServiceOutlets_datim4u)[colnames(ServiceOutlets_datim4u)=="name"] <- "ServiceOutlet_name"

# merging
test_datim4u <- test_datim4u %>% 
  merge(Countries_datim4u[,c("Country","Country_name")],by.x = "Country",by.y = "Country",all.x = TRUE) %>% 
  merge(Regions_datim4u[,c("Region","Region_name")],by.x = "Region",by.y = "Region",all.x = TRUE) %>% 
  merge(Districts_datim4u[,c("District","District_name")],by.x = "District",by.y = "District",all.x = TRUE) %>% 
  merge(Subcounties_datim4u[,c("Subcounty","Subcounty_name")],by.x = "Subcounty",by.y = "Subcounty",all.x = TRUE) %>% 
  merge(ServiceOutlets_datim4u[,c("ServiceOutlet","ServiceOutlet_name")],by.x = "ServiceOutlet",by.y = "ServiceOutlet",all.x = TRUE)

#DATIM4UorgunitData
DATIM4UorgunitData <- test_datim4u[,!colnames(test_datim4u)=="Blank"] # assign test to a proper eHMIS orgunits name


# Working on both eHMISorgunitData & DATIM4UorgunitData

# filtering of the datasets. filtering out blanks mainly in DATIM.uid column. This is first level
eHMISorgunitDataSubset <-eHMISorgunitData %>% 
  dplyr::filter(ehmis_orgUnits_level=="5") %>% 
  dplyr::select(ServiceOutlet,ehmis_orgUnits_level,ServiceOutlet_name,District_name,Subcounty_name) %>% 
  tidyr::unite(Concatenate_1, ServiceOutlet_name, District_name, Subcounty_name, sep=";", remove=FALSE) %>% 
  tidyr::unite(Concatenate_2, ServiceOutlet_name, District_name, sep=";", remove=FALSE)

DATIM4UorgunitDataSubset <-DATIM4UorgunitData %>% 
  dplyr::filter(datim4u_orgUnits_level=="7") %>% 
  dplyr::select(ServiceOutlet,datim4u_orgUnits_level,ServiceOutlet_name,District_name,Subcounty_name) %>% 
  tidyr::unite(Concatenate_1, ServiceOutlet_name, District_name, Subcounty_name, sep=";", remove=FALSE) %>% 
  tidyr::unite(Concatenate_2, ServiceOutlet_name, District_name, sep=";", remove=FALSE)

# merging of datasets and matching them

# Matching 1
eHMIS_DATIM4U_orgunit_merge_1 <-merge(eHMISorgunitDataSubset,
                                      DATIM4UorgunitDataSubset, 
                                      by.x="Concatenate_1", 
                                      by.y="Concatenate_1", 
                                      all.x = TRUE)

eHMIS_DATIM4U_orgunit_matching_1  <- eHMIS_DATIM4U_orgunit_merge_1 %>% 
  dplyr::filter(!is.na(datim4u_orgUnits_level))  %>% # those that match. update it the hibrid_vs_datim4u_orgunit_matching script. making it standard.
  dplyr::select(eHMIS_orgunit_uid=ServiceOutlet.x,DATIM4U_orgunit_uid=ServiceOutlet.y) # renaming of columns takes place at the same time columns are selected.

# Unmatching 1
eHMIS_DATIM4U_orgunit_merge_1_Subset <- eHMIS_DATIM4U_orgunit_merge_1 %>% 
  dplyr::filter(is.na(datim4u_orgUnits_level)) # those that haven't matched


# Matching 2
eHMIS_DATIM4U_orgunit_merge_2 <-merge(eHMIS_DATIM4U_orgunit_merge_1_Subset, 
                                      DATIM4UorgunitDataSubset, 
                                      by.x="Concatenate_2.x",
                                      by.y="Concatenate_2", 
                                      all.x = TRUE) # Try to match with District;ServiceOutletname

eHMIS_DATIM4U_orgunit_matching_2  <- eHMIS_DATIM4U_orgunit_merge_2 %>% 
  dplyr::filter(!is.na(datim4u_orgUnits_level.y))  %>% # those that match
  dplyr::select(eHMIS_orgunit_uid=ServiceOutlet.x,DATIM4U_orgunit_uid=ServiceOutlet) # renaming of columns takes place at the same time columns are selected.


# Unmatching 2
eHMIS_DATIM4U_orgunit_merge_2_Subset <- eHMIS_DATIM4U_orgunit_merge_2 %>% 
  dplyr::filter(is.na(datim4u_orgUnits_level.y)) # those that haven't matched


# temporary merge the matched orgunits
eHMIS_DATIM4U_orgunit_matching_all_temp <-rbind(eHMIS_DATIM4U_orgunit_matching_1,eHMIS_DATIM4U_orgunit_matching_2)


## check for duplicates
eHMIS_DATIM4U_orgunit_matching_all_temp %>% 
  dplyr::group_by(eHMIS_orgunit_uid) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1)  # This means that one site for eHMIS having more than one UID in DATIM4U. This can be ignored

# remove the select duplicate rows i.e get the most correct
## for ehmis_vs_datim4u, just ignore the duplicates because they exist in the same District-> Subcounty hierarchy



#Those that haven't matched above in the all_temp file
eHMIS_DATIM4U_orgunit_merge_2_Subset_2 <- eHMISorgunitDataSubset [which(!(eHMIS_DATIM4U_orgunit_merge_1$ServiceOutlet.x %in% unique(eHMIS_DATIM4U_orgunit_matching_all_temp$eHMIS_orgunit_uid))),]

colnames(eHMIS_DATIM4U_orgunit_merge_2_Subset_2)[colnames(eHMIS_DATIM4U_orgunit_merge_2_Subset_2)=="ServiceOutlet_name"] <- "ServiceOutlet_name.ehmis" # add additional column
DATIM4UorgunitDataSubset$`ServiceOutlet_name.datim` <- DATIM4UorgunitDataSubset$ServiceOutlet_name # adding additional column for merging


#Near matching

# you first near match the two sides, then after you find out how close they are (using stringdist::stringdist())
joined <-eHMIS_DATIM4U_orgunit_merge_2_Subset_2[,c("ServiceOutlet","ServiceOutlet_name.ehmis","District_name")] %>% 
  fuzzyjoin::stringdist_left_join(DATIM4UorgunitDataSubset[,c("ServiceOutlet","ServiceOutlet_name.datim","District_name")], 
                                  by=c(ServiceOutlet_name.ehmis="ServiceOutlet_name.datim"),
                                  distance_col="dist", method="jaccard", 
                                  max_dist=0.4, ignore_case = TRUE)

joined$Distance <- stringdist::stringdist(joined$`ServiceOutlet_name.ehmis`, joined$`ServiceOutlet_name.datim`) # important part for mapping, getting the closeness between the two parts


# Distance between first words
joined$ServiceOutlet_name.ehmis_first_word <- stringi::stri_extract_first_words(joined$ServiceOutlet_name.ehmis)
joined$ServiceOutlet_name.datim_first_word <- stringi::stri_extract_first_words(joined$ServiceOutlet_name.datim)

joined$Distance_first_words <- stringdist::stringdist(joined$ServiceOutlet_name.ehmis_first_word, joined$ServiceOutlet_name.datim_first_word) # important part for mapping, getting the closeness between the two parts


# subsetting joined
joined_subset_1<-joined %>% 
  dplyr::filter(Distance_first_words ==0 & Distance==12 & as.character(District_name.x) == as.character(District_name.y) ) # where they belong to the same district

joined_subset_2<-joined %>% 
  dplyr::filter(Distance_first_words ==0 & Distance==11 & as.character(District_name.x) == as.character(District_name.y) ) # where they belong to the same district

joined_subset_3<-joined %>% 
  dplyr::filter(Distance_first_words ==0 & Distance==10 & as.character(District_name.x) == as.character(District_name.y) ) # where they belong to the same district

joined_subset_4<-joined %>% 
  dplyr::filter(Distance_first_words ==0 & Distance==9 & as.character(District_name.x) == as.character(District_name.y) ) # where they belong to the same district

joined_subset_5<-joined %>% 
  dplyr::filter(Distance_first_words ==0 & Distance==8 & as.character(District_name.x) == as.character(District_name.y) ) # where they belong to the same district

joined_subset_6<-joined %>% 
  dplyr::filter(Distance_first_words ==0 & Distance %in% c(7,6,5,4,3,2,1,0) & as.character(District_name.x) == as.character(District_name.y) ) # where they belong to the same district

# combine the joined subsets together
df_list <- mget(ls(pattern="joined_|subset_"))
joined_subsets_all_together <-data.table::rbindlist(df_list) # filling in missing columns

# check for duplicates in the subsets_all_together df
joined_subsets_all_together %>% 
  dplyr::group_by(ServiceOutlet.x) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1)

# for those matched for duplicates, check for exact match manually
duplicates_in_a_column <- joined_subsets_all_together %>% 
  dplyr::group_by(ServiceOutlet.x) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1) %>% 
  dplyr::arrange(ServiceOutlet.x) # sorts in ascending order by the defined column

duplicate_counts <- duplicates_in_a_column %>% 
  dplyr::count(ServiceOutlet.x, sort=TRUE)

# subset
joined_subsets_all_together_subset_1 <- joined_subsets_all_together[!(joined_subsets_all_together$ServiceOutlet.x %in% unique(duplicates_in_a_column$ServiceOutlet.x)),] # for those that aren't duplicates


all_together_subset_1_match_1  <- joined_subsets_all_together_subset_1 %>% 
  dplyr::mutate(Distance_soundex = stringdist::stringdist(ServiceOutlet_name.ehmis, ServiceOutlet_name.datim, method = 'soundex')) # using soundex


# matching takes place here 
eHMIS_DATIM4U_orgunit_matching_3_1 <- all_together_subset_1_match_1 %>% 
  dplyr::filter(Distance_soundex !=1) #where they sound the same



ServiceOutlet_name_datim_count_2 <-eHMIS_DATIM4U_orgunit_matching_3_1 %>% 
  dplyr::count(ServiceOutlet_name.datim, sort=TRUE,name="ServiceOutlet_name_datim_count") 



# continued to match using reducing balance strategy
eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.ehmis_all_word <- stringi::stri_extract_all_words(eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.ehmis) # can be used to extract the nth word.
eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.datim_all_word <- stringi::stri_extract_all_words(eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.datim) # can be used to extract the nth word.

eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.ehmis_second_word <- as.character (lapply(eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.ehmis_all_word,"[",2) )
eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.datim_second_word <- as.character (lapply(eHMIS_DATIM4U_orgunit_matching_3_1$ServiceOutlet_name.datim_all_word,"[",2) )

#
District_name_ehmis_count <- eHMIS_DATIM4U_orgunit_matching_3_1 %>% 
  dplyr::group_by(District_name.x,ServiceOutlet_name.datim) %>% 
  dplyr::summarise(District_name_ehmis_service_outlet_pair_count=n())

#
# further do something on exact matching on the above i.e  eHMIS_DATIM4U_orgunit_matching_3_1
# here remove pairs

remove_service_outlets <- data.frame(
  ServiceOutlet_name.ehmis=c("Wakiso EPI Centre HC III","Good Hope HC II","Gulu Military Hospital","Gogonyo Medical Centre","Buikwe Kalagala HC II","Kiyunga Bukakande HC II","Karuma Medical Centre Clinic","Kisozi Flep HC III","Gogonyo Medical Centre","Atiira Medical Centre","Rugaaga Medical Centre","Katooke Clinic Centre HC II","Kiruhura Medical Centre Clinic"),
  ServiceOutlet_name.datim=c("Wakiso Health Centre IV","Good Health Clinic","Gulu Independent Hospital","Gogonyo Health Centre III","Buikwe Health Centre III","Kiyunga Health Centre II","Karuma Health Centre II","Kisozi Health Centre II","Gogonyo Health Centre III","Atiira Health Centre III","Rugaaga Health Centre II","Katooke Health Centre III","Kiruhura Health Centre IV")
)


for_appending <- data.frame(
  ServiceOutlet_name.ehmis=c("Good Luck Medical Clinic HC II","M&G Medical Clinic HC II","Nsawo Rcc HC III","Bbira Ngo HC II","Divine Health Care HC II","Family Dental Sergery HC II","Family Ortho Clinic Entre II","Kagulu (Namutumba) HC II","Kihanda GOVT HC II","Pajule Medical Center","Zion Treatment Centre HC II","Sure Medi Care HC II","Mercy General Care HC II","Life Saving Medical Centre","Life Trust Clinic","Kiwaga HC II","Grace Dental Clinic"),
  ServiceOutlet_name.datim=c("Good Health medical clinic HCIII","M & S General Clinic HC II NR","Nsawo Health Centre III","Bbira Health Centre II","Divine Medical Centre HCII","Family health care HCII","Family Health Clinic HCII","Kagulu Health Centre II","Kihanda Health Centre II","Pajule Health Centre IV","Zion Medical centre HCII","Sure Life Medical Centre HC II","Mercy Medical Centre CLINIC","Life Care medical Centre HCIII","Life care clinic HCII","Kiwaga Clinic","Grace Medical Clinic HCII")
)


for_appending_2 <- data.frame(
  ServiceOutlet_name.ehmis=c("St. Augstas Clinic HC II","Rubanda District HQ HC II","Rubanda PHC HC III","Rubanda Kagunga HC II","Busunju Police HC II","Busunju HC III","Cure Medical Centre (Kasubi) HC II","Cure Medical Centre (Kanyanya) HC II","Dental Trendz HC II","Dental Prince HC II","Doctors' Clinic (Luzira) HC II","Doctors Diagnostic Clinic HC II","Grace Medical Clinic (Wabigalo) HC II","Grace Medical and Dental Clinic HC II","Grace Medical Clinic (Mbuya I) HC II","Kakumiro - Masaka HC II","Kakumiro Medical HC II","Kampala Family Clinic HC III","Kampala Dental Clinic HC II","Lions Medical Centre (Kabowa) HC II","Lions Medical Centre (Mutundwe) HC II","Sembabule Police HC II","Sembabule Kabaale HC II","Amudat Joint Clinic","Angelina Domiciliary Clinic HC II","Arua Main Prisons HC III","Bugiri Buwunga HC III","Bulamu Medical Centre HC II","Bundibugyo Police HC II","Bunyiiro HC II","Buremba Medical Centre Clinic","Busaana Prison Clinic","Busingye Medical Clinic","Busunga HC II","Care Plus Medical Clinic HC II","Chinese Clinic Centre HC II","Desire Medical Centre HC II","Doctor's Medical Clinic HC II","Doctors Medical Care Centre HC II","Family Care Hospital","Family Care Clinic (Massajja) HC II","Family Clinic (Kisenyi II) HC II","Family Clinic (Lungujja) HC II","Focus Medical Center HC II","Focus Medical Centre HC II","Global Medical Services HC II","His Grace II Clinic HC II","His Grace Clinic HC II","Isingiro Police HC II","Jordan Medical Clinic","Jubilee Dental Ltd HC II","Kaberamaido Hospital","Kabirizi Lower HC II","Kaliro Flep HC II","Kalungu HC III","Kampala Independent Hospital","Kibaale Police Clinic HC II","Kisakye Medical Clinic (Nansana) HC II","Life Care Medical Clinic (Kyebando) HC II","Life Care Medical Clinic (Luzira) HC II","Lira Municipal HC II","Lwebitakuli Ngo HC III","Makindye Barracks HC III","Malcom Health Care (Kyanja) HC II","Malcom Medical Centre (Kasubi) HC II","Malcom Medical Centre Kawaala HC II","Maria Healthcare Clinic HC II","Masindi Main Prison HC III","Mengo Dental Clinic HC II","Mirembe Medical Clinic (Kyengera) HC II","Mirembe Medical Clinic (Nansana) HC II","Mwesigwa Medical Clinic","Nabbingo Medical Centre","Nkokonjeru HC II","Ntoroko Medical Centre HC III","Nyaruhanga Ngo HC II","Nyarukiika Medical Centre Clinic","Old Kampala Hospital","Pagirinya Health Post HC II","Shalom Medical Clinic HC II","Sironko Police HC II","Soroti Main Prisons HC III","St. Jude Medical Centre (Kibuye I) HC II","St. Jude Medical Centre (Kabowa) HC II","Tawhid Medical Centre (Kampala) HC II","Tawhid Medical Centre (Rubaga) HC II","Tororo Main Prisons HC III","Tropical Dental Clinic HC II","UMC Victoria (Kamwokya) Hospital","UMC Victoria (Naguru) Hospital","Vision Medical Centre HC II"),
  ServiceOutlet_name.datim=c("St. Jude Clinic","Rubanda Prison HC II","Rubanda Prison HC II","Rubanda Prison HC II","Busunju Health Centre II","Busunju Health Centre II","Cure Medical Centre - Bwaise HCII","Cure Medical Centre - Bwaise HCII","Dental Studio Clinic","Dental Studio Clinic","Doctors Clinic HCII","Doctors Clinic HCII","Grace Medical Clinic HCII","Grace Medical Clinic HCII","Grace Medical Clinic HCII","Kakumiro Prison HC II","Kakumiro Prison HC II","Kampala emergency clinic HCII","Kampala emergency clinic HCII","Lions Medical Centre Health Centre II","Lions Medical Centre Health Centre II","Sembabule Prison HC II","Sembabule Prison HC II","Amudat HOSPITAL","Angelina Maternity Clinic HCII","Arua Women Prison HC II","Bugiri Prison HC II","Bulamu Medical Care Clinic HCII","Bundibugyo Hospital","Bunyiiro HC III","Buremba Health Centre III","Busaana Parents Clinic","Busingye Clinic","Busunga HC III","Care Plus Clinic HCII","Chinese Clinic- Health Centre II","Desire Medical Services HCII","Doctor's Clinic - Bwaise HC II","Doctors Med/C Health Centre II","Family Care Clinic Masulita","Family Care Clinic Masulita","Family Clinic Lusazze HCII","Family Clinic Lusazze HCII","Focus Medical Centre HC II","Focus Medical Centre HC II","Global Safe Medical Centre HCII","His Grace Clinic- Kisaasi","His Grace Clinic- Kisaasi","Isingiro Prison HC II","Jordan Medical Clinic HCII","Jubilee Dental Health Centre II","Kaberamaido Prisons Clinic","Kabirizi Health Centre II","Kaliro Prison HC II","Kalungu Prison Clinic","Kampala Hospital","Kibaale Prison HC II","Kisakye Medical Clinic kyaliwajjala HCII","Life Care Medical Clinic Kitintale HCII","Life Care Medical Clinic Kitintale HCII","Lira Women Prison HC II","Lwebitakuli Prison Clinic","Makindye Maternity Home","Malcom Health Care Kabowa HCII","Malcom Medical Centre Health Centre II","Malcom Medical Centre Health Centre II","Maria Health Care HCII","Masindi Women Prison HC II","Mengo Doctors Clinic","Mirembe Medical Clinic HCII","Mirembe Medical Clinic HCII","Mwesigwa Medical Clinic HCII","Nabbingo Health Centre II","Nkokonjeru HOSPITAL","Ntoroko Health Centre III","Nyaruhanga Health Centre II","Nyarukiika Health Centre II","Old Kampala Police Clinic","Pagirinya Health Centre III","Shalom doctors clinic HCII","Sironko Health Centre III","Soroti Women Prison HC II","St. Jude Medical Centre Mubaraka HCII","St. Jude Medical Centre Mubaraka HCII","Tawhid Medical Centre HCII","Tawhid Medical Centre HCII","Tororo Women Prison HC II","Tropical Spa Clinic","UMC Victoria Hospital","UMC Victoria Hospital","Vision Medical Clinic HCII")
)

remove_service_outlets <- rbind(remove_service_outlets,for_appending,for_appending_2)


#
remove_service_outlets_uids <- data.frame(
  ServiceOutlet.x=c("aW4S9Y3XhuV","S6BIC2MogkB","NtwxKup5q97"),
  ServiceOutlet.y=c("ajZpwIOKQdf","qyN7zGusGxS","EeaB5eJjEnr")
)

#
eHMIS_DATIM4U_orgunit_matching_3_1 <- eHMIS_DATIM4U_orgunit_matching_3_1 %>% 
  dplyr::left_join(ServiceOutlet_name_datim_count_2, by=c("ServiceOutlet_name.datim"="ServiceOutlet_name.datim")) %>% 
  merge(District_name_ehmis_count,by.x=c("District_name.x","ServiceOutlet_name.datim"), by.y=c("District_name.x","ServiceOutlet_name.datim"), all.x = TRUE) %>% 
  dplyr::anti_join(remove_service_outlets) %>% # removes pair wise
  dplyr::anti_join(remove_service_outlets_uids)



#
joined_subsets_all_together_subset_2 <- all_together_subset_1_match_1 %>% 
  dplyr::filter(Distance_soundex ==1) %>% #where they don't sound the same
  dplyr::filter(!grepl("Prison|Star|parents|Vincent|Police|Link", ServiceOutlet_name.datim)) %>%   #where matching are issues
  dplyr::filter(!grepl("Police", ServiceOutlet_name.ehmis))  %>%  #where matching are issues
  dplyr::filter(!grepl("Doctors", ServiceOutlet_name.datim)) %>% 
  dplyr::filter(!grepl("Prison|City", ServiceOutlet_name.ehmis)) %>% 
  dplyr::filter(!grepl("Day|Rural|J\ \\&S|Plus|New\ Ham", ServiceOutlet_name.datim)) #%>% 

ServiceOutlet_name_datim_count <-joined_subsets_all_together_subset_2 %>% 
  dplyr::count(ServiceOutlet_name.datim, sort=TRUE) %>% 
  dplyr::rename(ServiceOutlet_name_datim_count=n)

joined_subsets_all_together_subset_2$ServiceOutlet_name.ehmis_all_word <- stringi::stri_extract_all_words(joined_subsets_all_together_subset_2$ServiceOutlet_name.ehmis) # can be used to extract the nth word.
joined_subsets_all_together_subset_2$ServiceOutlet_name.datim_all_word <- stringi::stri_extract_all_words(joined_subsets_all_together_subset_2$ServiceOutlet_name.datim) # can be used to extract the nth word.

joined_subsets_all_together_subset_2$ServiceOutlet_name.ehmis_last_word <- stringi::stri_extract_last_words(joined_subsets_all_together_subset_2$ServiceOutlet_name.ehmis) 
joined_subsets_all_together_subset_2$ServiceOutlet_name.datim_last_word <- stringi::stri_extract_last_words(joined_subsets_all_together_subset_2$ServiceOutlet_name.datim)


joined_subsets_all_together_subset_2$ServiceOutlet_name.ehmis_second_word <- as.character (lapply(joined_subsets_all_together_subset_2$ServiceOutlet_name.ehmis_all_word,"[",2) )
joined_subsets_all_together_subset_2$ServiceOutlet_name.datim_second_word <- as.character (lapply(joined_subsets_all_together_subset_2$ServiceOutlet_name.datim_all_word,"[",2) )

# reducing balance takes place here
joined_subsets_all_together_subset_2 <- joined_subsets_all_together_subset_2 %>%
  dplyr::anti_join(remove_service_outlets) %>% # removes pair wise
  dplyr::mutate(Distance_second_word_soundex = stringdist::stringdist(ServiceOutlet_name.ehmis_second_word, ServiceOutlet_name.datim_second_word, method = 'soundex')) %>% 
  dplyr::filter( !(Distance_second_word_soundex ==1 & grepl("st", ServiceOutlet_name.datim, ignore.case = TRUE) & !grepl("The Orthodontist Ltd", ServiceOutlet_name.datim, ignore.case = FALSE))) %>% 
  dplyr::mutate(Distance_last_word_soundex = stringdist::stringdist(ServiceOutlet_name.ehmis_all_word, ServiceOutlet_name.datim_last_word, method = 'soundex')) %>% 
  dplyr::left_join(ServiceOutlet_name_datim_count, by=c("ServiceOutlet_name.datim"="ServiceOutlet_name.datim")) %>% 
  dplyr::filter( !(ServiceOutlet_name_datim_count %in% c(3)))


# The dfs (eHMIS_DATIM4U_orgunit_matching_all_temp,eHMIS_DATIM4U_orgunit_matching_3_1, eHMIS_DATIM4U_orgunit_matching_4_1) with two columns are the ones matched
# matches added
eHMIS_DATIM4U_orgunit_matching_3_1 <- eHMIS_DATIM4U_orgunit_matching_3_1 %>% 
  dplyr::select(eHMIS_orgunit_uid=ServiceOutlet.x,DATIM4U_orgunit_uid=ServiceOutlet.y)

eHMIS_DATIM4U_orgunit_matching_4_1 <- joined_subsets_all_together_subset_2 %>% 
  dplyr::select(eHMIS_orgunit_uid=ServiceOutlet.x,DATIM4U_orgunit_uid=ServiceOutlet.y)

# temporary merge the matched orgunits
eHMIS_DATIM4U_orgunit_matching_all_temp_2 <-rbind(eHMIS_DATIM4U_orgunit_matching_all_temp,
                                                  eHMIS_DATIM4U_orgunit_matching_3_1,
                                                  eHMIS_DATIM4U_orgunit_matching_4_1) %>% 
                                                  dplyr::distinct(.keep_all = TRUE)

# continue with the unmatched

joined_subsets_all_together_subset_3 <- joined_subsets_all_together[!(joined_subsets_all_together$ServiceOutlet.x %in% eHMIS_DATIM4U_orgunit_matching_all_temp_2$eHMIS_orgunit_uid),]


#
ServiceOutlet_name_datim_count_3 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::count(ServiceOutlet_name.datim, sort=TRUE,name="ServiceOutlet_name_datim_count") 


District_name_ehmis_count_2 <- joined_subsets_all_together_subset_3 %>% 
  dplyr::group_by(District_name.x,ServiceOutlet_name.datim) %>% 
  dplyr::summarise(District_name_ehmis_service_outlet_pair_count=n())

#
joined_subsets_all_together_subset_3 <- joined_subsets_all_together_subset_3 %>% 
  dplyr::left_join(ServiceOutlet_name_datim_count_3, by=c("ServiceOutlet_name.datim"="ServiceOutlet_name.datim")) %>% 
  merge(District_name_ehmis_count_2,by.x=c("District_name.x","ServiceOutlet_name.datim"), by.y=c("District_name.x","ServiceOutlet_name.datim"), all.x = TRUE) 

#removes
# to remove by reducing balance strategy

#
data_temp_remove_3 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==23 & !(District_name_ehmis_service_outlet_pair_count==23 & ServiceOutlet_name.datim=="St. Micheal Medical Centre HCII" & ServiceOutlet_name.ehmis=="St. Michael Medical Centre HC II")) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_4 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==3 & !(District_name_ehmis_service_outlet_pair_count==3 & ServiceOutlet_name.datim=="Lugazi Scoul HOSPITAL" & ServiceOutlet_name.ehmis=="Lugazi Scoul Hospital" |
                                                                       ServiceOutlet_name.datim=="Doctors Clinic HCII" & ServiceOutlet_name.ehmis=="Doctors' Clinic (Luzira) HC II" |
                                                                       ServiceOutlet_name.datim=="Global Safe Medical Centre HCII" & ServiceOutlet_name.ehmis=="Global Medicare Center HC II" |
                                                                       ServiceOutlet_name.datim=="Life Care Medical Clinic Kitintale HCII" & ServiceOutlet_name.ehmis=="Life Care Medical Clinic HC II" |
                                                                       ServiceOutlet_name.datim=="Sanyu Clinic HCII" & ServiceOutlet_name.ehmis=="Sanyu Clinic Ggaba HC II" |
                                                                       ServiceOutlet_name.datim=="Katooke Health Centre III" & ServiceOutlet_name.ehmis=="Katooke HC III" |
                                                                       ServiceOutlet_name.datim=="Masaka Prisons Clinic HC III" & ServiceOutlet_name.ehmis=="Masaka Prisons HC III" |
                                                                       ServiceOutlet_name.datim=="Mukono Eye Clinic" & ServiceOutlet_name.ehmis=="Mukono Eye Clinic HC II" |
                                                                       ServiceOutlet_name.datim=="Life Care medical Centre HCIII" & ServiceOutlet_name.ehmis=="Life Care Medical Centre HC III"
  )) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_5 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==5 & !(District_name_ehmis_service_outlet_pair_count==5 & 
                                                                       ServiceOutlet_name.datim=="Alpha Medical Clinic - Bbunga HCII" & ServiceOutlet_name.ehmis=="Alpha Medical Clinic - Bbunga HC II" |
                                                                       ServiceOutlet_name.datim=="Alpha Medical Clinic Ntinda HCII" & ServiceOutlet_name.ehmis=="Alpha Medical Clinic (Ntinda) HC II" |
                                                                       ServiceOutlet_name.datim=="St Vincent Health Centre HCII" & ServiceOutlet_name.ehmis=="St. Vincent Health Centre HC II" |
                                                                       ServiceOutlet_name.datim=="Mbale parents Clinic HC II" & ServiceOutlet_name.ehmis=="Mbale Parent's Clinic HC II" |
                                                                       ServiceOutlet_name.datim=="St. Andrew Domiciliary Clinic HCII" & ServiceOutlet_name.ehmis=="St. Andrew Domiciliary Clinic HC III" |
                                                                       ServiceOutlet_name.datim=="St. Francis Domicilliary clinic HCIII" & ServiceOutlet_name.ehmis=="St. Francis Domicilliary Clinic HC III" |
                                                                       ServiceOutlet_name.datim=="St. Jude domicilliary Clinic HCIII" & ServiceOutlet_name.ehmis=="St. Jude Domicilliary Clinic HC III" |
                                                                       ServiceOutlet_name.datim=="St. Mary's domicilliary clinic HCII" & ServiceOutlet_name.ehmis=="St. Mary's Domicilliary Clinic HC III"
  )) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)

#
data_temp_remove_6 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==9 & !(District_name_ehmis_service_outlet_pair_count==9 & 
                                                                       ServiceOutlet_name.datim=="St. Anne Dom Clinic HCIII" & ServiceOutlet_name.ehmis=="St. Anne Dom Clinic HC III" |
                                                                       ServiceOutlet_name.datim=="St. Calvin Clinic HCII" & ServiceOutlet_name.ehmis=="St. Calvin Clinic HC II" |
                                                                       ServiceOutlet_name.datim=="St. Clare Medical Centre HCII" & ServiceOutlet_name.ehmis=="St. Clare Medical Centre HC II" |
                                                                       ServiceOutlet_name.datim=="St. James Medical centre HCII" & ServiceOutlet_name.ehmis=="St. James Medical Centre (Wakiso) HC II"
  )) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_7 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==4 & !(District_name_ehmis_service_outlet_pair_count==4 & 
                                                                       ServiceOutlet_name.datim=="Hoima Prisons Clinic" & ServiceOutlet_name.ehmis=="Hoima Prisons HC II" |
                                                                       ServiceOutlet_name.datim=="St. Phillips Medical Clinic HCIII" & ServiceOutlet_name.ehmis=="St. Phillips Medical Clinic HC III"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_8 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==7 & !(District_name_ehmis_service_outlet_pair_count==7 & 
                                                                       ServiceOutlet_name.datim=="St. Louis Medical Centre" & ServiceOutlet_name.ehmis=="St. Louis Medical Centre HC II" |
                                                                       ServiceOutlet_name.datim=="St. Joseph Medical Centre HCIII" & ServiceOutlet_name.ehmis=="St. Joseph Medical Centre HC III"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_9 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==16 & !(District_name_ehmis_service_outlet_pair_count==16 & 
                                                                        ServiceOutlet_name.datim=="St. Jude Clinic" & ServiceOutlet_name.ehmis=="St. Jude Clinic HC II"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_10 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==13 & !(District_name_ehmis_service_outlet_pair_count==13 & 
                                                                        ServiceOutlet_name.datim=="St. Clare Medical Clinic HCIII" & ServiceOutlet_name.ehmis=="St. Clare Medical Clinic HC III" |
                                                                        ServiceOutlet_name.datim=="St Timothy Medical Centre HCII" & ServiceOutlet_name.ehmis=="St. Timothy Medical Clinic HC II"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
# all to be removed for this subset
data_temp_remove_11 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==22  
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_12 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==19 & !(District_name_ehmis_service_outlet_pair_count==19 & 
                                                                        ServiceOutlet_name.datim=="St Loius Medical Centre HCII" & ServiceOutlet_name.ehmis=="St. Louis Medical Centre HC II"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_13 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==11 & !(District_name_ehmis_service_outlet_pair_count==11 & 
                                                                        ServiceOutlet_name.datim=="St. Paul Medical Clinic HCII" & ServiceOutlet_name.ehmis=="St. Paul Medical Clinic HC II"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_14 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==6 & !(District_name_ehmis_service_outlet_pair_count==6 & 
                                                                       ServiceOutlet_name.datim=="Life Star Clinic HCII" & ServiceOutlet_name.ehmis=="Life Star Clinic HC II" |
                                                                       ServiceOutlet_name.datim=="New Five Star Clinic HCII" & ServiceOutlet_name.ehmis=="New Five Star Clinic HC II" |
                                                                       ServiceOutlet_name.datim=="St. Mark Medical Centre - Mulago HCII" & ServiceOutlet_name.ehmis=="St. Mark Medical Centre HC II" |
                                                                       ServiceOutlet_name.datim=="St. Marys medical services HCIII" & ServiceOutlet_name.ehmis=="St. Marys Medical Services HC III"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_15 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==8 & !(District_name_ehmis_service_outlet_pair_count==8 & 
                                                                       ServiceOutlet_name.datim=="St. Jude Medical Centre Mubaraka HCII" & ServiceOutlet_name.ehmis=="St. Jude Medical Centre Mubaraka HC II"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 

#
data_temp_remove_16 <-joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==8 & !(District_name_ehmis_service_outlet_pair_count==8 & 
                                                                       ServiceOutlet_name.datim=="St. Jude Medical Centre Mubaraka HCII" & ServiceOutlet_name.ehmis=="St. Jude Medical Centre Mubaraka HC II"
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim) 


## check for duplicates
data_temp_17 <- joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==2) %>% 
  dplyr::group_by(ServiceOutlet.x) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1)  # This means that one site for eHMIS having more than one UID in DATIM4U. This can be ignored

# merge, temporary
data_temp_17_temp_merge <- merge(data_temp_17, eHMISorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                 by.x="ServiceOutlet.x",
                                 by.y="ServiceOutlet",
                                 all.x = TRUE)

data_temp_17_temp_merge <- merge(data_temp_17_temp_merge, DATIM4UorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                 by.x="ServiceOutlet.y",
                                 by.y="ServiceOutlet",
                                 all.x = TRUE)

# soundex matching
data_temp_17_temp_merge  <- data_temp_17_temp_merge %>% 
  dplyr::mutate(Distance_Subcounties_soundex = stringdist::stringdist(Subcounty_name.x,Subcounty_name.y, method = 'soundex')) # using soundex


#
data_temp_remove_17_1 <-data_temp_17_temp_merge %>%
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==2 & Distance_Subcounties_soundex==0 & !(
    ServiceOutlet.y=="LwhQKD9V6Mb" & ServiceOutlet.x=="a6qaoH2Xzb1" & ServiceOutlet_name.datim=="Buyende Health Centre III" & ServiceOutlet_name.ehmis=="Buyende HC III" |
      ServiceOutlet.y=="Dvqcqo6WK9y" & ServiceOutlet.x=="ACckadqFYV7" & ServiceOutlet_name.datim=="Kamwenge Health Centre III" & ServiceOutlet_name.ehmis=="Kamwenge HC III" |
      ServiceOutlet.y=="yW4ZTg6cnZ0" & ServiceOutlet.x=="Ad2soe0ZZLP" & ServiceOutlet_name.datim=="Kamwenge Medical Centre HCII" & ServiceOutlet_name.ehmis=="Kamwenge Medical Centre HC II" |
      ServiceOutlet.y=="cS9eh05OU2U" & ServiceOutlet.x=="aFgrVHVDqoZ" & ServiceOutlet_name.datim=="Amuru Health Centre III" & ServiceOutlet_name.ehmis=="Amuru Lacor HC III" |
      ServiceOutlet.y=="AhXF0XiO1vK" & ServiceOutlet.x=="aqerr5gK2pr" & ServiceOutlet_name.datim=="Friends Med/C Health Centre II" & ServiceOutlet_name.ehmis=="Friends Medical Center HC II" |
      ServiceOutlet.y=="AqjmkI2QaRG" & ServiceOutlet.x=="aqerr5gK2pr" & ServiceOutlet_name.datim=="Friends Medical Centre Health Centre II" & ServiceOutlet_name.ehmis=="Friends Medical Center HC II" |
      ServiceOutlet.y=="Dcp9cCL054j" & ServiceOutlet.x=="B3XcYiNcqa7" & ServiceOutlet_name.datim=="Multicare Medical Centre HCII" & ServiceOutlet_name.ehmis=="Multicare Medical Centre HC II" |
      ServiceOutlet.y=="IOtLR19xEzF" & ServiceOutlet.x=="DbISdEaBLk6" & ServiceOutlet_name.datim=="New life medical centre HCII" & ServiceOutlet_name.ehmis=="New Life Medical Centre HC II" |
      ServiceOutlet.y=="aToEawLC4E5" & ServiceOutlet.x=="dfbQ83xJOeg" & ServiceOutlet_name.datim=="Care Medical Centre HCIII" & ServiceOutlet_name.ehmis=="Care Medical Centre (Wakiso) HC III" |
      ServiceOutlet.y=="tWane5JFRsS" & ServiceOutlet.x=="e1pByDp3Nxo" & ServiceOutlet_name.datim=="Nakasongola Prison HC III" & ServiceOutlet_name.ehmis=="Nakasongola Prisons HC III" |
      ServiceOutlet.y=="aBeMfKzBRRQ" & ServiceOutlet.x=="eAuFbAc1MUZ" & ServiceOutlet_name.datim=="Panyadoli Hill Health Centre II" & ServiceOutlet_name.ehmis=="Panyadoli Hill HC II" |
      ServiceOutlet.y=="vKTaUF0DEQO" & ServiceOutlet.x=="euEg1r9HcSg" & ServiceOutlet_name.datim=="Cure Medical Consults HCIII" & ServiceOutlet_name.ehmis=="Cure Medical Consults HC III" |
      ServiceOutlet.y=="y2ScQrJjct7" & ServiceOutlet.x=="fUPJVU6p7Ur" & ServiceOutlet_name.datim=="Mafuga Health Centre II" & ServiceOutlet_name.ehmis=="Mafuga HC II" |
      ServiceOutlet.y=="LieNk9dwlYl" & ServiceOutlet.x=="KGqGBYXgnr0" & ServiceOutlet_name.datim=="Kamwenge Medical Clinic HCII" & ServiceOutlet_name.ehmis=="Kamwenge Medical Clinic HC II" |
      ServiceOutlet.y=="AhXF0XiO1vK" & ServiceOutlet.x=="lwlvBRRVEVN" & ServiceOutlet_name.datim=="Friends Med/C Health Centre II" & ServiceOutlet_name.ehmis=="Friends Medical Centre HC II" |
      ServiceOutlet.y=="AqjmkI2QaRG" & ServiceOutlet.x=="lwlvBRRVEVN" & ServiceOutlet_name.datim=="Friends Medical Centre Health Centre II" & ServiceOutlet_name.ehmis=="Friends Medical Centre HC II" |
      ServiceOutlet.y=="tmCLcm6Wxj7" & ServiceOutlet.x=="MdmwmQbuLjA" & ServiceOutlet_name.datim=="Sheema Community HCIII" & ServiceOutlet_name.ehmis=="Sheema Community HC III" |
      ServiceOutlet.y=="eX1g5Un6fEt" & ServiceOutlet.x=="o7OluoErEud" & ServiceOutlet_name.datim=="Hoima Police Clinic HC II PHP" & ServiceOutlet_name.ehmis=="Hoima Police HC II" |
      ServiceOutlet.y=="pQi8qBlT57x" & ServiceOutlet.x=="OERp3gZVfSx" & ServiceOutlet_name.datim=="Cure Medical Centre HCIII" & ServiceOutlet_name.ehmis=="Cure Medical Centre HC III" |
      ServiceOutlet.y=="HYRKuLDyZe5" & ServiceOutlet.x=="Ol1Eb7jfNAG" & ServiceOutlet_name.datim=="Hope Children's Clinic HCII" & ServiceOutlet_name.ehmis=="Hope Children's Clinic HC II" |
      ServiceOutlet.y=="ofpBZVTpK1L" & ServiceOutlet.x=="oYg6OsZ6wxS" & ServiceOutlet_name.datim=="Kitebi Medical Centre HCII" & ServiceOutlet_name.ehmis=="Kitebi Medical Centre HC II" |
      ServiceOutlet.y=="hECo9L4HGf1" & ServiceOutlet.x=="sLOvwpCrdh2" & ServiceOutlet_name.datim=="Kitebi Health Centre III" & ServiceOutlet_name.ehmis=="Kitebi Health Centre HC III" |
      ServiceOutlet.y=="ks8sFPsqaOy" & ServiceOutlet.x=="TEmxmKVJJPa" & ServiceOutlet_name.datim=="Gasovu Health Centre III" & ServiceOutlet_name.ehmis=="Gasovu HC III" |
      ServiceOutlet.y=="Tj5pk05veg8" & ServiceOutlet.x=="uuLehTDwCMC" & ServiceOutlet_name.datim=="Liberty Medical Clinic HCII" & ServiceOutlet_name.ehmis=="Liberty Clinic HC II" |
      ServiceOutlet.y=="CdbowvC8qwJ" & ServiceOutlet.x=="xK1NgGOVu2D" & ServiceOutlet_name.datim=="Gasovu Health Centre II" & ServiceOutlet_name.ehmis=="Gasovu HC II" |
      ServiceOutlet.y=="sOQ3dUHIE7Y" & ServiceOutlet.x=="yAtQPOJepli" & ServiceOutlet_name.datim=="Care Medical Centre HCII" & ServiceOutlet_name.ehmis=="Care Medical Centre HC II" |
      ServiceOutlet.y=="acg9uyuC5ue" & ServiceOutlet.x=="YlgW3mVo9uu" & ServiceOutlet_name.datim=="Netcare medical centre HCII" & ServiceOutlet_name.ehmis=="Netcare medical centre HC II" 
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)

#
data_temp_remove_17_2 <- data_temp_17_temp_merge %>%
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==2 & Distance_Subcounties_soundex==1) %>% 
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)

## not duplicates
data_temp_17_not_duplicates <- joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==2) %>% 
  dplyr::group_by(ServiceOutlet.x) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()==1)  # This means that one site for eHMIS having more than one UID in DATIM4U. This can be ignored

# merge, temporary
data_temp_17_not_duplicates_temp_merge <- merge(data_temp_17_not_duplicates, eHMISorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                                by.x="ServiceOutlet.x",
                                                by.y="ServiceOutlet",
                                                all.x = TRUE)

data_temp_17_not_duplicates_temp_merge <- merge(data_temp_17_not_duplicates_temp_merge, DATIM4UorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                                by.x="ServiceOutlet.y",
                                                by.y="ServiceOutlet",
                                                all.x = TRUE)

# soundex matching
data_temp_17_not_duplicates_temp_merge  <- data_temp_17_not_duplicates_temp_merge %>% 
  dplyr::mutate(Distance_Subcounties_soundex = stringdist::stringdist(Subcounty_name.x,Subcounty_name.y, method = 'soundex')) # using soundex

#
data_temp_remove_not_duplicates_17_1 <-data_temp_17_not_duplicates_temp_merge %>%
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==2 & Distance_Subcounties_soundex==0 & !(
    ServiceOutlet.y=="DGEzUqtomCK" & ServiceOutlet.x=="Eio9d3BPJ2B" & ServiceOutlet_name.datim=="St. Luke Society Health Centre II" & ServiceOutlet_name.ehmis=="St. Luke Society HC II" |
      ServiceOutlet.y=="fHgs6cV2Q6H" & ServiceOutlet.x=="pATBoZZRddT" & ServiceOutlet_name.datim=="Kaabong HOSPITAL" & ServiceOutlet_name.ehmis=="Kaabong Hospital" |
      ServiceOutlet.y=="FOkGddZTFLx" & ServiceOutlet.x=="PPycqcd3beA" & ServiceOutlet_name.datim=="Kwagala Medical Clinic HCII" & ServiceOutlet_name.ehmis=="Kwagala Medical Clinic (Mende) HC II" |
      ServiceOutlet.y=="FTENiIutCsZ" & ServiceOutlet.x=="ttcMJ7FNnU0" & ServiceOutlet_name.datim=="St. Marys Medical Services CLINIC" & ServiceOutlet_name.ehmis=="St. Marys Medical Services Clinic" |
      ServiceOutlet.y=="I8Z4BLjT6ua" & ServiceOutlet.x=="fnl80NudU5x" & ServiceOutlet_name.datim=="Divine Medical Centre HCII" & ServiceOutlet_name.ehmis=="Divine Medical Centre HC II" |
      ServiceOutlet.y=="lnnMkUKP1aJ" & ServiceOutlet.x=="aLPsQWPEMFT" & ServiceOutlet_name.datim=="Kakumiro Health Centre IV" & ServiceOutlet_name.ehmis=="Kakumiro HC IV" |
      ServiceOutlet.y=="OVj3HouyGRs" & ServiceOutlet.x=="KM8jCDa1qLw" & ServiceOutlet_name.datim=="Lions Medical Centre Health Centre II" & ServiceOutlet_name.ehmis=="Lions Medical Centre (Kabowa) HC II" |
      ServiceOutlet.y=="OVj3HouyGRs" & ServiceOutlet.x=="R0o09ipn1rj" & ServiceOutlet_name.datim=="Lions Medical Centre Health Centre II" & ServiceOutlet_name.ehmis=="Lions Medical Centre (Mutundwe) HC II" |
      ServiceOutlet.y=="PqZJBY7X6Hn" & ServiceOutlet.x=="QHFF4O9ynsn" & ServiceOutlet_name.datim=="Nakulabye Medical Centre HCII" & ServiceOutlet_name.ehmis=="Nakulabye Medical Centre (Makindye) HC II" |
      ServiceOutlet.y=="ps5XFBH4bLK" & ServiceOutlet.x=="FcZTYW9JOGL" & ServiceOutlet_name.datim=="Stake Health Care HCII" & ServiceOutlet_name.ehmis=="Stake Health Care HC II" |
      ServiceOutlet.y=="pWc7SYEKBSR" & ServiceOutlet.x=="fyWg56mdyYT" & ServiceOutlet_name.datim=="UMC Victoria Hospital" & ServiceOutlet_name.ehmis=="UMC Victoria (Naguru) Hospital" |
      ServiceOutlet.y=="rKBEm0QbvGz" & ServiceOutlet.x=="tGo7YRaHpCN" & ServiceOutlet_name.datim=="Malcom Health Care Kabowa HCII" & ServiceOutlet_name.ehmis=="Malcom Health Care (Kabowa) HC II" |
      ServiceOutlet.y=="s4g0v36Gf1j" & ServiceOutlet.x=="Lw5bQ2U3VQQ" & ServiceOutlet_name.datim=="St. Micheal Medical Centre Health Centre II" & ServiceOutlet_name.ehmis=="St. Micheal Medical Centre (Nakawa) HC II" |
      ServiceOutlet.y=="Sbn9udpf7V9" & ServiceOutlet.x=="BiHrgxghFvx" & ServiceOutlet_name.datim=="Zion Medical centre HCII" & ServiceOutlet_name.ehmis=="Zion Medical Centre HC II" |
      ServiceOutlet.y=="TFBmXjxJOyE" & ServiceOutlet.x=="wtyMvPnfdEI" & ServiceOutlet_name.datim=="Kamwenge Prison Hc II" & ServiceOutlet_name.ehmis=="Kamwenge Prison Hc II" |
      ServiceOutlet.y=="uVHxrRzjtKJ" & ServiceOutlet.x=="sUuGsTZpCxT" & ServiceOutlet_name.datim=="Katooke Health Clinic" & ServiceOutlet_name.ehmis=="Katooke Health Clinic HC II" |
      ServiceOutlet.y=="xfZ6CkzKk67" & ServiceOutlet.x=="LulU6pCpYAA" & ServiceOutlet_name.datim=="Sanyu Clinic Health Centre II" & ServiceOutlet_name.ehmis=="Sanyu Clinic Rubaga HC II" 
  )
  ) %>% # these ones are to be removed or eliminated by reducing balance strategy
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)

#
data_temp_remove_not_duplicates_17_2 <- data_temp_17_not_duplicates_temp_merge %>%
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==2 & Distance_Subcounties_soundex==1) %>% 
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)

## check for duplicates
data_temp_18 <- joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==1) %>% 
  dplyr::group_by(ServiceOutlet.x) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1)  # This means that one site for eHMIS having more than one UID in DATIM4U. This can be ignored

# merge, temporary
data_temp_18_temp_merge <- merge(data_temp_18, eHMISorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                 by.x="ServiceOutlet.x",
                                 by.y="ServiceOutlet",
                                 all.x = TRUE)

data_temp_18_temp_merge <- merge(data_temp_18_temp_merge, DATIM4UorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                 by.x="ServiceOutlet.y",
                                 by.y="ServiceOutlet",
                                 all.x = TRUE)

# soundex matching
data_temp_18_temp_merge  <- data_temp_18_temp_merge %>% 
  dplyr::mutate(Distance_Subcounties_soundex = stringdist::stringdist(Subcounty_name.x,Subcounty_name.y, method = 'soundex')) # using soundex

# all words
data_temp_18_temp_merge$ServiceOutlet_name.ehmis_all_word <- stringi::stri_extract_all_words(data_temp_18_temp_merge$ServiceOutlet_name.ehmis) # can be used to extract the nth word.
data_temp_18_temp_merge$ServiceOutlet_name.datim_all_word <- stringi::stri_extract_all_words(data_temp_18_temp_merge$ServiceOutlet_name.datim) # can be used to extract the nth word.


# second word
data_temp_18_temp_merge$ServiceOutlet_name.ehmis_second_word <- as.character (lapply(data_temp_18_temp_merge$ServiceOutlet_name.ehmis_all_word,"[",2) )
data_temp_18_temp_merge$ServiceOutlet_name.datim_second_word <- as.character (lapply(data_temp_18_temp_merge$ServiceOutlet_name.datim_all_word,"[",2) )

data_temp_18_temp_merge  <- data_temp_18_temp_merge %>% 
  dplyr::mutate(Distance_Second_word_soundex = stringdist::stringdist(ServiceOutlet_name.datim_second_word,ServiceOutlet_name.ehmis_second_word, method = 'soundex')) # using soundex

# subsetting for removal
data_temp_remain_18_1_temp <- data_temp_18_temp_merge %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==1 & Distance_Subcounties_soundex==0 & Distance_Second_word_soundex==0) %>% 
  dplyr::select(ServiceOutlet.y,ServiceOutlet.x,ServiceOutlet_name.datim,ServiceOutlet_name.ehmis) 

data_temp_remain_18_1 <- data_temp_18_temp_merge %>% 
  dplyr::anti_join(data_temp_remain_18_1_temp)
#dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)

data_temp_remain_18_1 %>%
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==1 & Distance_Subcounties_soundex==0 & Distance_Second_word_soundex==1) %>% 
  dplyr::select(ServiceOutlet_name.ehmis_second_word) %>% 
  dplyr::count(ServiceOutlet_name.ehmis_second_word) %>%  # group by the column that you want to check for duplicates in
  dplyr::arrange(-n) # arranged in descending order


# joined_subsets_all_together_subset_3 %>% 
#   dplyr::count(District_name_ehmis_service_outlet_pair_count) %>% 
#   dplyr::arrange(-n) # arranged in descending order

#last word
data_temp_remain_18_1$ServiceOutlet_name.ehmis_last_word <- stringi::stri_extract_last_words(data_temp_remain_18_1$ServiceOutlet_name.ehmis) 
data_temp_remain_18_1$ServiceOutlet_name.datim_last_word <- stringi::stri_extract_last_words(data_temp_remain_18_1$ServiceOutlet_name.datim)

data_temp_remain_18_1  <- data_temp_remain_18_1 %>% 
  dplyr::mutate(Distance_Last_word_default = stringdist::stringdist(ServiceOutlet_name.datim_last_word,ServiceOutlet_name.ehmis_last_word)) # using default

# subsetting for removal
data_temp_remain_18_1_temp_1 <- data_temp_remain_18_1 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==1 & Distance_Subcounties_soundex==0 & Distance_Second_word_soundex==1 & Distance_Last_word_default==0) %>% 
  dplyr::select(ServiceOutlet.y,ServiceOutlet.x,ServiceOutlet_name.datim,ServiceOutlet_name.ehmis) 

#
data_temp_remain_18_1_temp_1 <-data_temp_remain_18_1_temp_1 %>%
  dplyr::filter(!(
    ServiceOutlet.y=="baynBDuUXUg" &ServiceOutlet.x=="wweoNKCmZIo" &ServiceOutlet_name.datim=="St. James- Kasubi Health Centre II" &ServiceOutlet_name.ehmis=="St. Agnes Kasubi Health Clinic HC II" |
      ServiceOutlet.y=="cZUdN87UOWm" &ServiceOutlet.x=="aQ7USWbUHsH" &ServiceOutlet_name.datim=="Kyenjojo Prison HC II" &ServiceOutlet_name.ehmis=="Kyenjojo Police HC II" |
      ServiceOutlet.y=="IxxR2M3VAXj" &ServiceOutlet.x=="tFR0RVesj3i" &ServiceOutlet_name.datim=="Mayanga Health Centre II" &ServiceOutlet_name.ehmis=="Mayanga Medical Centre HC II" |
      ServiceOutlet.y=="liJMJApQpu6" &ServiceOutlet.x=="wweoNKCmZIo" &ServiceOutlet_name.datim=="St. Jude- Kasubi Health Centre II" &ServiceOutlet_name.ehmis=="St. Agnes Kasubi Health Clinic HC II" |
      ServiceOutlet.y=="lvjYFnJSI7l" &ServiceOutlet.x=="wVUi8BXxKkY" &ServiceOutlet_name.datim=="Kitwe Prison HC II" &ServiceOutlet_name.ehmis=="Kitwe HC II" |
      ServiceOutlet.y=="Sg4tgQqRe0Z" &ServiceOutlet.x=="BE5LiYZjBvM" &ServiceOutlet_name.datim=="Kabasanda Prison HC II" &ServiceOutlet_name.ehmis=="Kabasanda HC II" |
      ServiceOutlet.y=="UYZqWZHYfQD" &ServiceOutlet.x=="SNsnShtQa5C" &ServiceOutlet_name.datim=="Mutufu Health Centre II" &ServiceOutlet_name.ehmis=="Mutufu Prison HC II" |
      ServiceOutlet.y=="wnEgLOscLcG" &ServiceOutlet.x=="wVUi8BXxKkY" &ServiceOutlet_name.datim=="Kitwe Prison HC II" &ServiceOutlet_name.ehmis=="Kitwe HC II" |
      ServiceOutlet.y=="xEKnxTyooJ0" &ServiceOutlet.x=="cLnJlZo5COI" &ServiceOutlet_name.datim=="Sentema Prison HC II" &ServiceOutlet_name.ehmis=="Sentema HC II" |
      ServiceOutlet.y=="ZccwtjwsW4q" &ServiceOutlet.x=="BE5LiYZjBvM" &ServiceOutlet_name.datim=="Kabasanda Prison HC II" &ServiceOutlet_name.ehmis=="Kabasanda HC II" 
  )
  )

# for removal
data_temp_remove_18_1 <- data_temp_remain_18_1 %>% 
  dplyr::anti_join(data_temp_remain_18_1_temp_1) %>% 
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)


## not duplicates
data_temp_18_not_duplicates <- joined_subsets_all_together_subset_3 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==1) %>% 
  dplyr::group_by(ServiceOutlet.x) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()==1)  # This means that one site for eHMIS having more than one UID in DATIM4U. This can be ignored

# merge, temporary
data_temp_18_not_duplicates_temp_merge <- merge(data_temp_18_not_duplicates, eHMISorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                                by.x="ServiceOutlet.x",
                                                by.y="ServiceOutlet",
                                                all.x = TRUE)

data_temp_18_not_duplicates_temp_merge <- merge(data_temp_18_not_duplicates_temp_merge, DATIM4UorgunitDataSubset[,c("ServiceOutlet","Subcounty_name")],
                                                by.x="ServiceOutlet.y",
                                                by.y="ServiceOutlet",
                                                all.x = TRUE)

# soundex matching
data_temp_18_not_duplicates_temp_merge  <- data_temp_18_not_duplicates_temp_merge %>% 
  dplyr::mutate(Distance_Subcounties_soundex = stringdist::stringdist(Subcounty_name.x,Subcounty_name.y, method = 'soundex')) # using soundex

# all words
data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.ehmis_all_word <- stringi::stri_extract_all_words(data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.ehmis) # can be used to extract the nth word.
data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.datim_all_word <- stringi::stri_extract_all_words(data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.datim) # can be used to extract the nth word.


# second word
data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.ehmis_second_word <- as.character (lapply(data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.ehmis_all_word,"[",2) )
data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.datim_second_word <- as.character (lapply(data_temp_18_not_duplicates_temp_merge$ServiceOutlet_name.datim_all_word,"[",2) )

data_temp_18_not_duplicates_temp_merge  <- data_temp_18_not_duplicates_temp_merge %>% 
  dplyr::mutate(Distance_Second_word_soundex = stringdist::stringdist(ServiceOutlet_name.datim_second_word,ServiceOutlet_name.ehmis_second_word, method = 'soundex')) # using soundex

# subsetting for removal
data_temp_remain_not_duplicates_18_1_temp <- data_temp_18_not_duplicates_temp_merge %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==1 & Distance_Subcounties_soundex==0 & Distance_Second_word_soundex==0) %>% 
  dplyr::select(ServiceOutlet.y,ServiceOutlet.x,ServiceOutlet_name.datim,ServiceOutlet_name.ehmis) 

data_temp_remain_not_duplicates_18_1 <- data_temp_18_not_duplicates_temp_merge %>% 
  dplyr::anti_join(data_temp_remain_not_duplicates_18_1_temp) # remaining using second word

#last word
data_temp_remain_not_duplicates_18_1$ServiceOutlet_name.ehmis_last_word <- stringi::stri_extract_last_words(data_temp_remain_not_duplicates_18_1$ServiceOutlet_name.ehmis) 
data_temp_remain_not_duplicates_18_1$ServiceOutlet_name.datim_last_word <- stringi::stri_extract_last_words(data_temp_remain_not_duplicates_18_1$ServiceOutlet_name.datim)

data_temp_remain_not_duplicates_18_1  <- data_temp_remain_not_duplicates_18_1 %>% 
  dplyr::mutate(Distance_Last_word_default = stringdist::stringdist(ServiceOutlet_name.datim_last_word,ServiceOutlet_name.ehmis_last_word)) # using default

# subsetting for removal
data_temp_remain_not_duplicates_18_1_temp_1 <- data_temp_remain_not_duplicates_18_1 %>% 
  dplyr::filter(District_name_ehmis_service_outlet_pair_count==1 & Distance_Subcounties_soundex==0 & Distance_Second_word_soundex==1 & Distance_Last_word_default==0) %>% 
  dplyr::select(ServiceOutlet.y,ServiceOutlet.x,ServiceOutlet_name.datim,ServiceOutlet_name.ehmis) 

#
data_temp_remain_not_duplicates_18_1_temp_1 <-data_temp_remain_not_duplicates_18_1_temp_1 %>%
  dplyr::filter(!(
    ServiceOutlet.y=="c46AYg2H9kr" &ServiceOutlet.x=="h7wpkDly8qZ" &ServiceOutlet_name.datim=="Oyam Women Prison HC II" &ServiceOutlet_name.ehmis=="Oyam Main Prisons HC II" |
      ServiceOutlet.y=="eKziS8rk9nI" &ServiceOutlet.x=="AULdz853paf" &ServiceOutlet_name.datim=="Isingiro Prison HC II" &ServiceOutlet_name.ehmis=="Isingiro Police HC II" |
      ServiceOutlet.y=="HuyfoZ8Ahjm" &ServiceOutlet.x=="Kq6r9wr36GH" &ServiceOutlet_name.datim=="Luzira Remand Prison HC III" &ServiceOutlet_name.ehmis=="Luzira Upper Prison HC III" |
      ServiceOutlet.y=="KvacVyOjIXE" &ServiceOutlet.x=="zcKswPk0OtD" &ServiceOutlet_name.datim=="Busaana Parents Clinic" &ServiceOutlet_name.ehmis=="Busaana Prison Clinic" |
      ServiceOutlet.y=="MVvRRAI32os" &ServiceOutlet.x=="xClaXB4x20Y" &ServiceOutlet_name.datim=="Sure Life Medical Centre HC II" &ServiceOutlet_name.ehmis=="Sure Medi Care HC II" |
      ServiceOutlet.y=="oDHiuX0kQzL" &ServiceOutlet.x=="idF5Y57Ula8" &ServiceOutlet_name.datim=="Kitala Health Centre II" &ServiceOutlet_name.ehmis=="Kitala Prison HC II" |
      ServiceOutlet.y=="rhFhtExhfvf" &ServiceOutlet.x=="PajDI41MhPJ" &ServiceOutlet_name.datim=="St. Bernard'S Health Centre II" &ServiceOutlet_name.ehmis=="St. Vincent Health Centre HC II" |
      ServiceOutlet.y=="TBjKfwdFkEb" &ServiceOutlet.x=="x1UaYpPwx6c" &ServiceOutlet_name.datim=="Gulu Independent Hospital" &ServiceOutlet_name.ehmis=="Gulu Military Hospital" |
      ServiceOutlet.y=="VvDYjfe8kt7" &ServiceOutlet.x=="Owvv3PbIrZ2" &ServiceOutlet_name.datim=="Amita Health Centre II" &ServiceOutlet_name.ehmis=="Amita Prison HC II" 
  )
  ) 

# for removal
data_temp_remove_18_2 <- data_temp_remain_not_duplicates_18_1 %>% 
  dplyr::anti_join(data_temp_remain_not_duplicates_18_1_temp_1) %>% 
  dplyr::select(ServiceOutlet_name.ehmis,ServiceOutlet_name.datim)

# combine the remove subsets together
df_list <- mget(ls(pattern="data_temp_remove_"))
remove_subsets_all_together_merged <-data.table::rbindlist(df_list) # filling in missing columns

# temporary merge the matched orgunits
eHMIS_DATIM4U_orgunit_matching_all_temp_3 <- joined_subsets_all_together_subset_3 %>% 
  dplyr::anti_join(remove_subsets_all_together_merged) %>% 
  dplyr::select(eHMIS_orgunit_uid=ServiceOutlet.x,DATIM4U_orgunit_uid=ServiceOutlet.y) # renaming of columns takes place at the same time columns are selected.

eHMIS_DATIM4U_orgunit_matching_all_temp_3 <-rbind(eHMIS_DATIM4U_orgunit_matching_all_temp_3,eHMIS_DATIM4U_orgunit_matching_all_temp_2) %>% 
  dplyr::distinct(.keep_all = TRUE)

# export so far matched to csv
data.table::fwrite(eHMIS_DATIM4U_orgunit_matching_all_temp_3,
                   file="~/Downloads/DATIM4U/DATIM/eHMIS_DATIM4U_orgunit_matching_all_temp_file-02.csv",
                   row.names = FALSE)

# to continue
library(dplyr)
#DATIM4UorgunitDataSubset

#eHMIS_DATIM4U_orgunit_matching_all_temp_3
#DATIM4UremainingToBeMatched

DATIM4UMatched <-as.data.frame(eHMIS_DATIM4U_orgunit_matching_all_temp_3$DATIM4U_orgunit_uid)
colnames(DATIM4UMatched) <-"ServiceOutlet"

DATIM4UremainingToBeMatched <- DATIM4UorgunitDataSubset %>% 
  dplyr::anti_join(DATIM4UMatched)

#  Near Matching for those ToBeMatched

#eHMISorgunitDataSubset

eHMISorgunitDataSubset$`ServiceOutlet_name.ehmis` <- eHMISorgunitDataSubset$ServiceOutlet_name # adding additional column for merging

# you first near match the two sides, then after you find out how close they are (using stringdist::stringdist())
joined_ToBeMatched <-eHMISorgunitDataSubset[,c("ServiceOutlet","ServiceOutlet_name.ehmis","District_name")] %>% 
  fuzzyjoin::stringdist_left_join(DATIM4UremainingToBeMatched[,c("ServiceOutlet","ServiceOutlet_name.datim","District_name")], 
                                  by=c(ServiceOutlet_name.ehmis="ServiceOutlet_name.datim"),
                                  distance_col="dist", method="jaccard", 
                                  max_dist=0.4, ignore_case = TRUE)

joined_ToBeMatched$Distance <- stringdist::stringdist(joined_ToBeMatched$`ServiceOutlet_name.ehmis`, joined_ToBeMatched$`ServiceOutlet_name.datim`) # important part for mapping, getting the closeness between the two parts

joined_ToBeMatched$Distance_districts <- stringdist::stringdist(joined_ToBeMatched$`District_name.x`, joined_ToBeMatched$`District_name.y`)

joined_ToBeMatched$Distance_serviceOutletNames_soundex  <- stringdist::stringdist(joined_ToBeMatched$`ServiceOutlet_name.ehmis`, joined_ToBeMatched$`ServiceOutlet_name.datim`, method = 'soundex') # using soundex
   


 joined_ToBeMatched %>% 
  dplyr::count(Distance, sort=TRUE) %>%  head()
 
 
# subsetting joined_ToBeMatched
 eHMIS_DATIM4U_orgunit_ToBeMatched_1 <- joined_ToBeMatched %>% 
   dplyr::filter(Distance_serviceOutletNames_soundex == 0 & Distance_districts==0) #where they sound the same and in the same district
 
 # matching takes place here 
 eHMIS_DATIM4U_orgunit_Matched_1 <- eHMIS_DATIM4U_orgunit_ToBeMatched_1 %>% 
   dplyr::filter(Distance <= 1)
 
 # subsetting eHMIS_DATIM4U_orgunit_Matched_1 
 eHMIS_DATIM4U_orgunit_ToBeMatched_2 <- eHMIS_DATIM4U_orgunit_ToBeMatched_1 %>% 
   dplyr::filter(Distance >=2 & Distance <=24)
 
 # matching takes place here 
 eHMIS_DATIM4U_orgunit_Matched_2 <- eHMIS_DATIM4U_orgunit_ToBeMatched_2 %>% 
   dplyr::filter(dist == 0)
 
 # subsetting eHMIS_DATIM4U_orgunit_ToBeMatched_2 
 eHMIS_DATIM4U_orgunit_ToBeMatched_3 <- eHMIS_DATIM4U_orgunit_ToBeMatched_2 %>% 
   dplyr::anti_join(eHMIS_DATIM4U_orgunit_Matched_2)
 
 # matching takes place here 
 eHMIS_DATIM4U_orgunit_Matched_3 <- eHMIS_DATIM4U_orgunit_ToBeMatched_3 %>% 
   dplyr::filter(Distance >=2 & Distance <=4) %>% 
   dplyr::filter(!(ServiceOutlet_name.ehmis=="Rapha Medical Centre" & ServiceOutlet_name.datim=="Ripon Medical Centre"))
 
 # subsetting eHMIS_DATIM4U_orgunit_ToBeMatched_3 
 eHMIS_DATIM4U_orgunit_ToBeMatched_4 <- eHMIS_DATIM4U_orgunit_ToBeMatched_3 %>% 
   dplyr::anti_join(eHMIS_DATIM4U_orgunit_Matched_3)
 
 # matching takes place here 
 eHMIS_DATIM4U_orgunit_Matched_4 <- eHMIS_DATIM4U_orgunit_ToBeMatched_4 %>% 
   dplyr::filter(Distance >=3 & Distance <=8) %>% 
   dplyr::filter((ServiceOutlet.x=="Age86DCA0c3" &ServiceOutlet_name.ehmis=="Doctor's Clinic" &ServiceOutlet.y=="VtyXhdYoidp" &ServiceOutlet_name.datim=="Doctor`s Clinic Serere" |
                     ServiceOutlet.x=="AKFDKJ5sKSW" &ServiceOutlet_name.ehmis=="Royal Health Care HC II" &ServiceOutlet.y=="e0pN9mh7DDa" &ServiceOutlet_name.datim=="Royal Health Care" |
                     ServiceOutlet.x=="AUEXQv8yyQ4" &ServiceOutlet_name.ehmis=="Hellens Clinic HC II" &ServiceOutlet.y=="YBJqOurebJh" &ServiceOutlet_name.datim=="Hellen's Clinic" |
                     ServiceOutlet.x=="aVoCPJIXuun" &ServiceOutlet_name.ehmis=="Bweyogerere Medical Centre HC II" &ServiceOutlet.y=="nxfaWduU4CW" &ServiceOutlet_name.datim=="Bweyogerere Medical Centre" |
                     ServiceOutlet.x=="Blo1aN62cyE" &ServiceOutlet_name.ehmis=="Pillars Medical Centre HC II" &ServiceOutlet.y=="SIwN1XBiQLa" &ServiceOutlet_name.datim=="Pillars Medical Centre" |
                     ServiceOutlet.x=="BMmZS6sQJyw" &ServiceOutlet_name.ehmis=="Kamwenge Medical Centre" &ServiceOutlet.y=="yW4ZTg6cnZ0" &ServiceOutlet_name.datim=="Kamwenge Medical Centre HCII" |
                     ServiceOutlet.x=="bqcYbI3x1sY" &ServiceOutlet_name.ehmis=="Global Medicare Center HC II" &ServiceOutlet.y=="yfIH4NN0fPV" &ServiceOutlet_name.datim=="Global Medicare Centre" |
                     ServiceOutlet.x=="BUKrloYEKHj" &ServiceOutlet_name.ehmis=="Kg Life Care Clinic" &ServiceOutlet.y=="Of49RveNbCJ" &ServiceOutlet_name.datim=="KG Lifecare Clinic HCII" |
                     ServiceOutlet.x=="d6hRGy9iLd8" &ServiceOutlet_name.ehmis=="Center Clinic HC II" &ServiceOutlet.y=="AssTniZ7VlC" &ServiceOutlet_name.datim=="Centre Clinic" |
                     ServiceOutlet.x=="dte1mdnBYoM" &ServiceOutlet_name.ehmis=="Anna Medical Centre HC II" &ServiceOutlet.y=="JkiVjsCoMXx" &ServiceOutlet_name.datim=="Anna Medical Centre" |
                     ServiceOutlet.x=="e7LJGg7TItR" &ServiceOutlet_name.ehmis=="Jinja Medcare Clinic" &ServiceOutlet.y=="t1pGKeN5aY7" &ServiceOutlet_name.datim=="Jinja Medcare" |
                     ServiceOutlet.x=="ExhseILVUNQ" &ServiceOutlet_name.ehmis=="Em's Health Clinic HC III" &ServiceOutlet.y=="EzUIKA1S1Th" &ServiceOutlet_name.datim=="EM`S Health Clinic III" |
                     ServiceOutlet.x=="EyZf2HkJvV3" &ServiceOutlet_name.ehmis=="Gv Medical Center HC II" &ServiceOutlet.y=="hAwsyVlxMHk" &ServiceOutlet_name.datim=="GV Medical Centre" |
                     ServiceOutlet.x=="f7n93to8Ktw" &ServiceOutlet_name.ehmis=="Mcneil Medical Centre" &ServiceOutlet.y=="Lcvje18Y4eq" &ServiceOutlet_name.datim=="McNeil Medical Centre HCII" |
                     ServiceOutlet.x=="FKuNaclJDeD" &ServiceOutlet_name.ehmis=="Anyiribu HC III" &ServiceOutlet.y=="eqP5AAA38f6" &ServiceOutlet_name.datim=="anyiribu" |
                     ServiceOutlet.x=="gozU6bjgH2h" &ServiceOutlet_name.ehmis=="St. Catherine Medical Center HC II" &ServiceOutlet.y=="ybEXuUN8tSg" &ServiceOutlet_name.datim=="St Catherine Medical Centre" |
                     ServiceOutlet.x=="KUFnXlL4bk3" &ServiceOutlet_name.ehmis=="Abii Clinic HC IV" &ServiceOutlet.y=="xWzvlXKunlb" &ServiceOutlet_name.datim=="Abi Clinic" |
                     ServiceOutlet.x=="LFhV54lMSwu" &ServiceOutlet_name.ehmis=="Globe Clinic HC II" &ServiceOutlet.y=="HSy9c8cF3m5" &ServiceOutlet_name.datim=="Globe Clinic" |
                     ServiceOutlet.x=="MYzCGYUmL4t" &ServiceOutlet_name.ehmis=="Liberty Medical Centre HC II" &ServiceOutlet.y=="TqB3RE84PNd" &ServiceOutlet_name.datim=="Liberty Medical Centre" |
                     ServiceOutlet.x=="nlCA2HNWuFV" &ServiceOutlet_name.ehmis=="Kalisizo Prison Clinic HC II" &ServiceOutlet.y=="qD1nq3r3aZ2" &ServiceOutlet_name.datim=="Kalisizo Prison Clinic" |
                     ServiceOutlet.x=="OGk0nfcBuM8" &ServiceOutlet_name.ehmis=="Your Clinic HC II" &ServiceOutlet.y=="gqGPtHRknOH" &ServiceOutlet_name.datim=="Your Clinic" |
                     ServiceOutlet.x=="Qvfoevzoi3N" &ServiceOutlet_name.ehmis=="Doctors Diagnostic Clinic HC II" &ServiceOutlet.y=="B9kVtthSL0v" &ServiceOutlet_name.datim=="Doctor's Diagnostic Clinic" |
                     ServiceOutlet.x=="qYZPZV35BRO" &ServiceOutlet_name.ehmis=="Lugazi Police HC II" &ServiceOutlet.y=="CcqYFAy9o0i" &ServiceOutlet_name.datim=="Lugazi Police" |
                     ServiceOutlet.x=="SDX1xOAUEKj" &ServiceOutlet_name.ehmis=="Life Link Medical Centre HC II" &ServiceOutlet.y=="BsfGx3McAKg" &ServiceOutlet_name.datim=="Life Link Medical Centre" |
                     ServiceOutlet.x=="uxOQ8aiwSFp" &ServiceOutlet_name.ehmis=="Marie Stopes Uganda Bweyogerere HC II" &ServiceOutlet.y=="VZZ4a8D0knB" &ServiceOutlet_name.datim=="Mariestopes Clinic Bweyogerere HCII" |
                     ServiceOutlet.x=="uZbjvt36X6V" &ServiceOutlet_name.ehmis=="Mb Medical Services HC II" &ServiceOutlet.y=="BzOlhq1O9HJ" &ServiceOutlet_name.datim=="MB Medical services" |
                     ServiceOutlet.x=="uzQnq2eOgnd" &ServiceOutlet_name.ehmis=="Jb Clinic HC II" &ServiceOutlet.y=="wZT2RzXLu8Z" &ServiceOutlet_name.datim=="JB Clinic" |
                     ServiceOutlet.x=="uZT6h3cOG5y" &ServiceOutlet_name.ehmis=="Family Care Medical Centre HC II" &ServiceOutlet.y=="U9a42lTYq9q" &ServiceOutlet_name.datim=="Family Care Medical Centre" |
                     ServiceOutlet.x=="yeDXYRbILNm" &ServiceOutlet_name.ehmis=="Diva Medical Centre" &ServiceOutlet.y=="hLuOP2MGTGv" &ServiceOutlet_name.datim=="Diva Medical Centre HCIII" |
                     ServiceOutlet.x=="a1DNpYv02JK" &ServiceOutlet_name.ehmis=="Pakwach Mission HC III" &ServiceOutlet.y=="Fxfs6HysZOX" &ServiceOutlet_name.datim=="Pakwach Mission Health Centre III" |
                     ServiceOutlet.x=="a3rqRkgOyEV" &ServiceOutlet_name.ehmis=="Mbale General Clinic Health II" &ServiceOutlet.y=="eAmdEazgSVK" &ServiceOutlet_name.datim=="Mbale General Clinic" |
                     ServiceOutlet.x=="a680eATOSPd" &ServiceOutlet_name.ehmis=="Buhara (Ngo) HC III" &ServiceOutlet.y=="h1kMB9WuThs" &ServiceOutlet_name.datim=="Buhara (Ngo) Health Centre III" |
                     ServiceOutlet.x=="a6b9OQh6Y8L" &ServiceOutlet_name.ehmis=="J.K Medi-Care Clinic HC II" &ServiceOutlet.y=="mFTm0ubva5H" &ServiceOutlet_name.datim=="JK Medi-Care Clinic Health Centre II" |
                     ServiceOutlet.x=="a6G1rz13Unf" &ServiceOutlet_name.ehmis=="Paragon Medical Clinic HC II" &ServiceOutlet.y=="QflGY07DrZz" &ServiceOutlet_name.datim=="Paragon Medical Clinic Health Centre II" |
                     ServiceOutlet.x=="a6h04I37hOu" &ServiceOutlet_name.ehmis=="Family Health Clinic (Kiboga) HC II" &ServiceOutlet.y=="a6rvjkpcXYD" &ServiceOutlet_name.datim=="Family Health Clinic Health Centre II" |
                     ServiceOutlet.x=="a8Qm23qwGwy" &ServiceOutlet_name.ehmis=="Namungalwe HC III" &ServiceOutlet.y=="KIdUUpSJe9y" &ServiceOutlet_name.datim=="Namungalwe Health Centre III" |
                     ServiceOutlet.x=="abMk2o2JMM7" &ServiceOutlet_name.ehmis=="Najanankumbi K Clinic HC II" &ServiceOutlet.y=="O1dQAaCh9eb" &ServiceOutlet_name.datim=="Najjanankumbi K Health Centre II" |
                     ServiceOutlet.x=="abvRm5Gf8UC" &ServiceOutlet_name.ehmis=="Sure Clinic HC II" &ServiceOutlet.y=="Qv7dSTNiyWD" &ServiceOutlet_name.datim=="Sure Clinic Health Centre II" |
                     ServiceOutlet.x=="ac7OhvMEosK" &ServiceOutlet_name.ehmis=="Nabbingo HC II" &ServiceOutlet.y=="zovGIP4zEBo" &ServiceOutlet_name.datim=="Nabbingo Health Centre II" |
                     ServiceOutlet.x=="AcJBataZEgg" &ServiceOutlet_name.ehmis=="Florence Medic Drug Supplies HC II" &ServiceOutlet.y=="QGcjc6muumo" &ServiceOutlet_name.datim=="Florence Medic Drug Supplies Health Centre II" |
                     ServiceOutlet.x=="aczRbyTZuz8" &ServiceOutlet_name.ehmis=="Patience Maternity Home HC II" &ServiceOutlet.y=="f4yy1Ouc1jl" &ServiceOutlet_name.datim=="Patience Maternity Home Health Centre II" |
                     ServiceOutlet.x=="aD1S6DBzHJV" &ServiceOutlet_name.ehmis=="Ongica HC III" &ServiceOutlet.y=="AmJKLdZPk8m" &ServiceOutlet_name.datim=="Ongica HCIII --Delete" |
                     ServiceOutlet.x=="adaEKqgk4SF" &ServiceOutlet_name.ehmis=="Q & J Clinic HC II" &ServiceOutlet.y=="FOvdFBrnxRP" &ServiceOutlet_name.datim=="Q & J Clinic Health Centre II" |
                     ServiceOutlet.x=="aDcEum2g6mY" &ServiceOutlet_name.ehmis=="Kanywambogo HC III" &ServiceOutlet.y=="lb7LohwkD5i" &ServiceOutlet_name.datim=="Kanywambogo Health Centre III" |
                     ServiceOutlet.x=="adncZQdcSzE" &ServiceOutlet_name.ehmis=="Nyakatookye HC II" &ServiceOutlet.y=="FXcDNBYnWBt" &ServiceOutlet_name.datim=="Nyakatokye Health Centre II" |
                     ServiceOutlet.x=="adW7QMFjyjd" &ServiceOutlet_name.ehmis=="Mpulira HC II - Ngo" &ServiceOutlet.y=="zBtqLUTWLt7" &ServiceOutlet_name.datim=="Mpulira Health Centre II - NGO" |
                     ServiceOutlet.x=="aE1Vh9TGsK2" &ServiceOutlet_name.ehmis=="St. Thereza Domiciliary HC II" &ServiceOutlet.y=="cMstGGQslFP" &ServiceOutlet_name.datim=="St. Thereza Domiciary Health Centre II" |
                     ServiceOutlet.x=="aFr56s147t5" &ServiceOutlet_name.ehmis=="Kyabigondo HC II" &ServiceOutlet.y=="Fn1FedwVKNj" &ServiceOutlet_name.datim=="Kyabigondo Health Centre II" |
                     ServiceOutlet.x=="aG2h9UI08nA" &ServiceOutlet_name.ehmis=="Bugangari HC IV" &ServiceOutlet.y=="IhkPOGxxs1R" &ServiceOutlet_name.datim=="Bugangari Health Centre IV" |
                     ServiceOutlet.x=="ajFglpQB0DX" &ServiceOutlet_name.ehmis=="Kanyanda HC II" &ServiceOutlet.y=="H0iMMoTGZpz" &ServiceOutlet_name.datim=="Kanyanda Health Centre II" |
                     ServiceOutlet.x=="aJW3cbEYSpE" &ServiceOutlet_name.ehmis=="Kisalizi HC II" &ServiceOutlet.y=="xMy4I78bh65" &ServiceOutlet_name.datim=="Kisalizi Health Centre II" |
                     ServiceOutlet.x=="akc4IXZ7N3m" &ServiceOutlet_name.ehmis=="Shanyonja HC II" &ServiceOutlet.y=="XbckXwNX9Gs" &ServiceOutlet_name.datim=="Syanyonja Health Centre II" |
                     ServiceOutlet.x=="aKP6B0Z5nbZ" &ServiceOutlet_name.ehmis=="Bardege HC III" &ServiceOutlet.y=="Gqb9MjiwxU5" &ServiceOutlet_name.datim=="Bar-dege Health Centre III" |
                     ServiceOutlet.x=="aLQa0i8ndtW" &ServiceOutlet_name.ehmis=="St. Steven Clinic HC II" &ServiceOutlet.y=="PyistmIxUhW" &ServiceOutlet_name.datim=="St. Steven Clinic Health Centre II" |
                     ServiceOutlet.x=="aMcxqYcYtot" &ServiceOutlet_name.ehmis=="Kampala Poly Clinic HC II" &ServiceOutlet.y=="T06OUQPdCWs" &ServiceOutlet_name.datim=="Kampala Poly Clinic Health Centre II" |
                     ServiceOutlet.x=="aNchHEjSnch" &ServiceOutlet_name.ehmis=="Bukedea Mission HC II" &ServiceOutlet.y=="cf6jIlGBkS9" &ServiceOutlet_name.datim=="Bukedea Mission Health Centre II" |
                     ServiceOutlet.x=="aND7XoaR2P9" &ServiceOutlet_name.ehmis=="Mushumba HC II" &ServiceOutlet.y=="WpQKE9UZLkH" &ServiceOutlet_name.datim=="Mushumba Health Centre II" |
                     ServiceOutlet.x=="aNW8E8PWwck" &ServiceOutlet_name.ehmis=="Palabek-Gem HC III" &ServiceOutlet.y=="BT1UERKCwd4" &ServiceOutlet_name.datim=="Palabek-Gem Health Centre III" |
                     ServiceOutlet.x=="aoKwhABrCpq" &ServiceOutlet_name.ehmis=="Kasheregyenyi HC II" &ServiceOutlet.y=="dfiQi9pYCCT" &ServiceOutlet_name.datim=="Kasheregenyi Health Centre II" |
                     ServiceOutlet.x=="aPw8BIgtIDF" &ServiceOutlet_name.ehmis=="Nsamu/Kyali HC III" &ServiceOutlet.y=="iTy2HiovQAQ" &ServiceOutlet_name.datim=="Nsamu/Kyali Health Centre III" |
                     ServiceOutlet.x=="aR0Ye42mrx8" &ServiceOutlet_name.ehmis=="Rwamwijuka HC II" &ServiceOutlet.y=="pldSeNhzIj5" &ServiceOutlet_name.datim=="Rwamwijuka Health Centre II" |
                     ServiceOutlet.x=="ar4RR2FBfKU" &ServiceOutlet_name.ehmis=="Ntungu HC II" &ServiceOutlet.y=="hFE8IvB8W7U" &ServiceOutlet_name.datim=="Ntungu Health Centre II" |
                     ServiceOutlet.x=="aRRsrxSesDy" &ServiceOutlet_name.ehmis=="Kewerimidde HC II" &ServiceOutlet.y=="s6HI2G13IrC" &ServiceOutlet_name.datim=="Kawerimidde Health Centre II" |
                     ServiceOutlet.x=="AtuVg4C11UO" &ServiceOutlet_name.ehmis=="Mbale Main Prisons HC III" &ServiceOutlet.y=="P8uWKDFdQYe" &ServiceOutlet_name.datim=="Mbale Main Prision's Health Centre III" |
                     ServiceOutlet.x=="atvpY5SsE88" &ServiceOutlet_name.ehmis=="Indilinga HC II" &ServiceOutlet.y=="HfQ1S203paW" &ServiceOutlet_name.datim=="Indiliga Health Centre II" |
                     ServiceOutlet.x=="aTvsnjjZTyN" &ServiceOutlet_name.ehmis=="Kyanamukaaka HC IV" &ServiceOutlet.y=="XwOyLbdtAx3" &ServiceOutlet_name.datim=="Kyannamukaaka Health Centre IV" |
                     ServiceOutlet.x=="aXGjPN0urzK" &ServiceOutlet_name.ehmis=="Koboko Mission HC III" &ServiceOutlet.y=="i9QAlHz8kSp" &ServiceOutlet_name.datim=="Koboko Mission Health Centre III" |
                     ServiceOutlet.x=="B3W5yafHqZE" &ServiceOutlet_name.ehmis=="Kigalama Govt HC II" &ServiceOutlet.y=="xII4xr8b0mR" &ServiceOutlet_name.datim=="Kigalama (Govt) Health Centre II" |
                     ServiceOutlet.x=="bAh0tns9mFX" &ServiceOutlet_name.ehmis=="Nakifuma HC III" &ServiceOutlet.y=="bBH0FEU8PDN" &ServiceOutlet_name.datim=="Nakifuma Health Centre III" ))
 
 # matching takes place here 
 eHMIS_DATIM4U_orgunit_Matched_5 <- eHMIS_DATIM4U_orgunit_ToBeMatched_4 %>% 
   dplyr::filter(Distance >=8 & Distance <=13) %>%
  dplyr::filter(grepl("HC", ServiceOutlet_name.ehmis) & grepl("Health Centre", ServiceOutlet_name.datim)) %>% 
   dplyr::filter(!(ServiceOutlet.x=="aB5vYQeUHae" &ServiceOutlet_name.ehmis=="Life Standard Medical Centre HC II" &ServiceOutlet.y=="bqzDOX6ecvQ" &ServiceOutlet_name.datim=="Life Star Health Centre II" |
                     ServiceOutlet.x=="AbQGl8CrhXH" &ServiceOutlet_name.ehmis=="Patongo Prison HC II" &ServiceOutlet.y=="yqMIrpBNXnq" &ServiceOutlet_name.datim=="Patongo Health Centre III" |
                     ServiceOutlet.x=="adwWHZXOk8h" &ServiceOutlet_name.ehmis=="Sena Medical Clinic HC II" &ServiceOutlet.y=="cYkOw5X1YFr" &ServiceOutlet_name.datim=="Shina Medical Clinic Health Centre II" |
                     ServiceOutlet.x=="aG3pNEZqZdA" &ServiceOutlet_name.ehmis=="St. Agnes Clinic (Nateete) HC II" &ServiceOutlet.y=="HsbHpz6aQ4b" &ServiceOutlet_name.datim=="St. James Clinic Health Centre II" |
                     ServiceOutlet.x=="AhLbPGoqf7F" &ServiceOutlet_name.ehmis=="Ongino General HC II" &ServiceOutlet.y=="hEESSI2gqlM" &ServiceOutlet_name.datim=="Ongino Health Centre III" |
                     ServiceOutlet.x=="am0vQYNJohR" &ServiceOutlet_name.ehmis=="Jengari HC II" &ServiceOutlet.y=="REdoZmqLWQK" &ServiceOutlet_name.datim=="Jeng-Gari Health Centre II" |
                     ServiceOutlet.x=="Ap53MmxflW6" &ServiceOutlet_name.ehmis=="Hope Clinic Lukuli HC III" &ServiceOutlet.y=="HLpu21TYlUx" &ServiceOutlet_name.datim=="Hope Clinic Health Centre II" |
                     ServiceOutlet.x=="aRE0Ejk5hdp" &ServiceOutlet_name.ehmis=="Nabiswera HC IV" &ServiceOutlet.y=="w5NMYjqncXB" &ServiceOutlet_name.datim=="Nabiswera Health Centre III" |
                     ServiceOutlet.x=="avKz7P8ctz4" &ServiceOutlet_name.ehmis=="Fellowship Medical Centre HC III" &ServiceOutlet.y=="QZHNhWKI7wW" &ServiceOutlet_name.datim=="Fellow Ship Health Centre II" |
                     ServiceOutlet.x=="aVoCPJIXuun" &ServiceOutlet_name.ehmis=="Bweyogerere Medical Centre HC II" &ServiceOutlet.y=="ErvdXsvaapg" &ServiceOutlet_name.datim=="Bweyogerere SDA Health Centre III" |
                     ServiceOutlet.x=="boTY4zS1Br8" &ServiceOutlet_name.ehmis=="Anajimu Medical Centre HC II" &ServiceOutlet.y=="gbQIYMM7CS6" &ServiceOutlet_name.datim=="Anajim Clinic Health Centre II" |
                     ServiceOutlet.x=="cwGt2cWV5TN" &ServiceOutlet_name.ehmis=="Nabiganda HC IV" &ServiceOutlet.y=="LpogeJinKOO" &ServiceOutlet_name.datim=="Nabiganda Health Centre III" |
                     ServiceOutlet.x=="dPHGsGPWCmY" &ServiceOutlet_name.ehmis=="Tororo Police HC III" &ServiceOutlet.y=="xTVYvXL6i2W" &ServiceOutlet_name.datim=="Tororo Police Health Centre II" |
                     ServiceOutlet.x=="DPRgzXMkh9U" &ServiceOutlet_name.ehmis=="Kajjansi HC IV" &ServiceOutlet.y=="xrilvxOCD7v" &ServiceOutlet_name.datim=="Kajjansi Health Centre III" |
                     ServiceOutlet.x=="dWiH5rblNL5" &ServiceOutlet_name.ehmis=="Banda Community Health Centre HC II" &ServiceOutlet.y=="vVAlBUnItV1" &ServiceOutlet_name.datim=="Banda Comm Dev Progm Health Centre II" |
                     ServiceOutlet.x=="E9FHg5HyYzC" &ServiceOutlet_name.ehmis=="St. Francis (Ocodri) HC III" &ServiceOutlet.y=="G5lgHlV4LMA" &ServiceOutlet_name.datim=="St. Francis Health Centre III" |
                     ServiceOutlet.x=="eAuFbAc1MUZ" &ServiceOutlet_name.ehmis=="Panyadoli Hill HC II" &ServiceOutlet.y=="EU3bRi7KNLO" &ServiceOutlet_name.datim=="Panyadoli Hill Health Centre II" |
                     ServiceOutlet.x=="EiF3piKONJO" &ServiceOutlet_name.ehmis=="St. Mary's Salalira HC III" &ServiceOutlet.y=="BHpTGcuHn7M" &ServiceOutlet_name.datim=="St. Mary`s Health Centre" |
                     ServiceOutlet.x=="EqoaWG6hZmv" &ServiceOutlet_name.ehmis=="St. Mark Medical Cetre HC II" &ServiceOutlet.y=="oQDZ5VDHu2c" &ServiceOutlet_name.datim=="St. Martin Health Centre II" |
                     ServiceOutlet.x=="EuOjc4l3RJ4" &ServiceOutlet_name.ehmis=="St. Bernard Medicare HC II" &ServiceOutlet.y=="rhFhtExhfvf" &ServiceOutlet_name.datim=="St. Bernard'S Health Centre II" |
                     ServiceOutlet.x=="ev1JnEmvQe0" &ServiceOutlet_name.ehmis=="Nyamiringa HC III" &ServiceOutlet.y=="tAbIeuiBKfR" &ServiceOutlet_name.datim=="Namiringa Health Centre II" |
                     ServiceOutlet.x=="ExhseILVUNQ" &ServiceOutlet_name.ehmis=="Em's Health Clinic HC III" &ServiceOutlet.y=="sRFn1OMMyxG" &ServiceOutlet_name.datim=="EMESCO Health Centre III" |
                     ServiceOutlet.x=="EyJbnAQLHR2" &ServiceOutlet_name.ehmis=="Shalom Medical Clinic HC II" &ServiceOutlet.y=="xoqDXnO9qrJ" &ServiceOutlet_name.datim=="Shiloh Medical Clinic Health Centre II" |
                     ServiceOutlet.x=="EZA6XG2Eqra" &ServiceOutlet_name.ehmis=="Moyo Mission HC IV" &ServiceOutlet.y=="lErB40IR6Ci" &ServiceOutlet_name.datim=="Moyo Mission Health Centre III" |
                     ServiceOutlet.x=="FgldlD8VLgC" &ServiceOutlet_name.ehmis=="Nakifuma Prison HC II" &ServiceOutlet.y=="bBH0FEU8PDN" &ServiceOutlet_name.datim=="Nakifuma Health Centre III" |
                     ServiceOutlet.x=="fsjtVOC0Bu3" &ServiceOutlet_name.ehmis=="St. Mark Medical Centre HC II" &ServiceOutlet.y=="oQDZ5VDHu2c" &ServiceOutlet_name.datim=="St. Martin Health Centre II" |
                     ServiceOutlet.x=="FVZifzVfB2S" &ServiceOutlet_name.ehmis=="Mukama Kyakuwa Clinic HC II" &ServiceOutlet.y=="iSAGnVcbGvH" &ServiceOutlet_name.datim=="Mukama Kyakuwa Health Centre II" |
                     ServiceOutlet.x=="FWFO4gWgFSk" &ServiceOutlet_name.ehmis=="Our Lady Nakasongola HC III" &ServiceOutlet.y=="X4h3Dh16MyG" &ServiceOutlet_name.datim=="Our Lady Health Centre III" |
                     ServiceOutlet.x=="fXKRSDL6Xri" &ServiceOutlet_name.ehmis=="Busingye Medical Centre HC II" &ServiceOutlet.y=="IqzrHrsCAKw" &ServiceOutlet_name.datim=="Busingye Clinic Health Centre II" |
                     ServiceOutlet.x=="fXKRSDL6Xri" &ServiceOutlet_name.ehmis=="Busingye Medical Centre HC II" &ServiceOutlet.y=="VS0arKDzut8" &ServiceOutlet_name.datim=="Busingyo Clinic Health Centre II" |
                     ServiceOutlet.x=="g4eDmlzUtWL" &ServiceOutlet_name.ehmis=="Kakoro SDA Dispensary HC II" &ServiceOutlet.y=="GtdLBoOgKrq" &ServiceOutlet_name.datim=="Kakoro SDA Health Centre II" |
                     ServiceOutlet.x=="GaSVQD2cOzL" &ServiceOutlet_name.ehmis=="Saidina Abubakar Nursing Home HC III" &ServiceOutlet.y=="ItASaZIi3nI" &ServiceOutlet_name.datim=="Saidina Abubaker Nursing Home Health Centre II" |
                     ServiceOutlet.x=="gozU6bjgH2h" &ServiceOutlet_name.ehmis=="St. Catherine Medical Center HC II" &ServiceOutlet.y=="pRBhndaEJb3" &ServiceOutlet_name.datim=="St. Catherine Clinic Health Centre II" |
                     ServiceOutlet.x=="h67U9AijmDA" &ServiceOutlet_name.ehmis=="Jinja All Saints Kagoma HC III" &ServiceOutlet.y=="Wrg7XOgMfLz" &ServiceOutlet_name.datim=="Jinja All Saints Health Centre II" |
                     ServiceOutlet.x=="HIpoOqCIQgV" &ServiceOutlet_name.ehmis=="Rwenshambya HC II" &ServiceOutlet.y=="vBVI9YyCbls" &ServiceOutlet_name.datim=="Rwenshamya Health Centre II" |
                     ServiceOutlet.x=="I6QbcVsphVT" &ServiceOutlet_name.ehmis=="AAR Clinic Acacia HC II" &ServiceOutlet.y=="CyK3vcYkFIj" &ServiceOutlet_name.datim=="AAR Clinic Health Centre II" |
                     ServiceOutlet.x=="idkNxrymxSW" &ServiceOutlet_name.ehmis=="Bugungu Yp Prison HC II" &ServiceOutlet.y=="EubaNORvPcm" &ServiceOutlet_name.datim=="Bugungu YO Prison Health Centre II" |
                     ServiceOutlet.x=="IqShqwgt8mf" &ServiceOutlet_name.ehmis=="San Medical Crntre HC II" &ServiceOutlet.y=="GAZHVPWS90G" &ServiceOutlet_name.datim=="Skan Medical Centre Health Centre II" |
                     ServiceOutlet.x=="J8lqAR46pse" &ServiceOutlet_name.ehmis=="Lugazi Muslim HC III" &ServiceOutlet.y=="GvfeyLkPxfz" &ServiceOutlet_name.datim=="Lugazi Muslim Health Centre II" |
                     ServiceOutlet.x=="Jb3ruV9NJ4K" &ServiceOutlet_name.ehmis=="Kisa Kyamukama Makindye HC II" &ServiceOutlet.y=="bMSirgitELq" &ServiceOutlet_name.datim=="Kisakyamukama Health Centre II" |
                     ServiceOutlet.x=="jhP6JvFuXoU" &ServiceOutlet_name.ehmis=="Mbuya Clinic HC II" &ServiceOutlet.y=="S635ZAsWMu5" &ServiceOutlet_name.datim=="Muva Clinic Health Centre II" |
                     ServiceOutlet.x=="JJZe7WMe7Z1" &ServiceOutlet_name.ehmis=="Sims Clinic HC II" &ServiceOutlet.y=="GcA5WUyIe8N" &ServiceOutlet_name.datim=="Sam Clinic Health Centre II" |
                     ServiceOutlet.x=="Jk4Ws9uWKqb" &ServiceOutlet_name.ehmis=="Bugungu Yo Prison HC II" &ServiceOutlet.y=="LURniXlGxuY" &ServiceOutlet_name.datim=="Bugungu YP Prison Health Centre II" |
                     ServiceOutlet.x=="jM0QE9Wtu6L" &ServiceOutlet_name.ehmis=="Kitalya Prisons HC II" &ServiceOutlet.y=="Gc32guG0oKj" &ServiceOutlet_name.datim=="Kitala Prisons Health CentreII" |
                     ServiceOutlet.x=="KETYpFB0Gpn" &ServiceOutlet_name.ehmis=="St. Mark Medical Center HC II" &ServiceOutlet.y=="oQDZ5VDHu2c" &ServiceOutlet_name.datim=="St. Martin Health Centre II" |
                     ServiceOutlet.x=="kggrthitjzr" &ServiceOutlet_name.ehmis=="Life Saver Medical Centre HC II" &ServiceOutlet.y=="Fl0oidCsEoE" &ServiceOutlet_name.datim=="Life Savers Clinic Health Centre II" |
                     ServiceOutlet.x=="Ku0I5IciWi3" &ServiceOutlet_name.ehmis=="Victory Medical Centre HC II" &ServiceOutlet.y=="W0oglafFPkt" &ServiceOutlet_name.datim=="Victory Health Health Centre II" |
                     ServiceOutlet.x=="l5C1tkKf11K" &ServiceOutlet_name.ehmis=="Ongutoi HC III" &ServiceOutlet.y=="ojS5MSKgxHs" &ServiceOutlet_name.datim=="Onguto Health Centre III" |
                     ServiceOutlet.x=="LBq7q7rovGE" &ServiceOutlet_name.ehmis=="Canaanite Health Center HC II" &ServiceOutlet.y=="QcTVbGO0jYf" &ServiceOutlet_name.datim=="Community Med. Health Centre II" |
                     ServiceOutlet.x=="Lgs45wMr4Ua" &ServiceOutlet_name.ehmis=="Bupadhengo Flep HC II" &ServiceOutlet.y=="O8Opve9M9Hg" &ServiceOutlet_name.datim=="Bupadhengo Health Centre III" |
                     ServiceOutlet.x=="lIIOj56WBOF" &ServiceOutlet_name.ehmis=="Butuntumula Prison HC II" &ServiceOutlet.y=="qwvr2aWTESO" &ServiceOutlet_name.datim=="Butuntumula Health Centre III" |
                     ServiceOutlet.x=="Lj5ibr8aice" &ServiceOutlet_name.ehmis=="Nakisunga Prison HC II" &ServiceOutlet.y=="Qxy0bfGfodI" &ServiceOutlet_name.datim=="Nakisunga Health Centre III" |
                     ServiceOutlet.x=="lwlvBRRVEVN" &ServiceOutlet_name.ehmis=="Friends Medical Centre HC II" &ServiceOutlet.y=="zQWKVCCkv8t" &ServiceOutlet_name.datim=="Friends Valley Health Centre II" |
                     ServiceOutlet.x=="m8iIXo1Saga" &ServiceOutlet_name.ehmis=="Rapha Medical Centre HC III" &ServiceOutlet.y=="Q23Z0j9A3Ss" &ServiceOutlet_name.datim=="Rapha Medical Centre- Health Centre II" |
                     ServiceOutlet.x=="mvtqd7EGRAM" &ServiceOutlet_name.ehmis=="Kisakye Health Center HC II" &ServiceOutlet.y=="QG4V1hCxTbT" &ServiceOutlet_name.datim=="Kisugu Health Centre III" |
                     ServiceOutlet.x=="mXVB3V7ft4h" &ServiceOutlet_name.ehmis=="Ayivu Health Centre HC III" &ServiceOutlet.y=="FYUwbTPE9nT" &ServiceOutlet_name.datim=="Apo Health Centre II" |
                     ServiceOutlet.x=="nP8o3jXx1WF" &ServiceOutlet_name.ehmis=="Platinum Medical Centre HC IV" &ServiceOutlet.y=="fsDEciGm1d3" &ServiceOutlet_name.datim=="Platinum Medical Centre Health Centre III" |
                     ServiceOutlet.x=="Ob2khLTIpAW" &ServiceOutlet_name.ehmis=="Kyanja Community Health Centre HC II" &ServiceOutlet.y=="PKLZNuPhGeA" &ServiceOutlet_name.datim=="Konge Clinic Health Centre II" |
                     ServiceOutlet.x=="okLSLD0KEwv" &ServiceOutlet_name.ehmis=="St. Joseph Medical Centre HC III" &ServiceOutlet.y=="Vh63cTB9Xc8" &ServiceOutlet_name.datim=="St. Kizito Health Centre HCIII" |
                     ServiceOutlet.x=="OwpzGOcbJxc" &ServiceOutlet_name.ehmis=="Mulunox Medical Centre HC II" &ServiceOutlet.y=="uwmONy1YyQW" &ServiceOutlet_name.datim=="Mulungi Medical Centre Health Centre II" |
                     ServiceOutlet.x=="p0fJEq6m1ew" &ServiceOutlet_name.ehmis=="TASO Soroti Special Clinic" &ServiceOutlet.y=="t3KC7IQ5qjS" &ServiceOutlet_name.datim=="TASO Soroti CLINIC" |
                     ServiceOutlet.x=="Pd6q0Mtx9Lu" &ServiceOutlet_name.ehmis=="Peak Medical Centre HC II" &ServiceOutlet.y=="VjzkUrIDCVn" &ServiceOutlet_name.datim=="Peace Medical Centre Health Centre II" |
                     ServiceOutlet.x=="PhsfViO0gu2" &ServiceOutlet_name.ehmis=="Life Care Medical Clinic (Kyebando) HC II" &ServiceOutlet.y=="cd5ZMZAQOiA" &ServiceOutlet_name.datim=="Life Care Medical Clinic Kitintale HCII" |
                     ServiceOutlet.x=="pMbWcRf3ZsC" &ServiceOutlet_name.ehmis=="Victoria Medical Centre (Masajja) HC III" &ServiceOutlet.y=="TMBOxn35U2Z" &ServiceOutlet_name.datim=="Victoria Medical Centre -Entebbe CLINIC" |
                     ServiceOutlet.x=="Ppyz6vwPpKi" &ServiceOutlet_name.ehmis=="AAR Clinic Bweyogerere HC II" &ServiceOutlet.y=="ggyTeqC74MQ" &ServiceOutlet_name.datim=="AAR clinic Freedom City HCII" |
                     ServiceOutlet.x=="ptxlYr62NSB" &ServiceOutlet_name.ehmis=="Labian Medical Centre HC II" &ServiceOutlet.y=="jTHypdiVZTX" &ServiceOutlet_name.datim=="Lubowa Medical Clinic HCII" |
                     ServiceOutlet.x=="pvMhc7H31I2" &ServiceOutlet_name.ehmis=="Tripple B Medical Clinic" &ServiceOutlet.y=="C9NxYKmDsgl" &ServiceOutlet_name.datim=="Travellers Clinic" |
                     ServiceOutlet.x=="qgcvP6v3ElR" &ServiceOutlet_name.ehmis=="Rahma Clinic HC II" &ServiceOutlet.y=="wZtMfgyzcwq" &ServiceOutlet_name.datim=="Rhema Clinic Health Centre II" |
                     ServiceOutlet.x=="R43DSu1tUIj" &ServiceOutlet_name.ehmis=="Katungu Mission HC III" &ServiceOutlet.y=="lrdrIuCB9xW" &ServiceOutlet_name.datim=="Katungu Health Centre II" |
                     ServiceOutlet.x=="rfk0AEPFYk5" &ServiceOutlet_name.ehmis=="Nyakadoti HC II" &ServiceOutlet.y=="rqy55F2sPNv" &ServiceOutlet_name.datim=="Nyakadot Health Centre III" |
                     ServiceOutlet.x=="Ru4lS70azm9" &ServiceOutlet_name.ehmis=="Vyne Medical Centre HC II" &ServiceOutlet.y=="k0Utclon6os" &ServiceOutlet_name.datim=="Vine Med Health Centre II" |
                     ServiceOutlet.x=="RXMQeaZcjjG" &ServiceOutlet_name.ehmis=="Nalinya Ndagire HC III" &ServiceOutlet.y=="HGkypbucq4X" &ServiceOutlet_name.datim=="Nalinnya Ndagire Health Centre II" |
                     ServiceOutlet.x=="sKqfvslNASd" &ServiceOutlet_name.ehmis=="Kaberamaido Police HC II" &ServiceOutlet.y=="Bkg0Vf75NSq" &ServiceOutlet_name.datim=="Kaberamaido Health Centre IV" |
                     ServiceOutlet.x=="sUuGsTZpCxT" &ServiceOutlet_name.ehmis=="Katooke Health Clinic HC II" &ServiceOutlet.y=="DfKqGD39T5k" &ServiceOutlet_name.datim=="Katooke Health Centre III" |
                     ServiceOutlet.x=="tqKvua1F3MU" &ServiceOutlet_name.ehmis=="Devine Clinic HC II" &ServiceOutlet.y=="LSgXrd2W0th" &ServiceOutlet_name.datim=="Divine Clinic Health Centre II" |
                     ServiceOutlet.x=="TXw7G3pIJE0" &ServiceOutlet_name.ehmis=="State House HC IV" &ServiceOutlet.y=="COdQ356DNat" &ServiceOutlet_name.datim=="State House Health Centre II" |
                     ServiceOutlet.x=="uiiH4IURTuT" &ServiceOutlet_name.ehmis=="Kabirizi Lower HC II" &ServiceOutlet.y=="hrtQZzyx0ij" &ServiceOutlet_name.datim=="Kabirizi 2 Health Centre II" |
                     ServiceOutlet.x=="uzQnq2eOgnd" &ServiceOutlet_name.ehmis=="Jb Clinic HC II" &ServiceOutlet.y=="Y2GNwAoXsqU" &ServiceOutlet_name.datim=="Jubi Clinic Health Centre II" |
                     ServiceOutlet.x=="ViVCNsxZYF4" &ServiceOutlet_name.ehmis=="Kabirizi Upper HC II" &ServiceOutlet.y=="hrtQZzyx0ij" &ServiceOutlet_name.datim=="Kabirizi 2 Health Centre II" |
                     ServiceOutlet.x=="W6xq6GhydXm" &ServiceOutlet_name.ehmis=="Kisakye Maternity Centre HC II" &ServiceOutlet.y=="YRhwzBVWyAP" &ServiceOutlet_name.datim=="Kisaasi Maternity Health Centre II" |
                     ServiceOutlet.x=="WByrVzVSS8z" &ServiceOutlet_name.ehmis=="Anyangatir HC III" &ServiceOutlet.y=="jD0UnpNdkAH" &ServiceOutlet_name.datim=="Anyangatir Health Centre II" |
                     ServiceOutlet.x=="wKczE4oVqOy" &ServiceOutlet_name.ehmis=="Kalungu HC III" &ServiceOutlet.y=="dg3wXIL7pH7" &ServiceOutlet_name.datim=="Kalungi Health Centre II" |
                     ServiceOutlet.x=="wqsIFyvLng4" &ServiceOutlet_name.ehmis=="Kitante Medical Centre HC IV" &ServiceOutlet.y=="mMvaxXKe5gK" &ServiceOutlet_name.datim=="Kitante Medical Center Health Centre II" |
                     ServiceOutlet.x=="Wwj8be6FRkT" &ServiceOutlet_name.ehmis=="Ttula Clinic HC II" &ServiceOutlet.y=="LMK4uv6RofO" &ServiceOutlet_name.datim=="Tuula Clinic Health Centre II" |
                     ServiceOutlet.x=="X3ZIaE3ccDb" &ServiceOutlet_name.ehmis=="Mushenene HC III" &ServiceOutlet.y=="NmuMElPcfET" &ServiceOutlet_name.datim=="Musyenene Health Centre III" |
                     ServiceOutlet.x=="X7iYabwpJfR" &ServiceOutlet_name.ehmis=="Mutukula HC III" &ServiceOutlet.y=="vEQv3z66rlu" &ServiceOutlet_name.datim=="Mitukula Health Centre III" |
                     ServiceOutlet.x=="YGb3TGRjmYq" &ServiceOutlet_name.ehmis=="Kd Clinic HC II" &ServiceOutlet.y=="GOdjriE2xUy" &ServiceOutlet_name.datim=="Ked Clinic Health Centre II" |
                     ServiceOutlet.x=="Z4327slMc14" &ServiceOutlet_name.ehmis=="Bubulo Walanga HC II" &ServiceOutlet.y=="YaXTD0ib6bG" &ServiceOutlet_name.datim=="Bubulo Health Centre IV")) 
 
 # matching takes place here 
 eHMIS_DATIM4U_orgunit_Matched_6 <- eHMIS_DATIM4U_orgunit_ToBeMatched_4 %>% 
   dplyr::filter(Distance >=14 & Distance <=19) %>% 
   dplyr::filter((ServiceOutlet.x=="B2H2ffGYIGG" &ServiceOutlet_name.ehmis=="Tusuubira Med. Cent HC II" &ServiceOutlet.y=="eIe54mnEtKu" &ServiceOutlet_name.datim=="Tusubira Medical Centre Health Centre II" |
                    ServiceOutlet.x=="k58XY2uVEjN" &ServiceOutlet_name.ehmis=="Bihanga UPDF Barracks HC II" &ServiceOutlet.y=="EN7d7UgJEuQ" &ServiceOutlet_name.datim=="Bihanga Updf Baracks Health Centre II" |
                    ServiceOutlet.x=="MxuyoEUEPpk" &ServiceOutlet_name.ehmis=="Living Proof Community HC III" &ServiceOutlet.y=="mUEGWXCQv84" &ServiceOutlet_name.datim=="Living Proof Community Health Centre HC III" |
                    ServiceOutlet.x=="s1VmGXPPQg5" &ServiceOutlet_name.ehmis=="Entebbe UVRI HC II" &ServiceOutlet.y=="vzizzQll5po" &ServiceOutlet_name.datim=="Entebbe Uvri Health Centre II" |
                    ServiceOutlet.x=="Y3hpndpv6cR" &ServiceOutlet_name.ehmis=="Muhooti Barraccks HC III" &ServiceOutlet.y=="UiGAy9v6yEq" &ServiceOutlet_name.datim=="Muhooti Baracks Health Centre II"))
 
 # temporary merge of matches gotten from eHMIS_DATIM4U_orgunit_ToBeMatched_4
 
 gotten_from_ToBeMatched_4_merged <- rbind(eHMIS_DATIM4U_orgunit_Matched_4,eHMIS_DATIM4U_orgunit_Matched_5,eHMIS_DATIM4U_orgunit_Matched_6)
 
 # subsetting eHMIS_DATIM4U_orgunit_ToBeMatched_4 
 eHMIS_DATIM4U_orgunit_ToBeMatched_5 <- eHMIS_DATIM4U_orgunit_ToBeMatched_4 %>% 
   dplyr::anti_join(gotten_from_ToBeMatched_4_merged)
 
 # matching takes place here
 eHMIS_DATIM4U_orgunit_Matched_7 <- eHMIS_DATIM4U_orgunit_ToBeMatched_5 %>%
   dplyr::filter((ServiceOutlet.x=="amOfrCsmDMr" &ServiceOutlet_name.ehmis=="Luwero Industries Ltd Clinic" &ServiceOutlet.y=="iS89iGcWCmp" &ServiceOutlet_name.datim=="Luwero Industries Ltd Clinic Health Centre II" |
                    ServiceOutlet.x=="Go1AzZKYcoS" &ServiceOutlet_name.ehmis=="Multicare Medical Centre HC II (Kawempe Division}" &ServiceOutlet.y=="vvCBoqUfq31" &ServiceOutlet_name.datim=="Multicare Medical Centre HCII" |
                    ServiceOutlet.x=="hLzOQxvRf4P" &ServiceOutlet_name.ehmis=="Rukungiri Blood Collection & Distribution Point" &ServiceOutlet.y=="QDpxXMEKtUn" &ServiceOutlet_name.datim=="Rukungiri Blood Collection Centre" |
                    ServiceOutlet.x=="nFMZAMrVcca" &ServiceOutlet_name.ehmis=="Asianut Medical Centre" &ServiceOutlet.y=="GBB4IP4O7Gx" &ServiceOutlet_name.datim=="Asianut Medical Centre HCII" |
                    ServiceOutlet.x=="sUPmMA66xoY" &ServiceOutlet_name.ehmis=="Kichompyo HC II" &ServiceOutlet.y=="MkjixHSlx7x" &ServiceOutlet_name.datim=="Kicompyo Health Center II" |
                    ServiceOutlet.x=="UNFFbN2I5sE" &ServiceOutlet_name.ehmis=="Lugusuulu HC II" &ServiceOutlet.y=="ux2zhBY1tls" &ServiceOutlet_name.datim=="Lugusulu Health Center II" |
                    ServiceOutlet.x=="XNtIAeZw6jh" &ServiceOutlet_name.ehmis=="National Leadership institute HC II" &ServiceOutlet.y=="VVQKPBMPxUi" &ServiceOutlet_name.datim=="National Leadership Training Instititute Health Centre II" |
                    ServiceOutlet.x=="XpJak3aZzRX" &ServiceOutlet_name.ehmis=="AIDS Information Centre (Arua) Special Clinic" &ServiceOutlet.y=="hbCXw0gsfja" &ServiceOutlet_name.datim=="AIDS Information Center Health Centre II" |
                    ServiceOutlet.x=="ZSC1XbtGdzq" &ServiceOutlet_name.ehmis=="St. Luke HC II" &ServiceOutlet.y=="Y4WeHKnkuiP" &ServiceOutlet_name.datim=="St. Luke Health Care Centre HCII"))
 
 # combine the match subsets together
 df_list <- mget(ls(pattern="eHMIS_DATIM4U_orgunit_Matched_"))
 match_subsets_all_together_merged <-data.table::rbindlist(df_list) # filling in missing columns
 
 # select columns
 eHMIS_DATIM4U_orgunit_matching_all_temp_4 <- match_subsets_all_together_merged %>% 
      dplyr::select(eHMIS_orgunit_uid=ServiceOutlet.x,DATIM4U_orgunit_uid=ServiceOutlet.y) # renaming of columns takes place at the same time columns are selected.
 
 # combine
 eHMIS_DATIM4U_orgunit_matching_all_temp_5 <- rbind(eHMIS_DATIM4U_orgunit_matching_all_temp_3, eHMIS_DATIM4U_orgunit_matching_all_temp_4)
 
 # export so far matched to csv
 data.table::fwrite(eHMIS_DATIM4U_orgunit_matching_all_temp_5,
                    file="~/Downloads/DATIM4U/DATIM/eHMIS_DATIM4U_orgunit_matching_all_temp_file-03.csv",
                    row.names = FALSE)
 
 # to continue with manual matching
