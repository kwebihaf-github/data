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