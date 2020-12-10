##Notes
# run metadata first, then data values, one at a time
# uncomment some parts when there are import conflicts


#  Install Packages
.packages <- c("here", "httr", "assertthat","jsonlite","tidyjson","rstudioapi")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Urls and their endpoints
MUWRP.dest.url<-"http://129.205.7.71:8080/"
MUWRP.dest.username<-"FKwebiha" # change username accordingly
MUWRP.dest.password<-rstudioapi::askForPassword() # run only once

LOCAL.url<-"http://localhost:12345/" # change to HIBRD Prodn base url accordingly
LOCAL.username<-"FKwebiha_HIBRID" # change username accordingly
LOCAL.password<-rstudioapi::askForPassword() # run only once

# functions used

# login check
#This will give us a cookie we can use for later use. 
loginDHIS2<-function(baseurl,username,password) {
  url<-paste0(baseurl,"api/me")
  r<-httr::GET(url,httr::authenticate(username,password))
  assertthat::assert_that(r$status_code == 200L) 
}

# dataset metedata existence check
postedMETADATA<-function(baseurl,datasetuid,username,password) {
  url<-paste0(baseurl,"api/dataSets/",datasetuid,"/metadata")
  r<-httr::GET(url,httr::authenticate(username,password))
  assertthat::assert_that(r$status_code == 200L) 
}


# export dataset metadata into MUWRP DEV
#G2lVPA37j3G, MER-2.3 annual hrh_staff
#rGH024QxEyi, MER-2.3 monthly facility-level dataset

dataset_uids <- c("G2lVPA37j3G","rGH024QxEyi","UawJH2h4wRl","XZwUx8XnYyd") # put the dataset uid here. one at a time.
dataset_names <- c("MER-2.3-annually_hrh_staff",
                   "MER-2.3-monthly-facility",
                   "MER-2.3-monthly-community",
                   "MER-2.3-monthly-facility-and-parish")
names(dataset_uids) <- dataset_names

for (i in 1:length(unname(dataset_uids))) {
#rm(tmp)
#paste0("tmp","_",dataset_uids[i]) <- tempfile()
httr::GET(paste0(LOCAL.url,"api/dataSets/",unname(dataset_uids)[i],"/metadata.json"),
          httr::authenticate(LOCAL.username,LOCAL.password, type = "basic"),
          httr::content_type("application/json"),
          httr::write_disk(paste0("tmp","_",unname(dataset_uids)[i]),overwrite=TRUE),
          httr::set_config(httr::config(ssl_verifypeer = 0L)),
          httr::progress("down")
)

}

# getting category options and categories from MUWRP DEV server
rm(tmp)
tmp <- tempfile()
httr::GET(paste0(MUWRP.dest.url,"api/metadata.json","?categoryOptions=true&categories=true&paging=false&fields=id,name"),
          httr::authenticate(MUWRP.dest.username,MUWRP.dest.password, type = "basic"),
          httr::content_type("application/json"),
          httr::write_disk(tmp,overwrite=TRUE),
          httr::set_config(httr::config(ssl_verifypeer = 0L)),
          httr::progress("down")
)

metadata_json_from_muwrp_dev_server <-jsonlite::fromJSON(tmp)

metadata_json <-jsonlite::fromJSON(paste0("tmp","_",unname(dataset_uids)[2])) # change from 1 upto ... length(unname(dataset_uids)).



# prune the dataset metadata json
  # remove parts that will cause import conflict errros
  # in categoryOptions
  # in dataSets
  
metadata_json$categoryOptions$userGroupAccesses <- NULL
metadata_json$categoryOptions$organisationUnits <- NULL
metadata_json$categoryOptions$userAccesses <- NULL
metadata_json$dataSets$organisationUnits <- NULL

# # test first
# which(metadata_json$categoryOptions$name == "default") # for testing purposes
# which(metadata_json$categories$name == "default") # for testing purposes
# which(metadata_json$categoryCombos$name == "default") # for testing purposes
# which(metadata_json$categoryOptionCombos$name == "default") # for testing purposes

if(length(which(metadata_json$categoryOptions$name == "default")) >= 1) {
  
  
  metadata_json$categoryOptions$name[which(metadata_json$categoryOptions$name == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
  metadata_json$categoryOptions$shortName[which(metadata_json$categoryOptions$shortName == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
  metadata_json$categoryOptions$code[which(metadata_json$categoryOptions$code == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing

} else if(length(which(metadata_json$categories$name == "default")) >= 1) {
  
  metadata_json$categories$name[which(metadata_json$categories$name == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
  metadata_json$categories$code[which(metadata_json$categories$code == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
  
} else if(length(which(metadata_json$categoryCombos$name == "default")) >= 1) {
  
  metadata_json$categoryCombos$name[which(metadata_json$categoryCombos$name == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
  metadata_json$categoryCombos$code[which(metadata_json$categoryCombos$code == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
} else if (length(which(metadata_json$categoryOptionCombos$name == "default"))) {
  
  metadata_json$categoryOptionCombos$name[which(metadata_json$categoryOptionCombos$name == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
  metadata_json$categoryOptionCombos$code[which(metadata_json$categoryOptionCombos$code == "default")]="default2" # replace if there is `default` value othwerwise an error (500) will occur while importing
} else {
  
  print ("No default in the dataset metadata")
}


# pick parts that mostly cause import conflicts i.e category options & categories

# category options
# name & shortName are required fields in category options, that's why they appear in import conflicts.
# using named vectors for this

# 
# #example
# LOCAL_server                                      MUWRP_dev_server
# (aKK1iTcbJmu, Surgical Technique)               (t4unTtlKuoR,Surgical Technique)
# (C9QK4nhXi5e,MSM who are not SW)               (C9QK4nhXi5e,MSM who are not SW)
# 

# pick ids and names from named objects metadata_json & metadata_json_from_muwrp_dev_server
uids <-metadata_json$categoryOptions$id
names(uids) <-metadata_json$categoryOptions$name

uids_from_muwrp_dev_server <- metadata_json_from_muwrp_dev_server$categoryOptions$id
names(uids_from_muwrp_dev_server) <- metadata_json_from_muwrp_dev_server$categoryOptions$name

for_fixing_names<- uids[which(!(uids %in% uids_from_muwrp_dev_server))]
names(uids)[which(names(uids) %in% names(for_fixing_names))] <-paste0(names(uids[which(!(uids %in% uids_from_muwrp_dev_server))]),2) # hits it right at the end or last which means fixes so as to avoid import conflicts

metadata_json$categoryOptions$id <- unname(uids)
metadata_json$categoryOptions$name <- names(uids)
metadata_json$categoryOptions$shortName <- names(uids) # assumes that name and shortName are the same in category options

# categories
uids_categories <-metadata_json$categories$id
names(uids_categories) <-metadata_json$categories$name

uids_categories_from_muwrp_dev_server <- metadata_json_from_muwrp_dev_server$categories$id
names(uids_categories_from_muwrp_dev_server) <- metadata_json_from_muwrp_dev_server$categories$name

for_fixing_names_categories<- uids_categories[which(!(uids_categories %in% uids_categories_from_muwrp_dev_server))]
names(uids_categories)[which(names(uids_categories) %in% names(for_fixing_names_categories))] <-paste0(names(uids_categories[which(!(uids_categories %in% uids_categories_from_muwrp_dev_server))]),2)

metadata_json$categories$id <- unname(uids_categories)
metadata_json$categories$name <- names(uids_categories)


#import_into_dev and  re-import_into_dev

# posting into MUWRP DEV server

# using httr
# first import (one at a time) the categoryOptions and categories, and fix (if any) , because most import conflicts come from them for MUWRP metadata (HIBRID) to DEV instance.
# then after import everything, i.e  use body=jsonlite::toJSON(metadata_json)
postDatasetMetadataToDev<-httr::POST(paste0(MUWRP.dest.url,"api/metadata?dryRun=false&preheatCache=false&atomicMode=NONE&async=false&strategy=CREATE_AND_UPDATE"),
                                     #body=httr::upload_file(here::here("files","test_categoryOptions.json")),
                                     #body=jsonlite::toJSON(list(categoryOptions=metadata_json$categoryOptions)), # importing individual components of the dataset metadata
                                     #body=jsonlite::toJSON(list(categories=metadata_json$categories)), # importing individual components of the dataset metadata
                                     #body=jsonlite::toJSON(list(categoryCombos=metadata_json$categoryCombos)), # importing individual components of the dataset metadata
                                     #body=jsonlite::toJSON(list(categoryOptionCombos=metadata_json$categoryOptionCombos)), # importing individual components of the dataset metadata
                                     #body=jsonlite::toJSON(list(dataElements=metadata_json$dataElements)), # importing individual components of the dataset metadata
                                     #body=jsonlite::toJSON(list(dataEntryForms=metadata_json$dataEntryForms)), # importing individual components of the dataset metadata
                                     #body=jsonlite::toJSON(list(dataSets=metadata_json$dataSets)), # importing individual components of the dataset metadata
                                     body=jsonlite::toJSON(metadata_json),
                                     httr::verbose(),
                                     httr::authenticate(MUWRP.dest.username,MUWRP.dest.password, type = "basic"),
                                     httr::content_type("application/json"),
                                     httr::progress("up"),
                                     httr::set_config(httr::config(ssl_verifypeer = 0L))
)


# get content response
import_conflict_response <-jsonlite::fromJSON(httr::content(postDatasetMetadataToDev, "text", encoding = "UTF-8"))

#
# # if there are import conflicts, do something
# 
# # Use for categoryOptions and categories only, accordingly
# if (import_conflict_response$stats$ignored > 0) {
# 
# 
# indices_that_need_fixing <-import_conflict_response[["typeReports"]][["objectReports"]][[1]][["index"]] # for metadata
# indices_that_need_fixing <- indices_that_need_fixing + 1 # increment the indexes by 1 to get the actual in the `metadata_json` list object
# 
# #metadata_json$categoryOptions$name[indices_that_need_fixing] # for testing purposes, category options, before and after
# 
# names_replace_categoryoptions_before <- c("Male","Other","Female","Unknown Age","MSM" ) # add more catyegoryOption items that aren't listed here
# names_replace_categoryoptions <-paste0(names_replace_categoryoptions_before,2)
# j <- which(names_replace_categoryoptions_before %in% metadata_json$categoryOptions$name[indices_that_need_fixing]) # for storing indices, for items that are used in replacing in `names_replace_categoryoptions` vector object
# 
# # change for categoryOptions and categories only, accordingly
# for(i in 1:length(indices_that_need_fixing)){
#   
#   metadata_json$categoryOptions$name[indices_that_need_fixing[i]] = names_replace_categoryoptions[j[i]] # for replacing names
#   metadata_json$categoryOptions$shortName[indices_that_need_fixing[i]]=names_replace_categoryoptions[j[i]] # for replacing shortnames
#   #metadata_json$categoryOptions$code[indices_that_need_fixing[i]]=codes_replace_categoryoptions[j[i]]  # for replacing codes
#   
#   
#   #metadata_json$categories$name[indices_that_need_fixing[i]]=names_replace_categories[j[i]]
#   #metadata_json$categories$code[indices_that_need_fixing[i]]=names_replace_categories[j[i]]
#   
#   #metadata_json$categoryCombos$name[indices_that_need_fixing[i]]=names_replace_categoryCombos[j[i]]
#   #metadata_json$categoryCombos$code[indices_that_need_fixing[i]]=names_replace_categoryCombos[j[i]]
#   
#   #metadata_json$categoryOptionCombos$name[indices_that_need_fixing[i]]=codes_replace_categoryoptioncombos[j[i]]
#   #metadata_json$categoryOptionCombos$code[indices_that_need_fixing[i]]=codes_replace_categoryoptioncombos[j[i]]
#   
# }
# 
# }
# 
# 
# if(length(import_conflict_response[["typeReports"]][["klass"]]) ==7) {
#   
#   print ("All the dataset metadata has gone in")
# } else{
#   
#   print ("Do the flow properly. i.e categoryOptions -> categories -> Everything (all the dataset metadata)")
#   
# }
#


# posting datavalues into MUWRP DEV server

# datavalues
#do at a time for every dataset
# first change the dataset uid and appropriate period below. im uid remains fixed, no need to change.
#sqlView used is of  uid=LH8rDoSQnAZ in HIBRID Prodn

#G2lVPA37j3G, MER-2.3 annual hrh_staff

#
im <- c("INcVj9FijNl") # IM uid for MUWRP obtained from categoryOptionCombos
datasetuid <- c("G2lVPA37j3G") # change here accordingly
startdate <- c("2018-10-01") # change here accordingly
enddate <- c("2019-09-30") # change here accordingly

#paste0(LOCAL.url,"api/sqlViews/","Izrx9CSlNER","/data.csv")

rm(tmp)
tmp <- tempfile()
httr::GET(paste0(LOCAL.url,"api/sqlViews/","LH8rDoSQnAZ","/data.csv","?var=im:",im,"&var=datasetuid:",datasetuid,"&var=startdate:",startdate,"&var=enddate:",enddate),
          httr::authenticate(LOCAL.username,LOCAL.password, type = "basic"),
          httr::content_type("application/csv"),
          httr::write_disk(tmp,overwrite=TRUE),
          httr::set_config(httr::config(ssl_verifypeer = 0L)),
          httr::progress("down")
)

datavalues_csv <-read.csv(tmp) # 


# if there are values posted by MUWRP into HIBRID for that dataset
# here there is import and re-import. Re-import when there are import conflicts.
if (nrow(datavalues_csv) > 0) { 
  
library(dplyr) # load the package that contains `%>%`
datavalues_csv$lastUpdated <- datavalues_csv$lastUpdated %>% 
                              stringr::str_replace_all(c(" "="T"))  #format date and time columns appropriately

datavalues_csv <-datavalues_csv %>% 
                  dplyr::filter(value!="")  # remove rows that have NAs

#post
postDatasetDatavaluesToDev<-httr::POST(paste0(MUWRP.dest.url,"api/dataValueSets?dryRun=false&preheatCache=false&importStrategy=CREATE_AND_UPDATE"),
                                       #body=httr::upload_file(here::here("files","FY2019-metadata-dataset-quarterly-facility_categoryOptions.json")), 
                                       body=jsonlite::toJSON(list(dataValues=datavalues_csv)),
                                       httr::verbose(),
                                       httr::authenticate(MUWRP.dest.username,MUWRP.dest.password, type = "basic"),
                                       httr::content_type("application/json"),
                                       httr::progress("up"),
                                       httr::set_config(httr::config(ssl_verifypeer = 0L))
)

# get content response
import_conflict_response <-jsonlite::fromJSON(httr::content(postDatasetDatavaluesToDev, "text", encoding = "UTF-8"))


}

# 
# # fix the import conflicts from data values
# 
# if(import_conflict_response$importCount$ignored > 0) {
# 
# 
#   # parts that need fixing are orgunits and period.
# 
#   # fixing for orgunits first
#   uids_that_need_fixing <-import_conflict_response[["conflicts"]][["object"]] # for datavalues
# 
#   # get orgunits from LOCAL server
#   rm(tmp)
#   tmp <- tempfile()
#   httr::GET(paste0(LOCAL.url,"api/organisationUnits.json?paging=false&fields=id,name,code,parent&filter=id:in:[",paste(uids_that_need_fixing, collapse=","),"]"),
#             httr::authenticate(LOCAL.username,LOCAL.password, type = "basic"),
#             httr::content_type("application/json"),
#             httr::write_disk(tmp,overwrite=TRUE),
#             httr::set_config(httr::config(ssl_verifypeer = 0L)),
#             httr::progress("down")
#   )
# }
# 
# orgunits_missing <-data.frame(jsonlite::fromJSON(tmp, flatten = TRUE))
# 
# # re-arranging columns
# orgunits_missing <-dplyr::select (orgunits_missing,
#                                   organisationUnits.name,
#                                   organisationUnits.id,
#                                   organisationUnits.code,
#                                   organisationUnits.parent.id)
# 
# 
# # save csv file for UI importing
# data.table::fwrite(orgunits_missing,
#                    file=here::here("files","orgunits-to-add_03.csv"),
#                    row.names = FALSE) # for UI importing
# 
# # fixing for periods second. this is custom i.e you just have to know that you have to fix it. know it from the checking the data values from the LOCAL server
# metadata_json$dataSets$dataInputPeriods<- NULL # remove the property name first, incase there is a period you want to add from the data values. in this case we are going to re-import the dataset metadata
# period_template <- '{"period":{"id":"%s"}}'
# id <- data.frame(id=c("2018AprilS2", "2019AprilS1", "2019Q2")) # normally happens for semi-annual indicators
# 
# dataInputPeriod_import_json<- paste0(apply(id, 1, function(y) {
#   sprintf(period_template,y["id"])
# }
# ))
# 
# metadata_json$dataSets$dataInputPeriods <-list(dataInputPeriod_import_json) # reconstruct the dataInputPeriod property in the metadata_json list object.
# 
# #
# writeLines(jsonlite::toJSON(list(dataSets=metadata_json$dataSets), pretty = TRUE, auto_unbox = FALSE),
#            useBytes=TRUE,con=here::here("files","period_dataset.json"))
# 
# # read json file into R
# find_and_replace_readLines <- readLines(here::here("files","period_dataset.json"), encoding="UTF-8")
# 
# # multi find and replace
# 
# # combined bit by bit together
# find_and_replace_readLines <- mgsub(c('[[]["][{]', '["][{]', '["]]', '[}]["]', '[\\]'), c("[{", "{", "]", "}", ""),find_and_replace_readLines)
# 
# # save object into file
# writeLines(find_and_replace_readLines,
#            con=here::here("files","period_dataset_for_import.json"))
# 
# # post period. in this case we use the file which was formatted easily.
# 
# postDatasetPeriodToDev<-httr::POST(paste0(MUWRP.dest.url,"api/metadata?dryRun=false&preheatCache=false&atomicMode=NONE&async=false&strategy=CREATE_AND_UPDATE"),
#                                    body=httr::upload_file(here::here("files","period_dataset_for_import.json")),
#                                    httr::verbose(),
#                                    httr::authenticate(MUWRP.dest.username,MUWRP.dest.password, type = "basic"),
#                                    httr::content_type("application/json"),
#                                    httr::progress("up"),
#                                    httr::set_config(httr::config(ssl_verifypeer = 0L))
# )
# 
# # get content response
# import_conflict_response <-jsonlite::fromJSON(httr::content(postDatasetPeriodToDev, "text", encoding = "UTF-8")) # when there are no import conflicts, re-import the whole data values.
#


# if all the datavalues have gone in
# check if the sums are equal i.e import dataframe object and export csv file from the DEV server

datasetuid <- c("G2lVPA37j3G") # change here accordingly. should be one only.

rm(tmp)
tmp <- tempfile()
httr::GET(paste0(MUWRP.dest.url,"api/dataValueSets.csv?","dataSet=",datasetuid,"&startDate=2018-10-01&endDate=2019-09-30&orgUnit=x81SIGvma6z&children=true&includeDeleted=true"), # change startDate and endDate accordingly
          httr::authenticate(MUWRP.dest.username,MUWRP.dest.password, type = "basic"),
          httr::content_type("application/csv"),
          httr::write_disk(tmp,overwrite=TRUE),
          httr::set_config(httr::config(ssl_verifypeer = 0L)),
          httr::progress("down")
)

sum_imported_csv <-read.csv(tmp) # read the csv response

# check is done here
#nrow(sum_imported_csv)
#sum(as.numeric(sum_imported_csv$value), na.rm = TRUE)
#sum(as.numeric(datavalues_csv$value), na.rm = TRUE)
#as.list(c(sum=sum(as.numeric(sum_imported_csv$value), na.rm = TRUE),count=nrow(sum_imported_csv)))
#as.list(c(sum=sum(as.numeric(datavalues_csv$value), na.rm = TRUE),count=nrow(datavalues_csv)))
identical(as.matrix(c(sum=sum(as.numeric(sum_imported_csv$value), na.rm = TRUE),count=nrow(sum_imported_csv))),
          as.matrix(c(sum=sum(as.numeric(datavalues_csv$value), na.rm = TRUE),count=nrow(datavalues_csv)))) # if response is TRUE, then all the datavalues have gone in, otherwise fix the imbalance (sum of datavalues and count of rows)


