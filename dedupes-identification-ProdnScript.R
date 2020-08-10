# read import ready file, that's in csv format
for_dedupe_identification <- read.csv(here::here("files","tests","01.DATIM4U.FY2020Q3.ImportReady.HTS_TST_facility2020-08-09_21:30_EAT.csv"),
                                      header=TRUE,
                                      sep=",",
                                      check.names = FALSE,
                                      stringsAsFactors=FALSE,
                                      quote="\"",
                                      row.names=NULL)

# check for pure duplicates.
library(dplyr)

pure_dedupes <-for_dedupe_identification %>% 
  dplyr::group_by(dataElement,period,orgUnit,categoryOptionCombo) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1)

# add group_counts in form of group_no

#length(rep(1:356, each=2)) # count of the repeatitions
#pure_dedupes$group_no <-rep(1:356, each=2) # in case there are dedupes identified, group them


# check for cross-walk duplicates.
crosswalk_dedupes <-for_dedupe_identification %>% 
  dplyr::group_by(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo) %>%  # group by the column that you want to check for duplicates in
  dplyr::filter(n()>1)


# export for inclusion of names and orgunit hierachy

#write.csv(pure_dedupes, file=here::here("files/aggregating/", "02_pure_dedupes_community_FY2020Q2.csv"), row.names = FALSE)
