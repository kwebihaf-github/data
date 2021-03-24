## datim validations, start ##

# installation of datimvalidation package
install.packages(c("devtools", "here"))
require(devtools)
install_github("jason-p-pickering/datim-validation", force=TRUE)

# loading of datimvalidation package ans secrets file
require(datimvalidation)
secrets=here::here("files","secrets.json")
loadSecrets(secrets)

# validations
# for AGYW_PREV indicator, replace the default IM of uid=`HllvX50cXC0` with another IM of uid=`Y14uAWeaYX8` for purposes to go through validations smoothly
# Then after running the validations,  replace uid=Y14uAWeaYX8 with uid=HllvX50cXC0
datafile<-here::here("files","02.DATIM4U.FY2020Q2.ImportReady.TX_COMBINED.csv")
d<-d2Parser(filename = datafile ,type = "csv")
#checkDataElementOrgunitValidity(data = d,datasets = c("qzVASYuaIey","jKdHXpBfWop","BPEyzcDb8fT","em1U5x9hhXh", "mbdbMiLZ4AA")) # for MER 2.4
checkDataElementOrgunitValidity(data = d,datasets = c("TBcmmtoaCBC","BPEyzcDb8fT","zL8TlPVzEBZ","em1U5x9hhXh", "qHyrHc4zwx4")) # for MER 2.5

#checkDataElementDisaggValidity(data=d,datasets = c("qzVASYuaIey","jKdHXpBfWop","BPEyzcDb8fT","em1U5x9hhXh", "mbdbMiLZ4AA")) # for MER 2.4
checkDataElementDisaggValidity(data=d,datasets = c("TBcmmtoaCBC","BPEyzcDb8fT","zL8TlPVzEBZ","em1U5x9hhXh", "qHyrHc4zwx4")) # for MER 2.5

checkValueTypeCompliance(d)
checkNegativeValues(d)
#FY2020Q2_TX_Combined_Facility_violations <-validateData(data = d, datasets = c("qzVASYuaIey","jKdHXpBfWop","BPEyzcDb8fT","em1U5x9hhXh","mbdbMiLZ4AA"),return_violations_only = TRUE) # for MER 2.4
FY2020Q2_TX_Combined_Facility_violations <-validateData(data = d, datasets = c("TBcmmtoaCBC","BPEyzcDb8fT","zL8TlPVzEBZ","em1U5x9hhXh", "qHyrHc4zwx4"),return_violations_only = TRUE) # for MER 2.5

# saving of violations int csv file
write.csv(FY2020Q2_HTS_TST_Facility_violations,file=here::here("files/violations","FY2020Q2_TX_Combined_Facility_violations.csv"), row.names = FALSE)

## datim validations, end ##
