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
datafile<-here::here("files","02.DATIM4U.FY2020Q2.ImportReady.TX_COMBINED.csv")
d<-d2Parser(filename = datafile ,type = "csv")
checkDataElementOrgunitValidity(data = d,datasets = c("qzVASYuaIey","jKdHXpBfWop","BPEyzcDb8fT","em1U5x9hhXh"))

checkDataElementDisaggValidity(data=d,datasets = c("qzVASYuaIey","jKdHXpBfWop","BPEyzcDb8fT","em1U5x9hhXh"))

checkValueTypeCompliance(d)
checkNegativeValues(d)
FY2020Q2_TX_Combined_Facility_violations <-validateData(data = d, datasets = c("qzVASYuaIey","jKdHXpBfWop","BPEyzcDb8fT","em1U5x9hhXh"),return_violations_only = TRUE)

# saving of violations int csv file
write.csv(FY2020Q2_HTS_TST_Facility_violations,file=here::here("files/violations","FY2020Q2_TX_Combined_Facility_violations.csv"), row.names = FALSE)

## datim validations, end ##