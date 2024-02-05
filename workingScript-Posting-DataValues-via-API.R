postDatavalues<-httr::POST("https://dev-de.datim.org/api/dataValueSets?dryRun=false&preheatCache=false&importStrategy=CREATE_AND_UPDATE",
                                       body=httr::upload_file("/home/fred/Downloads/DATIM4U/FY2024/Q1/Imports/FKwebiha/01.DATIM4U.FY2024Q2.ImportReady.HTS_SELF_Community_2024-02-04_00:50_EAT.csv"),
                                       httr::verbose(),
                                       httr::authenticate("FKwebiha_DATIMGlobal","21!September!2023", type = "basic"),
                                       httr::content_type("application/csv"),
                                       httr::progress("up"),
                                       httr::set_config(httr::config(ssl_verifypeer = 0L))
)

# get content response
httr::content(postDatavalues, "text", encoding = "UTF-8")