## data QA&QC (DATIM Pre-Coding), Start

#  Install Packages
.packages <- c("here", "readxl", "openxlsx")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)


# read files into R and choose which indicators to work with
#files <- "/home/rstudio/files/data/20_FY2019 Q4 DATIM Data version 18_as of 17th 12 2019.xlsx"
data_Prep <-readxl::read_excel(here::here("files","HTS_SELF_facility-Extracted_ver13_FY2020Q2_.xlsx"), range = "HTS_SELF_facility!A3:CO402")
eHMIS_orgunits <- read.csv(here::here("files","eHMIS-organisationUnits (5June2020).csv"),header = TRUE,sep = ",",stringsAsFactors = FALSE)
DAPTS_list <- readxl::read_excel(here::here("files","18_Revised FY2020 DAPTS ME Version_21-05-2020.xlsx"), sheet=2, range = "C3:R2256") # used sheet index and provided range separately
 

#checks
has_neg <- data.frame(Negatives=apply(data_Prep[,-(1:12)], 1, function(row) any(row < 0,na.rm=TRUE)))
Length_Outlet_IDs<-nchar(data_Prep$col_8)

has_blanks_IMCode <- data.frame(BlanksIMCode=apply(data_Prep[10], 1, function(row) any(is.na(row))))
has_blanks_DATIMOutletID <- data.frame(BlanksDATIMOutletID=apply(data_Prep[8], 1, function(row) any(is.na(row))))
has_blanks_TypeOfSupport <- data.frame(BlanksTypeOfSupport=apply(data_Prep[9], 1, function(row) any(is.na(row))))

swapped_orgunits_with_eHMIS <-data.frame(swapped_orgunits_with_eHMIS=as.character(data_Prep$...6 %in% unlist(eHMIS_orgunits$id))) # 6th column contains DATIM orgunit ID

orgunits_belongs_to_IM <- data.frame(orgunits_belongs_to_IM=as.character(paste(data_Prep$...6,data_Prep$...13, sep = ";") %in% unlist(paste(DAPTS_list$`DATIM ID`,DAPTS_list$`IM Code`, sep = ";")))) # compares concatenated values from data side (pairwise) and the DAPTS list (pairwise)


#final dataframe
data_Prep_Negatives <-cbind(data_Prep,has_neg,Length_Outlet_IDs,has_blanks_IMCode,has_blanks_DATIMOutletID,has_blanks_TypeOfSupport, swapped_orgunits_with_eHMIS,orgunits_belongs_to_IM)

# Excel headerStyles
#--https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
#--https://github.com/tidyverse/readxl#readme
#--https://github.com/tidyverse/readxl/issues/231

hs1 <- openxlsx::createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "white")
#openxlsx::write.xlsx(data_Prep_Negatives, file = "/home/rstudio/files/data/data_QAQC.xlsx",colNames = TRUE, borders = "rows", headerStyle = hs1)

#save the data into an Excel file
#--https://github.com/awalker89/openxlsx/issues/157

openxlsx::write.xlsx(list("TX_PVLS_1"=data_Prep_Negatives,"TX_PVLS_2"=data_Prep_Negatives), file = here::here("files/data", "data_QAQC.xlsx"),colNames = TRUE, borders = "rows", headerStyle = hs1, overwrite=TRUE) # for several worksheets in a single file

## data QA&QC (DATIM Pre-Coding), End
