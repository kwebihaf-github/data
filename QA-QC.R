## data QA&QC (DATIM Pre-Coding), Start
files <- "/home/rstudio/files/data/20_FY2019 Q4 DATIM Data version 18_as of 17th 12 2019.xlsx"
data_Prep <-readxl::read_excel(files, range = "TX_PVLS_DATIM!A7:GG1664")

#checks
has_neg <- data.frame(Negatives=apply(data_Prep[,-(1:12)], 1, function(row) any(row < 0,na.rm=TRUE)))
Length_Outlet_IDs<-nchar(data_Prep$col_8)

has_blanks_IMCode <- data.frame(BlanksIMCode=apply(data_Prep[10], 1, function(row) any(is.na(row))))
has_blanks_DATIMOutletID <- data.frame(BlanksDATIMOutletID=apply(data_Prep[8], 1, function(row) any(is.na(row))))
has_blanks_TypeOfSupport <- data.frame(BlanksTypeOfSupport=apply(data_Prep[9], 1, function(row) any(is.na(row))))

#final dataframe
data_Prep_Negatives <-cbind(data_Prep,has_neg,Length_Outlet_IDs,has_blanks_IMCode,has_blanks_DATIMOutletID,has_blanks_TypeOfSupport)

# Excel headerStyles
--https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
--https://github.com/tidyverse/readxl#readme
--https://github.com/tidyverse/readxl/issues/231

hs1 <- openxlsx::createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "white")
#openxlsx::write.xlsx(data_Prep_Negatives, file = "/home/rstudio/files/data/data_QAQC.xlsx",colNames = TRUE, borders = "rows", headerStyle = hs1)

#save the data into an Excel file
--https://github.com/awalker89/openxlsx/issues/157

openxlsx::write.xlsx(list("TX_PVLS_1"=data_Prep_Negatives,"TX_PVLS_2"=data_Prep_Negatives), file = "/home/rstudio/files/data/data_QAQC.xlsx",colNames = TRUE, borders = "rows", headerStyle = hs1, overwrite=TRUE) # for several worksheets in a single file

## data QA&QC (DATIM Pre-Coding), End
