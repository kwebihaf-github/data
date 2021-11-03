# Read the Excel file
data <- readxl::read_excel("/home/fred/Downloads/DATIM4U/FY2021/Q4/Datasets/DAA/TX_NEW-MoH-Alignments.xlsx", sheet ="For-Coding", range="A1:Z1899")

#duplicate columns incase of any
duplicate_columns <-names(data)[duplicated(names(data))]
col_indexes <-which(colnames(data)==duplicate_columns)
if(length(col_indexes>=1))
{
  stop(paste0("These are the duplicate column indexes: ", col_indexes, sep=" "))
}

#transform the data
data.m <- reshape2::melt(data, id=c(1:2)) # the rest of the columns are measure.vars
data.m$value <- as.numeric(gsub(',', '', data.m$value))
data.m.split <-tidyr::separate(data = data.m, col = variable, into = c("MOH_Indicator_ID", "MOH_Disag_ID"), sep = "\\.")

#Rename columns
data.m.split <- dplyr::rename(data.m.split, Value=value)

#deal with blanks
data.m.split[is.na(data.m.split)] <- 0

#remove rows having zeroes
data.m.split <- dplyr::filter(data.m.split, Value != 0)

#Aggregate rows
ImportReady.aggregate <- aggregate(Value ~ MOH_Facility_ID+Period+MOH_Indicator_ID+MOH_Disag_ID, data=data.m.split, FUN = sum) 

#Print sums before and after
options(digits = 10) #incase there are decimal values, they will not be rounded
print (sum(as.numeric(as.character(data.m$value)),na.rm=T))
print (sum(as.numeric(as.character(ImportReady.aggregate$Value))))

#save as csv
write.csv(ImportReady.aggregate, file="/home/fred/Downloads/DATIM4U/FY2021/Q4/Datasets/DAA/Imports/TX_NEW-DAA-2021-ImportReady.csv", row.names = FALSE)