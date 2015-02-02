
# Access and Clean Data ---------------------------------------------------
setwd("~")
getwd()
setwd("Dropbox/DropData/18Reasons/")
list.files()

# unzip files
unzip(zipfile = "resavetimethisyearwithwashioandsprig.zip")

dataFileNames <- list.files()
# lapply(dfCSV, names)
# consider renaming files but it doesn't really matter right now file.rename()
# file.rename(from = list.files(pattern=" "), to = "" )
# eliminate source zip file from list; this might not be necesssary but it's cool I suppose
dataFileNames <- dataFileNames[-which(dataFileNames %in% dataFileNamesZIP)]
# name the elements of the list with the filenames
names(dataFileNames) <- str_replace_all(dataFileNames, pattern = " ","_")

dataFileNamesCSV <- list.files(pattern = ".csv")
CSV_name_list <- str_replace_all(dataFileNamesCSV, pattern = " ","_")
CSV_name_list <- str_replace_all(CSV_name_list, pattern = ".csv","")
CSV_name_list

dataFileNamesXLSX <- list.files(pattern = ".xlsx")
XLXS_name_list <- str_replace_all(dataFileNamesXLSX, pattern = " ","_")
XLXS_name_list <- str_replace_all(XLXS_name_list, pattern = ".xlsx","")
XLXS_name_list

# dataFileNamesXLS <- list.files(pattern = ".xls")
dataFileNamesZIP <- list.files(pattern = ".zip")

listCSV <- lapply(dataFileNamesCSV, function(x) read.csv(file = x, header = TRUE, stringsAsFactors = F))
head(listCSV[[1]][1:76])
listXLSX <- lapply(dataFileNamesXLSX, function(x) read.xlsx(file = x, sheetIndex = 1, header = TRUE))
names(listXLSX)


# loop on all files to create list and dataframe                                
for (i in 1: length(listCSV)) {
  as.data.frame(assign(paste0("df_",CSV_name_list[[i]]), listCSV[[i]]))
}                                

for (i in 1: length(listXLSX)) {
  as.data.frame(assign(paste0("df_",XLXS_name_list[[i]]), listXLSX[[i]]))
}                                

str(df_Donors)
str(df_Members_from_all_time_with_addresses)



rm(list = ls(pattern = "df_"))
