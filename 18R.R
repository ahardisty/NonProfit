
# Load Packages -----------------------------------------------------------

library(ggmap)
library(tidyr)
library(zipcode)

# Access and Clean Data ---------------------------------------------------
setwd("~")
getwd()
setwd("Dropbox/DropData/18Reasons/")
list.files()

# unzip files
unzip(zipfile = "resavetimethisyearwithwashioandsprig.zip")

dataFileNames <- list.files()
# eliminate source zip file from list; this might not be necesssary but it's cool I suppose
dataFileNamesZIP <- list.files(pattern = ".zip")
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

# loop to create CSV and XLSX lists
listCSV <- lapply(dataFileNamesCSV, function(x) read.csv(file = x, header = TRUE, stringsAsFactors = F))
head(listCSV[[1]][1:76])

listXLSX <- lapply(dataFileNamesXLSX, function(x) read.xlsx(file = x, sheetIndex = 1, header = TRUE, stringsAsFactors = F))
head(listXLSX[[1]])


# loop on all files to create list and dataframe
# add "df_" to name of each element in the list
for (i in 1: length(listCSV)) {
  as.data.frame(assign(paste0("df_",CSV_name_list[[i]]), listCSV[[i]]))
}                                

for (i in 1: length(listXLSX)) {
  as.data.frame(assign(paste0("df_",XLXS_name_list[[i]]), listXLSX[[i]]))
}                                

str(df_Donors)
str(df_Members_from_all_time_with_addresses)


# Change to tidy data -----------------------------------------------------
# find address variables in all data frames for geocode_df
# use ggmap and dplyr

# 18 th street volunteers
df_18_St_Volunteers$memberCode <- "Volunteer"
df_18_St_Volunteers$Street.Address <- unique(df_18_St_Volunteers$Street.Address)

geocode_df_18_St_Volunteers <- df_18_St_Volunteers %>%
        select (memberCode, 
                Street.Address, City, State,
                Postal.Code) %>%
        filter(!is.na(Postal.Code)) %>%
        arrange(Street.Address)
        
# 18th street classes
head(df_18th_St_registrations)

df_18th_St_registrations$memberCode <- "PaidClass"
df_18th_St_registrations$Postal.Code <- ""
geocode_df_18th_St_registrations <- df_18th_St_registrations %>%
        select (memberCode, 
                Street.Address, City, State,
                Postal.Code) %>%
        filter(!is.na(Street.Address)) %>%
        arrange(Street.Address)

names(geocode_df_18th_St_registrations)

# CM Volunteers
df_CM_Volunteers$memberCode <- "CM_Volunteer"
geocode_df_CM_Volunteers <- df_CM_Volunteers %>%
        select (memberCode, 
                Street.Address = Primary.Street, 
                City = Primary.City , 
                State = Primary.State,
                Postal.Code = Primary.Zip) %>%
        filter(!is.na(Street.Address)) %>%
        arrange(Street.Address)


# 18 Reasons Donors
df_Donors$memberCode <- "Donor"
geocode_df_Donors <- df_Donors %>%
        select (memberCode, 
                Street.Address = Address, 
                City, 
                State,
                Postal.Code = Zip) %>%
        arrange(Street.Address)

# 18 Reasons Members all time
df_Members_from_all_time_with_addresses$memberCode <- "Member"
geocode_df_Members <- df_Members_from_all_time_with_addresses %>%
        filter(Status == "Current") %>% # active members only
        select (memberCode,
                Street.Address, 
                City, 
                State = State.Province,
                Postal.Code) %>%
        filter(!is.na(Street.Address)) %>% # no missing values in Street.Address
        filter(Street.Address !=" ") %>%  # no missing values in Street.Address
        arrange(Street.Address)

# identify duplicates in subsetted data frames
dupe18RVolunteers <- which(duplicated(x = geocode_df_18_St_Volunteers$Street.Address))
dupeRegistations <- which(duplicated(x = geocode_df_18th_St_registrations$Street.Address))
dupeCMVolunteers <- which(duplicated(x = geocode_df_CM_Volunteers$Street.Address))
dupeDonors <- which(duplicated(x = geocode_df_Donors$Street.Address))
dupeMembers <- which(duplicated(x = geocode_df_Members$Street.Address))


# remove duplicates in subsetted data frames
geocode_df_18_St_Volunteers <- geocode_df_18_St_Volunteers[-dupe18RVolunteers,]
geocode_df_18th_St_registrations <- geocode_df_18th_St_registrations[-dupeRegistations,]
geocode_df_CM_Volunteers <- geocode_df_CM_Volunteers[-dupeCMVolunteers,]
geocode_df_Donors <- geocode_df_Donors[-dupeDonors,]
geocode_df_Members <- geocode_df_Members[-dupeMembers,]


# Combine and Tidy Individual Data Frames ------------------------------------------
# rbind_all
geocode_df_All <- rbind(geocode_df_18th_St_registrations, 
                            geocode_df_18_St_Volunteers, 
                            geocode_df_CM_Volunteers, 
                            geocode_df_Donors, 
                            geocode_df_Members)

dim(geocode_df_All)
summary(geocode_df_All)
filter(.data = geocode_df_All, Postal.Code =="")
geocode_df_All[1460,]
geocode_df_All$Postal.Code <- clean.zipcodes(geocode_df_All$Postal.Code)

geocode_df_All$fullAddress <- paste(geocode_df_All$Street.Address, geocode_df_All$City, geocode_df_All$State, sep = ", ")

# identify gps coordinates of unique street addresses
dupeStreet <- which(duplicated(x = geocode_df_All$Street.Address))
uniqueAddress <- geocode_df_All[-dupeStreet,]
dim(uniqueAddress)
dim(geocode_df_All)

# create sample

#create "training" data frame
uniquePost <- filter(.data = geocode_df_All, !is.na(Postal.Code))
uniqueNA <- filter(.data = geocode_df_All, is.na(Postal.Code))
dim(uniqueNA)
head(uniquePost)
# identify gps coordinates of unique postal code
uniquePostalCode <- unique(geocode_df_All$Postal.Code)
# identify gps coordinates of 
uniqueCity <- unique(geocode_df_All$City)
class(uniqueCity)
uniqueState <- unique(geocode_df_All$State)

# geocode

head(geocode_df_All)
geoData <- geocode(geocode_df_All$fullAddress)
geoData        

qplot
        
qplot(geocode_df_All,x=memberCode)

dim(geocode_df_All)
names(geocode_df_All)

geocode_group_summary <- geocode_df_All %>%
        group_by(memberCode) %>%
        summarise(n = n())

geocode_summary <- geocode_df_All %>%
        group_by(Postal.Code, City, memberCode) %>%
        summarise(unique_addresses = n()) %>%
        arrange(desc(unique_addresses))
geocode_summary

geocode_summary <- geocode_df_All %>%
        group_by(City, memberCode, Postal.Code) %>%
        summarise(unique_addresses = n()) %>%
        arrange(desc(unique_addresses))
geocode_summary

ggplot(geocode_summary, aes(x = memberCode, y = unique_addresses, fill = memberCode)) +
        geom_bar(stat="identity") +
        facet_wrap( ~ City)



ggplot(diamonds, aes(x=cut)) + geom_bar()

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")

hourly_delay <- flights %>%
        filter(!is.na(dep_delay)) %>%
        group_by(date, hour) %>%
        summarise(delay = mean(dep_delay), n = n()) %>%
filter(n > 10)
summary(hourly_delay)

per_hour <- flights %>%
filter(cancelled == 0) %>%
mutate(time = hour + minute / 60) %>%
group_by(time) %>%
summarise(
arr_delay = mean(arr_delay, na.rm = TRUE),
n = n()
)
!
qplot(time, arr_delay, data = per_hour)
qplot(time, arr_delay, data = per_hour, size = n) + scale_size_area()
qplot(time, arr_delay, data = filter(per_hour, n > 30), size = n) +
scale_size_area()
!
ggplot(filter(per_hour, n > 30), aes(time, arr_delay)) +
geom_vline(xintercept = 5:24, colour = "white", size = 2) +
geom_point()


table(geocode_summary)

(geocode_summary$Postal.Code)
        