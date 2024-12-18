## USGS daily average discharge data
## Packages used: dataRetrieval and tis
# Data retrieval in raw form for Sheepscot river ----
library(dataRetrieval)
# Sheepscot River at North Whitefield, Maine
site_id <- "01038000"
startDate <- "1938-10-01"
endDate <- "2022-09-30"
pCode <- "00060"
rawDailyQ <- readNWISdv(site_id, pCode, startDate, endDate) # raw flow data from 1938 1st Oct to 2022 30th Sept

# Import functions from IHA_codes_single_function.R
source("IHA_codes_single_function.R")
dailyQ.matrix1 <- USGS2dailyQ(rawDailyQ, st_month_WY = 5)
# reshape_dailyQ1 <- averageLeapYear(dailyQ.matrix1) # Not needed really. Operations can be done using dailyQ
res_grp1 <- IHA_Group01_Analysis(dailyQ.matrix1, st_month_WY = 5)
res_grp2 <- IHA_Group02_Analysis(dailyQ.matrix1)
res_grp3 <- IHA_Group03_Analysis(dailyQ.matrix1)
res_grp4 <- IHA_Group04_Analysis(dailyQ.matrix1)
res_grp5 <- IHA_Group05_Analysis(dailyQ.matrix1)
# Create a Daily discharge matrix from the rawQ ----
n1 <- dim(rawDailyQ)[1]
dailyQ.matrix<-matrix(NA,n1,6)
colnames(dailyQ.matrix)<-c("WY","Year","Month","Day","DOWY","Q")
dailyQ.matrix[,6]<-as.vector(rawDailyQ[,4])
dailyQ.matrix[,1]<-calcWaterYear(rawDailyQ[,3])
# Filling up day, month, year column in the daily Q matrix
for(k in 1:n1){
  print(k)
  a1<-strsplit(as.character(rawDailyQ[k,3]),"-")
  dailyQ.matrix[k,2]<-as.numeric(a1[[1]][1])
  dailyQ.matrix[k,3]<-as.numeric(a1[[1]][2])
  dailyQ.matrix[k,4]<-as.numeric(a1[[1]][3])
}
# For the 5th column, i.e. day of water year (dowy), specific to USA
library(tis)
# dowy
mo.days<-c(0,cumsum(c(31,30,31,31,28,31,30,31,30,31,31,30))[1:11])[c(4:12,1:3)]
mo.days.l<-c(0,cumsum(c(31,30,31,31,29,31,30,31,30,31,31,30))[1:11])[c(4:12,1:3)]
for(k in 1:n1){
  print(k)
  if(isLeapYear(dailyQ.matrix[k,1])==T) {dailyQ.matrix[k,5]<-(mo.days.l[dailyQ.matrix[k,3]]+dailyQ.matrix[k,4])}
  else {dailyQ.matrix[k,5]<-(mo.days[dailyQ.matrix[k,3]]+dailyQ.matrix[k,4])}
}
#### Matrix construction completed!



# We need to modify/restructure the daily flow data so as to account for any leap year
data <- dailyQ.matrix
n_yr<-length(unique(data[,1]))
wy_data_yrs<-unique(data[,1]) 
reshape_dailyQ <- matrix(NA,n_yr,365)
rownames(reshape_dailyQ) <- wy_data_yrs
colnames(reshape_dailyQ) <- 1:365

for (i in 1:length(unique_years)){
  Q_current_year <- data[data[, 1] == wy_data_yrs[i], ]
  if(isLeapYear(Q_current_year[1,1])==T){
    feb_28_row <- which(Q_current_year[,3] == 2 & Q_current_year[,4] == 28)
    feb_29_row <- which(Q_current_year[,3] == 2 & Q_current_year[,4] == 29)
    # Calculate the average for, replace the 28th Feb row with average and remove the 29th Feb row
    average_feb2829 <- mean(c(Q_current_year[feb_28_row, 6], Q_current_year[feb_29_row, 6]))
    Q_current_year[feb_28_row, 6] <- average_feb2829
    Q_current_year <- Q_current_year[-feb_29_row, ]
    reshape_dailyQ[i, ] <- Q_current_year [ ,6]
  }else{
    reshape_dailyQ[i, ] <- Q_current_year [ ,6]
  }
}

# Merge all the dataframes to have a unified result matrix for all the IHA metrics
# Function to merge outputs for different IHA functions
merge_all <- function(x, ..., by = "row.names") {
  L <- list(...)
  for (i in seq_along(L)) {
    x <- merge(x, L[[i]], by = by)
    rownames(x) <- x$Row.names
    x$Row.names <- NULL
  }
  return(x)
}

res <- merge_all(res_grp1, res_grp2, res_grp3, 
                 res_grp4, res_grp5)
