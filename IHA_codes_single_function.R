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
############################################################
#
for(k in 1:n1){
  print(k)
  if(isLeapYear(dailyQ.matrix[k,1])==T) {dailyQ.matrix[k,5]<-(mo.days.l[dailyQ.matrix[k,3]]+dailyQ.matrix[k,4])}
  else {dailyQ.matrix[k,5]<-(mo.days[dailyQ.matrix[k,3]]+dailyQ.matrix[k,4])}
}
#### Matrix construction completed!

# Construct IAH functions ----
# IHA Group 1 : Monthly mean flow ===== 
IHA_Group01_Analysis<-function(data=dailyQ.matrix){
  #' Calculates the Monthly mean flow for all water years.
  #' 12 IAH parameters (group1) (Richter et al., 1996) will be generated through IAH_Group1 function
  #' @param data A matrix with daily flow data.
  #' @return The monthly mean of the daily flow data in `data`.
  #' @examples
  #' result_monthlymean <- IHA_Group01_Analysis(data = dailyQ.matrix)

  dim.data<-dim(data)
  n_yr<-length(unique(data[,1]))
  wy_data_yrs<-unique(data[,1])
  result_monthlymean <- data.frame(matrix(NA, ncol = 12, nrow = n_yr))
  rownames(result_monthlymean)<-wy_data_yrs
  colnames(result_monthlymean)<-c(10,11,12,1,2,3,4,5,6,7,8,9)
  mo_seq<-c(10:12,1:9)
  
  for (i in 1:length(wy_data_yrs)) 
    {for (k in 1:12){
      result_monthlymean[i,k]<-mean(data[data[,1]==wy_data_yrs[i] & data[,3]==mo_seq[k],6], na.rm = TRUE)
    }
  }
  return(result_monthlymean)
}

# IHA Group 2 : Annual Maxima of 1 day means, 3 day means, 7 day means, 30 day means, 90 day means flow ===== 
IHA_Group02_Analysis<-function(data=dailyQ.matrix){
  #' Calculates the max (min) n-day moving mean annual flow for all water years.
  #' 10 IAH parameters (group2) (Richter et al., 1996) will be generated through IAH_Group2 function
  #' @param data A matrix with daily flow data.
  #' @return n-day annual maxima (minima) of the daily flow data in `data`.
  #' @examples
  #' 
  
  library(zoo) # for rollapply function to calculate moving average
  dim.data<-dim(data)
  n_yr<-length(unique(data[,1]))
  wy_data_yrs<-unique(data[,1])
  result_IHA_group2 <- data.frame(matrix(NA, ncol = 10, nrow = n_yr))
  rownames(result_IHA_group2) <-wy_data_yrs
  colnames(result_IHA_group2) <-c("Max_1daymean", "Max_3daymean", "Max_7daymean", "Max_30daymean", "Max_90daymean", 
                                  "Min_1daymean", "Min_3daymean", "Min_7daymean", "Min_30daymean", "Min_90daymean")
  for (i in 1:n_yr){
    flowdata_year_i <- dailyQ.matrix[dailyQ.matrix[,1]==wy_data_yrs[i], 6] # subsetting the 6th column data (flow data) for the ith water year
    # Calculating moving average with the rollapply function from the zoo library for different width
    subset_3day <- rollapply(flowdata_year_i, width = 3, FUN = mean, partial = TRUE, align = "right")
    subset_7day <- rollapply(flowdata_year_i, width = 7, FUN = mean, partial = TRUE, align = "right")
    subset_30day <- rollapply(flowdata_year_i, width = 30, FUN = mean, partial = TRUE, align = "right")
    subset_90day <- rollapply(flowdata_year_i, width = 90, FUN = mean, partial = TRUE, align = "right")
    # Fill up the result matrix by maximum/minimum values of the moving averaged flow value for the ith water year
    result_IHA_group2$Max_1daymean[i] <- max(flowdata_year_i)
    result_IHA_group2$Max_3daymean[i] <- max(subset_3day)
    result_IHA_group2$Max_7daymean[i] <- max(subset_7day)
    result_IHA_group2$Max_30daymean[i] <- max(subset_30day)
    result_IHA_group2$Max_90daymean[i] <- max(subset_90day)
    result_IHA_group2$Max_1daymean[i] <- min(flowdata_year_i)
    result_IHA_group2$Max_3daymean[i] <- min(subset_3day)
    result_IHA_group2$Max_7daymean[i] <- min(subset_7day)
    result_IHA_group2$Max_30daymean[i] <- min(subset_30day)
    result_IHA_group2$Max_90daymean[i] <- min(subset_90day)
  }
  return(result_IHA_group2)
}

# IAH Group 3: Julian Dates Annual maxima Flow and Annual Minima Flow ===== 
IHA_Group03_Analysis<-function(data=dailyQ.matrix){
  #' Calculates the julian date of annual maxima / minima flow condition.
  #' 2 IAH parameters (group3) (Richter et al., 1996) will be generated through IAH_Group3 function
  #' @param data A matrix with daily flow data.
  #' @return julian date of annual maxima / minima flow of the daily flow data in `data`.
  #' @examples
  #' 
  
  n_yr<-length(unique(data[,1]))
  wy_data_yrs<-unique(data[,1])
  result_group3_timing <- data.frame(matrix(NA, ncol = 2, nrow = n_yr))
  rownames(result_IHA_group2) <-wy_data_yrs
  colnames(result_group3_timing)<-c("DOWYMax", "DOWYMin")
  
  # We need to modify/restructure the daily flow data so as to account for any leap year
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
  # Main code for estimating the dowy for annual maxima and minima
  for (i in 1:n_yr) {
    # Extract flow values for the current year
    flowdata_year_i <- reshape_dailyQ[i, ] 
    # Find the index of the maximum value in the row
    max_index <- which.max(flowdata_year_i)
    DOWY_max_index <- max(which.max(flowdata_year_i))
    min_index <- which.min(flowdata_year_i)
    DOWY_min_index <- max(which.min(flowdata_year_i))
    result_group3_timing$DOWYMax[i] <- DOWY_max_index
    result_group3_timing$DOWYMin[i] <- DOWY_min_index
  }
  return(result_group3_timing)
}

# IAH Group 4: Magnitude and Frequency of high and Low flow ===== 
IHA_Group04_Analysis<-function(data=dailyQ.matrix){
  #' Calculates the frequency and mean duration for high/low flow pulses.
  #' High and low threshold are 75th and 25th annual flow respectively.
  #' 4 IAH parameters (group3) (Richter et al., 1996) will be generated through IAH_Group4 function
  #' @param data A matrix with daily flow data.
  #' @return Frequency and mean duration of high and low pulses of the daily flow data in `data`.
  #' @examples
  #'
  
  n_yr<-length(unique(data[,1]))
  wy_data_yrs<-unique(data[,1])
  result_group3_timing <- data.frame(matrix(NA, ncol = 2, nrow = n_yr))
  rownames(result_IHA_group2) <-wy_data_yrs
  colnames(result_group3_timing)<-c("DOWYMax", "DOWYMin")
  
  # We need to modify/restructure the daily flow data so as to account for any leap year
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
  # Main code for estimating the frequency and mean duration for annual high (low) flow pulses
  result_group4 <- data.frame(matrix(NA, ncol = 4, nrow = n_yr))
  rownames(result_group4) <- wy_data_yrs
  colnames(result_group4) <- c("MeanD_HFlow", "Freq_HFlow", "MeanD_LFlow", "Freq_LFlow")
  
  for (i in 1:84) {
    # Extract flow values for the current year
    flowdata_year_i <- reshape_dailyQ[i, ]  
    
    #Find the quantiles (25th, and 75th percentiles) of the vector
    quant_values_year_i <- quantile(flowdata_year_i, probs = c(0.25,0.75))
    q75 <- quant_values_year_i[2]
    q25 <- quant_values_year_i[1]
    
    # For high flow pulses
    list_high_pulses <- NULL
    j <- 1
    while (j <= length(flowdata_year_i)){
      if (flowdata_year_i[j] >= q75){
        k <- 1
        j <- j + 1
        while (flowdata_year_i[j] >= q75 & j <= length(flowdata_year_i)){
          k <- k + 1
          j <- j + 1
        }
        list_high_pulses <- cbind(list_high_pulses, k)
      }else{
        j <- j + 1
      }
    }
    result_group4$MeanD_HFlow[i] <- mean(list_high_pulses)
    result_group4$Freq_HFlow[i] <- length(list_high_pulses)
    
    # For low flow pulses
    list_low_pulses <- NULL
    j <- 1
    while (j <= length(flowdata_year_i)){
      if (flowdata_year_i[j] <= q25){
        k <- 1
        j <- j + 1
        while (flowdata_year_i[j] <= q25 & j <= length(flowdata_year_i)){
          k <- k + 1
          j <- j + 1
        }
        list_low_pulses <- cbind(list_low_pulses, k)
      }else{
        j <- j + 1
      }
    }
    result_group4$MeanD_LFlow[i] <- mean(list_low_pulses)
    result_group4$Freq_LFlow[i] <- length(list_low_pulses)
  }
  return(result_group4)
}
# =====
# IAH Group 5: ===== 
IHA_Group04_Analysis<-function(data=dailyQ.matrix){
  
}
# Construction of the all the IHA functions complete


