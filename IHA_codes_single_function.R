# Function for the number of days in any month
daysinmonth <- function(seqmonth){
  library(lubridate)
  return(as.vector(days_in_month(seqmonth)))
}

# Function for day of water year calculation
day_wy <- function(st_month_WY, yr){
  library(tis)
  mo_days <- vector(mode = 'integer', length = 12)
  if (isLeapYear(yr) == F){
    
    for (i in 1:length(mo_days)){
      seqmonth <- i + st_month_WY - 1
      if ((seqmonth) <= 12){
        mo_days[i] <- daysinmonth(seqmonth)
      }else{
        mo_days[i] <- daysinmonth(seqmonth - 12) # subtracting 12 returns to january
      }
    }
  }else{

    for (i in 1:length(mo_days)){
      seqmonth <- i + st_month_WY - 1
      if (seqmonth <= 12){
        if (seqmonth == 2){
          mo_days[i] <- daysinmonth(seqmonth) + 1
        }else{
          mo_days[i] <- daysinmonth(seqmonth)
        }
      }else{
        if ((seqmonth - 12) == 2){
          mo_days[i] <- daysinmonth(seqmonth - 12) + 1
        }else{
          mo_days[i] <- daysinmonth(seqmonth - 12) # subtracting 12 returns to january
        }
      }
    }
  }
  mo_days <- c(0,cumsum(mo_days)[1:11])
  if (st_month_WY != 1){
    indx_dec <- 12 - st_month_WY + 1 # Index of Dec
    mo_days <- mo_days[c((indx_dec + 1):12, 1:indx_dec)]
  }
  return(mo_days)
}

# Prepare WRIS data for Indian rivers to daily flow format
WRIS2dailyQ <- function(data, st_month_WY = 6){
  cal_date <- data[, 1]
  # Split the date column and create new columns for day, month, and year
  date_parts <- sapply(strsplit(as.character(cal_date ), "/"), function(x) as.numeric(x))
  date_parts_sep <- t(date_parts) # transverse of date_parts
  nnn <- dim(data)[1]
  date_Q_mat <- matrix(NA, nrow = nnn, ncol = 4)
  colnames(date_Q_mat) <- c('Day', 'Month', ' Year', 'Discharge')
  date_Q_mat[,1] <- date_parts_sep[,1]
  date_Q_mat[,2] <- date_parts_sep[,2]
  date_Q_mat[,3] <- date_parts_sep[,3]
  date_Q_mat[,4] <- data[, 4]
  # To covert it into water year format, we need to add water year and day of wateryear columns
  daily_Q_mat <- matrix(NA, dim(date_Q_mat)[1],6)
  colnames(daily_Q_mat) <- c("WY","Year","Month","Day","DOWY","Q") # this has to be the format for the IAH functions input
  daily_Q_mat[,2]<- date_Q_mat[,3] # year
  daily_Q_mat[,3]<- date_Q_mat[,2]# month
  daily_Q_mat[,4]<- date_Q_mat[,1] # day of month
  daily_Q_mat[,1]<- ifelse(date_Q_mat[,2] < st_month_WY, date_Q_mat[,3], date_Q_mat[,3] + 1)
  daily_Q_mat[,6]<- date_Q_mat[,4] # discharge

  # For dowy
  # Calculate DOWY for each row
  n1 <- dim(daily_Q_mat)[1]
  for(k in 1:n1){
    # print(k)
    mo_days <- day_wy(st_month_WY, daily_Q_mat[k,2])
    daily_Q_mat[k,5] <- mo_days[daily_Q_mat[k,3]]+daily_Q_mat[k,4]
  }
  return(daily_Q_mat)
}

# Prepare USGS station data to daily flow format
USGS2dailyQ <- function(rawDailyQ, st_month_WY = 6){
  n1 <- dim(rawDailyQ)[1]
  dailyQ.matrix<-matrix(NA,n1,6)
  colnames(dailyQ.matrix)<-c("WY","Year","Month","Day","DOWY","Q") # this has to be the format for the IAH functions input
  dailyQ.matrix[,6]<-as.vector(rawDailyQ[,4])
  # dailyQ.matrix[,1]<-calcWaterYear(rawDailyQ[,3])
  
  # Filling up day, month, year column in the daily Q matrix
  for(k in 1:n1){
    # print(k)
    a1<-strsplit(as.character(rawDailyQ[k,3]),"-")
    dailyQ.matrix[k,2]<-as.numeric(a1[[1]][1])
    dailyQ.matrix[k,3]<-as.numeric(a1[[1]][2])
    dailyQ.matrix[k,4]<-as.numeric(a1[[1]][3])
  }
  dailyQ.matrix[,1]<- ifelse(dailyQ.matrix[,3] < st_month_WY, dailyQ.matrix[,2], dailyQ.matrix[,2] + 1)
  # For dowy
  # Calculate DOWY for each row
  n1 <- dim(dailyQ.matrix)[1]
  for(k in 1:n1){
    # print(k)
    mo_days <- day_wy(st_month_WY, dailyQ.matrix[k,2])
    dailyQ.matrix[k,5] <- mo_days[dailyQ.matrix[k,3]]+dailyQ.matrix[k,4]
  }
  #### Matrix construction completed!
  return(dailyQ.matrix)
}

# Modify dailyQ data to average the leap year data to 365 days flow data
averageLeapYear<-function(dailyQ.matrix){
  # to modify/restructure the daily flow data so as to account for any leap year
  data <- dailyQ.matrix
  n_yr<-length(unique(data[,1]))
  wy_data_yrs<-unique(data[,1]) 
  reshape_dailyQ <- matrix(NA,n_yr,365)
  rownames(reshape_dailyQ) <- wy_data_yrs
  colnames(reshape_dailyQ) <- 1:365
  for (i in 1:length(unique_years)){
    print(i)
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
  return(reshape_dailyQ)
}

# Construct IAH functions ----
# IHA Group 1 : Monthly mean flow ===== 
IHA_Group01_Analysis<-function(data=dailyQ.matrix, st_month_WY = 6){
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
  mo_seq<-{if (st_month_WY != 1){
            c(st_month_WY:12,1:(st_month_WY-1))
          }else{
            c(1:12)
          }}
  colnames(result_monthlymean) <- mo_seq
  
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
  
  data <- averageLeapYear(data) # Since we don't need monthly information. This is just a daily data for each year
  n_yr<-dim(data)[1]
  wy_data_yrs<-as.numeric(rownames(data))
  result_group3_timing <- data.frame(matrix(NA, ncol = 2, nrow = n_yr))
  rownames(result_group3_timing) <- wy_data_yrs
  colnames(result_group3_timing)<- c("DOWYMax", "DOWYMin")
  
  # Main code for estimating the dowy for annual maxima and minima
  for (i in 1:n_yr) {
    # Extract flow values for the current year
    flowdata_year_i <- data[i, ] 
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
  #' 4 IAH parameters (group4) (Richter et al., 1996) will be generated through IAH_Group4 function
  #' @param data A matrix with daily flow data.
  #' @return Frequency and mean duration of high and low pulses of the daily flow data in `data`.
  #' @examples
  #'
  
  data <- averageLeapYear(data)
  n_yr<-dim(data)[1]
  wy_data_yrs<-as.numeric(rownames(data))
  
  # Main code for estimating the frequency and mean duration for annual high (low) flow pulses
  result_group4 <- data.frame(matrix(NA, ncol = 4, nrow = n_yr))
  rownames(result_group4) <- wy_data_yrs
  colnames(result_group4) <- c("MeanD_HFlow", "Freq_HFlow", "MeanD_LFlow", "Freq_LFlow")
  
  for (i in 1:n_yr) {
    # Extract flow values for the current year
    flowdata_year_i <- data[i, ]  
    
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

# IAH Group 5: ===== 
IHA_Group05_Analysis<-function(data=dailyQ.matrix){
  #' Calculates the mean of flow rise and flow fall along with the frequency of these rises (falls).
  #' one rise (fall) event is calculated from the beginning of a rise event to the start of next fall (ans so on)
  #' 4 IAH parameters (group5) (Richter et al., 1996) will be generated through IAH_Group5 function
  #' @param data A matrix with daily flow data.
  #' @return mean and frequency of rise (fall) of the daily flow data in `data`.
  
  data <- averageLeapYear(data)
  n_yr<-dim(data)[1]
  wy_data_yrs<-as.numeric(rownames(data))
  result_group5 <- data.frame(matrix(NA, ncol = 4, nrow = n_yr))
  rownames(result_group5)<-wy_data_yrs
  colnames(result_group5)<-c("Mean_Pos_dif", "Mean_Neg_dif", "num_rises", "num_falls")
  
  for (i in 1:n_yr) {
    # Subset rows for the current year
    flow_y <- data[i, ]
    flow_y_df <- diff(flow_y)
    list_pos <- NULL
    list_df_pos <- NULL
    list_df_neg <- NULL
    j <- 1
    while (j <= length(flow_y_df)){
      if (flow_y_df[j] >= 0){
        k <- 1
        j <- j + 1
        while (flow_y_df[j] >= 0 & j <= length(flow_y_df)){
          k <- k + 1
          j <- j + 1
        }
        list_df_pos <- cbind(list_df_pos, k)
      }else{
        k <- 1
        j <- j + 1
        while (flow_y_df[j] <= 0 & j <= length(flow_y_df)){
          k <- k + 1
          j <- j + 1
        }
        list_df_neg <- cbind(list_df_neg, k)
      }
    }
    
    result_group5$Mean_Pos_dif[year] <- mean((flow_y_df>0), na.rm = TRUE)
    result_group5$Mean_Neg_dif[year] <- mean((flow_y_df<0), na.rm = TRUE)
    result_group5$num_rises[year] <- length(list_df_pos) 
    result_group5$num_falls[year] <- length(list_df_neg)
  }
  return(result_group5)
}
# Construction of the all the IHA functions complete


