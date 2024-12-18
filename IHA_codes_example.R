# Import raw daily flow data ----

## USGS daily average discharge data ----
## Packages used: dataRetrieval and tis
# Data retrieval in raw form for Sheepscot river 
library(dataRetrieval)
# Sheepscot River at North Whitefield, Maine
site_id <- "01038000"
startDate <- "1938-10-01"
endDate <- "2022-09-30"
pCode <- "00060"
rawDailyQ <- readNWISdv(site_id, pCode, startDate, endDate) # raw flow data from 1938 1st Oct to 2022 30th Sept

# WRIS data ----
setwd("D:/ramendra_EPM103/Sonam/IHA_R_sscripts_PeninsularData/WRIS_DATA_SEDIMENT_DISCHARGE/krishna")
rawDailyQ <- read.csv("Sediment Data Huvinhedgi,1976-2014.csv", header = TRUE, skip = 2)

# Import functions from IHA_codes_single_function.R ----
setwd("D:/ramendra_EPM103/Sonam/IHA_R_sscripts_PeninsularData/IAH_analysis")
source("IHA_codes_single_function.R")
dailyQ.matrix1 <- USGS2dailyQ(rawDailyQ, st_month_WY = 10)
# dailyQ.matrix1 <- WRIS2dailyQ(rawDailyQ, st_month_WY = 6)
res_grp1 <- IHA_Group01_Analysis(dailyQ.matrix1, st_month_WY = 10) # st_month_WY is the starting month of water year
res_grp2 <- IHA_Group02_Analysis(dailyQ.matrix1)
res_grp3 <- IHA_Group03_Analysis(dailyQ.matrix1)
res_grp4 <- IHA_Group04_Analysis(dailyQ.matrix1)
res_grp5 <- IHA_Group05_Analysis(dailyQ.matrix1)

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

# Merge all the dataframes to have a unified result matrix for all the IHA metrics ----
res <- merge_all(res_grp1, res_grp2, res_grp3, 
                 res_grp4, res_grp5)



# Subplots for all the IHA outputs ----
# for IHA group 1 results ----
x = as.numeric(row.names(res_grp1))
cols <- colnames(res_grp1)
# First create an empty plot.
plot(1, type = 'n', xlim = c(min(x), max(x)), ylim = c(0, max(res_grp1)), 
     xlab = "year", ylab = "mean_flow")

# Create a list of colors to use for the lines.
cl <- rainbow(length(cols))
plotcol <- vector(mode = "list", length = length(cols))
# Now fill plot with data from the
# files one by one.
for(i in 1:length(cols)) {
  y <- res_grp1[cols[i]]
  lines(x, y[,], col = cl[i])
  plotcol[i] <- cl[i]
}
legend("topright", legend = cols, col = as.character(plotcol), lwd = 1,
       cex = 0.5)


