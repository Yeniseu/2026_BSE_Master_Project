# Author: Silvia Ianeselli
# Date: 24/11/2025
# Scope: This script applies the original data transformation code to our dataset
  # The output data_transformed considers data beyond our period of interest and NAs are not yet dealth with


library(dplyr)
library(readr)  

# ============================================
# 1. LOAD DATA
# ============================================
file <- "02_Input/2026-01-MD.csv"

# define the fredmd function 
fredmd <- function(file = "", date_start = NULL, date_end = NULL, transform = TRUE) {
  # Error checking
  if (!is.logical(transform)) # check that transform is TRUE/FALSE
    stop("'transform' must be logical.")
  if ((class(date_start) != "Date") && (!is.null(date_start))) # check date start and end are either Date or null
    stop("'date_start' must be Date or NULL.")
  if ((class(date_end) != "Date") && (!is.null(date_end)))
    stop("'date_end' must be Date or NULL.")
  
  if (class(date_start) == "Date") {
    if (as.numeric(format(date_start, "%d")) != 1)
      stop("'date_start' must be Date whose day is 1.")
    if (date_start < as.Date("1959-01-01"))
      stop("'date_start' must be later than 1959-01-01.")
  }
  
  if (class(date_end) == "Date") {
    if (as.numeric(format(date_end, "%d")) != 1)
      stop("'date_end' must be Date whose day is 1.")
  }
  
  
  # Prepare raw data
  rawdata <- readr::read_csv(file, col_names = FALSE, col_types = cols(X1 = col_date(format = "%m/%d/%Y")),
                             skip = 2) # load csv skipping first 2 rows and trating first column as date
  
  rawdata <- as.data.frame(rawdata)
  
  # remove last 20 rows if they are empty/NA (???)
  row_to_remove = c()
  for (row in (nrow(rawdata)-20):nrow(rawdata)){
    if(!any(is.finite(unlist(rawdata[row, ])))){
      row_to_remove = c(row_to_remove,row)# remove NA rows
    }
  }
  if(length(row_to_remove)>0){
    rawdata = rawdata[-row_to_remove,]
  }
  
  # extract headers
  attrdata <- utils::read.csv(file, header = FALSE, nrows = 2)
  header <- c("date", unlist(attrdata[1,2:ncol(attrdata)]))
  colnames(rawdata) <- header
  
  # Change the transformation of price indices to 5
  # so that they are log-differenced only one time
  price_indices <- c(  "WPSFD49207","WPSFD49502","WPSID61","WPSID62","OILPRICEx",
                       "PPICMM","NAPMPRI","CPIAUCSL","CPIAPPSL","CPITRNSL","CPIMEDSL",
                       "CUSR0000SAC","CUUR0000SAD","CUSR0000SAS","CPIULFSL",
                       "CUUR0000SA0L2","CUSR0000SA0L5","PCEPI",
                       "DDURRG3M086SBEA","DNDGRG3M086SBEA","DSERRG3M086SBEA")
  attrdata[2, attrdata[1, ] %in% price_indices] <- "5"  
  # Store transformation codes as tcode
  tcode <- unlist(attrdata[2,2:ncol(attrdata)])
  
  
  # Subfunction transxf: data transformation based on tcodes
  transxf <- function(x, tcode) {
    # Number of observations (including missing values)
    n <- length(x)
    
    # Value close to zero
    small <- 1e-06
    
    # Allocate output variable
    y <- rep(NA, n)
    y1 <- rep(NA, n)
    
    # TRANSFORMATION: Determine case 1-7 by transformation code
    if (tcode == 1) {
      # Case 1 Level (i.e. no transformation): x(t)
      y <- x
      
    } else if (tcode == 2) {
      # Case 2 First difference: x(t)-x(t-1)
      y[2:n] <- x[2:n] - x[1:(n - 1)]
      
    } else if (tcode == 3) {
      # case 3 Second difference: (x(t)-x(t-1))-(x(t-1)-x(t-2))
      y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
      
    } else if (tcode == 4) {
      # case 4 Natural log: ln(x)
      if (min(x, na.rm = TRUE) > small)
        y <- log(x)
      
    } else if (tcode == 5) {
      # case 5 First difference of natural log: ln(x)-ln(x-1)
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[2:n] <- x[2:n] - x[1:(n - 1)]
      }
      
    } else if (tcode == 6) {
      # case 6 Second difference of natural log:
      # (ln(x)-ln(x-1))-(ln(x-1)-ln(x-2))
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
      }
      
    } else if (tcode == 7) {
      # case 7 First difference of percent change:
      # (x(t)/x(t-1)-1)-(x(t-1)/x(t-2)-1)
      y1[2:n] <- (x[2:n] - x[1:(n - 1)])/x[1:(n - 1)]
      y[3:n] <- y1[3:n] - y1[2:(n - 1)]
    }
    
    return(y)
  }
  
  
  # Transform data (loop through all columns except date and apply the appropriate transformation code)
  if (transform) {
    # Apply transformations
    N <- ncol(rawdata)
    data <- rawdata
    data[, 2:N] <- NA
    
    # Perform transformation using subfunction transxf (see below for
    # details)
    for (i in 2:N) {
      temp <- transxf(rawdata[, i], tcode[i - 1])
      data[, i] <- temp
    }
    
  } else {
    data <- rawdata
  }
  
  # Import all data from 1959 until the last month present in the data
  
  # Null case of date_start and date_end
  if (is.null(date_start))
    date_start <- as.Date("1959-01-01")
  if (is.null(date_end))
    date_end <- data[, 1][nrow(data)]
  
  
  # Subset data
  index_start <- which.max(data[, 1] == date_start)
  index_end <- which.max(data[, 1] == date_end)
  
  outdata <- data[index_start:index_end, ]
  class(outdata) <- c("data.frame", "fredmd")
  return(outdata)
  
}


# ============================================
# NOW ACTUALLY USE THE FUNCTION
# ============================================

# Get transformed data
data_transformed <- fredmd(file, transform = TRUE)

#View(data_transformed)

# save dataset
saveRDS(data_transformed, file = "02_Input/data_transformed.rds")

   
