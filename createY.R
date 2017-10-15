library(xts)
library(lubridate)
library(data.table)

setwd("~/Grad School/UCLA/AFP/data/data")

#read all files
filenames <- list.files(path = "~/Grad School/UCLA/AFP/data/data", pattern="*.csv", full.names=TRUE)
futures_list <- lapply(filenames, function(x) read.csv(x, skip = 1, header = T, stringsAsFactors = F))

#extract product names
all_names <- lapply(futures_list, function(x) unique(substr(x$SYMBOL_NAME, 8, 9)))
all_names <- unlist(all_names)

#classify price changes +1,0,-1
price_change <- function(file1, threshold) {
      file1 <- as.data.table(file1)
      
      file1$X.TIMESTAMP <- ymd_hms(file1$X.TIMESTAMP)
      file1[, LAST := na.locf(LAST, na.rm = F, fromLast = F)]
      
      file1[, change := ifelse(shift(LAST, type = "lead") > LAST * (1+threshold), 1, 
                               ifelse(shift(LAST, type = "lead") < LAST * (1-threshold), -1, 0))]
      
      file1[, hour := hour(X.TIMESTAMP)]
      file1 <- file1[hour >= 10 & hour < 16]
      
      output <- matrix(nrow = 3, ncol = nrow(file1))
      
      for (j in 1:ncol(output)) {
            if (file1$change[j] == -1)
                  output[, j] <- c(1,0,0)
            else if (file1$change[j] == 1)
                  output[, j] <- c(0,0,1)
            else
                  output[, j] <- c(0,1,0)
      }
      
      return(output)
}

#create Y
threshold <- 0.0005
Y_variables <- lapply(futures_list, price_change, threshold = threshold)
Y_variables <- as.matrix(do.call("rbind", Y_variables))
rownames(Y_variables) <- paste0(rep(all_names, each = 3), rep(c("-1","0","1"), 14))
