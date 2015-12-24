
# Calculate WRW on your submission. 

# Accepts a data frame with the five standard columns. 
# Need to make sure your cluster column is renamed to 'TripId'
# Ex) colnames(submission)[5] <- "TripId"

library(geosphere)

Calc_WRW <- function(x){
  
  Tot_WRW <- vector("numeric")
  #If you want to take descending Lat approach - orig_s <- x[order(-(x$Latitude)),]
  orig_s <- x
  
  for (p in (unique(x$TripId))){
    orig_sel <- subset(orig_s,TripId==p)
    dropped <- c(0)
    WRW <- vector("numeric")
    n <- 1
    all_weight <- sum(orig_sel$Weight)
    
    while (n <= (length(orig_sel$GiftId)+1)){
      if (n==1){
        p1 <- c(0,90)
        p2 <- c(orig_sel$Longitude[n],orig_sel$Latitude[n])
      }
      if (n==length(orig_sel$GiftId)+1){
        p1 <- c(orig_sel$Longitude[n-1],orig_sel$Latitude[n-1])
        p2 <- c(0,90)
      }
      if ((n!=1) & (n!=length(orig_sel$GiftId)+1)) {
        p1 <- c(orig_sel$Longitude[n-1],orig_sel$Latitude[n-1])
        p2 <- c(orig_sel$Longitude[n],orig_sel$Latitude[n])
      }
      
      dist <- distHaversine(p1,p2)
      weight <- (all_weight - sum(dropped) + 10) 
      rw <- weight * dist
      WRW <- c(WRW,rw)
      dropped <- c(dropped,orig_sel$Weight[n])
      n <- n + 1
    }
    Tot_WRW <- c(Tot_WRW,sum(WRW))
    rm(WRW)
    rm(dropped)
    rm(orig_sel)
  }
  
  Score <<- (sum(Tot_WRW))
}

# Check variable 'Score' for your output
# Or just use a return statement...
