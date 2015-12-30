# Checking to see if trips should re-ordered within a submission

library(sqldf)
library(geosphere)

Calc_WRW(submission) 
Baseline <- Score

for (w in (unique(submission$TripId))){
  
  ex <- sqldf(sprintf("SELECT * FROM submission WHERE TripId='%s'", w))
  ex <- setorder(ex, -Latitude)
  totes <- sum(ex$Weight) + 10
  
  # FORWARDS 
  
  y <- 1
  while (y <= length(ex$GiftId)-1){
    # First sequence
    if ((y==1)&(ex$Weight[y]<ex$Weight[y+1])){
      p_1 <- c(0,90)
      p_2 <- c(ex$Longitude[y],ex$Latitude[y])
      p_3 <- c(ex$Longitude[y+1],ex$Latitude[y+1])
      p_4 <- c(ex$Longitude[y+2],ex$Latitude[y+2])
      w2 <- ex$Weight[y]
      w3 <- ex$Weight[y+1]
      w4 <- ex$Weight[y+2]
      total_w <- totes
      dist_1_2 <- distHaversine(p_1,p_2)
      dist_1_3 <- distHaversine(p_1,p_3)
      dist_2_3 <- distHaversine(p_2,p_3)
      dist_3_2 <- distHaversine(p_3,p_2)
      dist_2_4 <- distHaversine(p_2,p_4)
      dist_3_4 <- distHaversine(p_3,p_4)
      original <- (dist_1_2*total_w) + (dist_2_3*(total_w-w2)) + (dist_3_4*(total_w-w2-w3))
      hypo <- (dist_1_3*total_w) + (dist_3_2*(total_w-w3)) + (dist_2_4*(total_w-w2-w3))
      if (hypo < original){
        hippo <- ex[y,]
        ex[y,] <- ex[y+1,]
        ex[y+1,] <- hippo
        print('start')
      }
    }
    # Not the first or last sequence
    if ((y!=1) & (y!=length(ex$TripId)-1) &(ex$Weight[y]<ex$Weight[y+1])){
      p_1 <- c(ex$Longitude[y-1],ex$Latitude[y-1])
      p_2 <- c(ex$Longitude[y],ex$Latitude[y])
      p_3 <- c(ex$Longitude[y+1],ex$Latitude[y+1])
      p_4 <- c(ex$Longitude[y+2],ex$Latitude[y+2])
      w2 <- ex$Weight[y]
      w3 <- ex$Weight[y+1]
      w4 <- ex$Weight[y+2]
      total_w <- (totes - sum(ex$Weight[1:(y-1)]))
      dist_1_2 <- distHaversine(p_1,p_2)
      dist_1_3 <- distHaversine(p_1,p_3)
      dist_2_3 <- distHaversine(p_2,p_3)
      dist_3_2 <- distHaversine(p_3,p_2)
      dist_2_4 <- distHaversine(p_2,p_4)
      dist_3_4 <- distHaversine(p_3,p_4)
      original <- (dist_1_2*total_w) + (dist_2_3*(total_w-w2)) + (dist_3_4*(total_w-w2-w3))
      hypo <- (dist_1_3*total_w) + (dist_3_2*(total_w-w3)) + (dist_2_4*(total_w-w2-w3))
      if (hypo < original){
        hippo <- ex[y,]
        ex[y,] <- ex[y+1,]
        ex[y+1,] <- hippo
        print(y)
      }
    }
    # Last Sequence
    if (y==length(((ex$TripId)-1))&(ex$Weight[y]<ex$Weight[y+1])){
      p_1 <- c(ex$Longitude[y-1],ex$Latitude[y-1])
      p_2 <- c(ex$Longitude[y],ex$Latitude[y])
      p_3 <- c(ex$Longitude[y+1],ex$Latitude[y+1])
      p_4 <- c(0,90)
      w2 <- ex$Weight[y]
      w3 <- ex$Weight[y+1]
      w4 <- 0
      total_w <- (totes - sum(ex$Weight[1:(y-1)]))
      dist_1_2 <- distHaversine(p_1,p_2)
      dist_1_3 <- distHaversine(p_1,p_3)
      dist_2_3 <- distHaversine(p_2,p_3)
      dist_3_2 <- distHaversine(p_3,p_2)
      dist_2_4 <- distHaversine(p_2,p_4)
      dist_3_4 <- distHaversine(p_3,p_4)
      original <- (dist_1_2*total_w) + (dist_2_3*(total_w-w2)) + (dist_3_4*(total_w-w2-w3))
      hypo <- (dist_1_3*total_w) + (dist_3_2*(total_w-w3)) + (dist_2_4*(total_w-w2-w3))
      if (hypo < original){
        hippo <- ex[y,]
        ex[y,] <- ex[y+1,]
        ex[y+1,] <- hippo
        print('end')
      }
    }
    y <- y + 1
  }
  
  # BACKWARDS 
  
  y <- length(ex$GiftId)-1
  while (y >= 1){
    # First sequence
    if ((y==1)&(ex$Weight[y]<ex$Weight[y+1])){
      p_1 <- c(0,90)
      p_2 <- c(ex$Longitude[y],ex$Latitude[y])
      p_3 <- c(ex$Longitude[y+1],ex$Latitude[y+1])
      p_4 <- c(ex$Longitude[y+2],ex$Latitude[y+2])
      w2 <- ex$Weight[y]
      w3 <- ex$Weight[y+1]
      w4 <- ex$Weight[y+2]
      total_w <- totes
      dist_1_2 <- distHaversine(p_1,p_2)
      dist_1_3 <- distHaversine(p_1,p_3)
      dist_2_3 <- distHaversine(p_2,p_3)
      dist_3_2 <- distHaversine(p_3,p_2)
      dist_2_4 <- distHaversine(p_2,p_4)
      dist_3_4 <- distHaversine(p_3,p_4)
      original <- (dist_1_2*total_w) + (dist_2_3*(total_w-w2)) + (dist_3_4*(total_w-w2-w3))
      hypo <- (dist_1_3*total_w) + (dist_3_2*(total_w-w3)) + (dist_2_4*(total_w-w2-w3))
      if (hypo < original){
        hippo <- ex[y,]
        ex[y,] <- ex[y+1,]
        ex[y+1,] <- hippo
        print('start')
      }
    }
    # Not the first or last sequence
    if ((y!=1) & (y!=length(ex$TripId)-1) &(ex$Weight[y]<ex$Weight[y+1])){
      p_1 <- c(ex$Longitude[y-1],ex$Latitude[y-1])
      p_2 <- c(ex$Longitude[y],ex$Latitude[y])
      p_3 <- c(ex$Longitude[y+1],ex$Latitude[y+1])
      p_4 <- c(ex$Longitude[y+2],ex$Latitude[y+2])
      w2 <- ex$Weight[y]
      w3 <- ex$Weight[y+1]
      w4 <- ex$Weight[y+2]
      total_w <- (totes - sum(ex$Weight[1:(y-1)]))
      dist_1_2 <- distHaversine(p_1,p_2)
      dist_1_3 <- distHaversine(p_1,p_3)
      dist_2_3 <- distHaversine(p_2,p_3)
      dist_3_2 <- distHaversine(p_3,p_2)
      dist_2_4 <- distHaversine(p_2,p_4)
      dist_3_4 <- distHaversine(p_3,p_4)
      original <- (dist_1_2*total_w) + (dist_2_3*(total_w-w2)) + (dist_3_4*(total_w-w2-w3))
      hypo <- (dist_1_3*total_w) + (dist_3_2*(total_w-w3)) + (dist_2_4*(total_w-w2-w3))
      if (hypo < original){
        hippo <- ex[y,]
        ex[y,] <- ex[y+1,]
        ex[y+1,] <- hippo
        print(y)
      }
    }
    # Last Sequence
    if (y==length(((ex$TripId)-1))&(ex$Weight[y]<ex$Weight[y+1])){
      p_1 <- c(ex$Longitude[y-1],ex$Latitude[y-1])
      p_2 <- c(ex$Longitude[y],ex$Latitude[y])
      p_3 <- c(ex$Longitude[y+1],ex$Latitude[y+1])
      p_4 <- c(0,90)
      w2 <- ex$Weight[y]
      w3 <- ex$Weight[y+1]
      w4 <- 0
      total_w <- (totes - sum(ex$Weight[1:(y-1)]))
      dist_1_2 <- distHaversine(p_1,p_2)
      dist_1_3 <- distHaversine(p_1,p_3)
      dist_2_3 <- distHaversine(p_2,p_3)
      dist_3_2 <- distHaversine(p_3,p_2)
      dist_2_4 <- distHaversine(p_2,p_4)
      dist_3_4 <- distHaversine(p_3,p_4)
      original <- (dist_1_2*total_w) + (dist_2_3*(total_w-w2)) + (dist_3_4*(total_w-w2-w3))
      hypo <- (dist_1_3*total_w) + (dist_3_2*(total_w-w3)) + (dist_2_4*(total_w-w2-w3))
      if (hypo < original){
        hippo <- ex[y,]
        ex[y,] <- ex[y+1,]
        ex[y+1,] <- hippo
        print('end')
      }
    }
    y <- y-1
  }
  submission <- submission[submission$TripId != w]
  submission <- rbind(submission,ex)
}

Calc_WRW(submission)
Score

