closest_point  <- function(features, reference) {
# 'features' : block, position
# 'reference': block, position

   reference <- reference[complete.cases(reference),]

   closest <- data.frame()
   for (block in unique(features[,1])) {
      x <- features[features[,1] == block,]
      y <- reference[reference[,1] == block,]
      y <- y[order(y[,2]),]
      bounds <- c(-Inf, y[,2], Inf)
      i <- findInterval(x[,2], bounds)
      dist_to_next <- bounds[-1][i] - x[,2]
      dist_to_prev <- x[,2] - bounds[-length(bounds)][i]
      id_closest <- i - (dist_to_next > dist_to_prev)
      closest <- rbind(closest_ref, y[id_closest,])
   }

   return (closest)

}
