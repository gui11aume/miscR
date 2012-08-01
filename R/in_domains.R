in_domains  <- function(features, domains) {
# 'features': block, position
# 'domains' : block, start, end

   domains$extra_id <- 1:nrow(domains)
   domains <- domains[complete.cases(domains),]

   matches <- c()
   for (block in unique(features[,1])) {
      x <- features[features[,1] == block,]
      y <- domains[domains[,1] == block,]
      y <- y[order(y[,2]),]

      # Check overlap.
      stopifnot(all(y[,3][-nrow(y)] - y[,2][-1] < 0))
      
      i_start <- findInterval(x[,2], y[,2])
      i_end   <- findInterval(x[,2], c(-Inf, y[,3]))

      # NA or domain id.
      NA_or_id <- i_start * c(NA,1)[1 + (i_start == i_end)]
      matches <- c(matches, y$extra_id[NA_or_id])
   }

   return (matches)

}
