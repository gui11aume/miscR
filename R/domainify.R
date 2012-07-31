domainify <- function(x) {
   # Transform a 0-1 data.frame into a domain data.frame
   # PARAMETERS:
   #   'x' 'data.frame' with columns seqnanme, start, end, 0-1.
   # RETURN:
   #   a 'data.frame' with columns seqname, start, end.

   # Sort 'x' on block name and start.
   x <- x[order(x[,1], x[,2]),]

   domains <- data.frame()
   for (block in unique(x[,1])) {
      # 'y' is the restriction of 'x' to given block.
      y <- x[x[,1] == block,]
      starts <- which(diff(y[,4]) == 1) + 1
      ends   <- which(diff(y[,4]) == -1)
      if (head(y[,4],1)) starts <- c(1, starts)
      if (tail(y[,4],1)) ends <- c(ends, nrow(y))
      # TODO: Use something faster than 'rbind'.
      domains <- rbind(
         domains,
         data.frame(block=block, start=y[starts,2], end=y[ends,3])
      )
   }

   return (domains)

}
