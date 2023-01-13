


#' @export
growthProjection <- function(cols,n,growth,projection) {
  r <- c(cols, seq_len(projection)+cols[length(cols)])
  v <- c(n,rep(mean(n), projection))
  g <- c(growth,growth[seq_len(projection)] + growth[length(growth)])
  return(list(cols=r,n=v,growth=g))
}

#' @export
growthMatrix <- function(cols, n, growth, projection=4, offset=0) {
  if (missing(growth)) growthFactor <- 1:length(cols)
  else growthFactor <- growth
  args <- growthProjection(cols, n, growthFactor, projection)
  scale <- c(length(args$cols):1)
  result <-as.data.frame(
    sapply(scale,
           function(x) {
             rev(c(rep(0,x-1), args$growth[seq_len(length(args$cols)-x+1)]))
           }) * args$n
  )[offset:length(args$cols), offset:length(args$cols)]
  names(result) <- as.character(args$cols[offset:length(args$cols)])
  return(result)
}

#' @export
growthTotals <- function(cols,n, values, projection=4, offset=0) {
  args <- growthProjection(cols, n, 1:length(cols), projection)
  result <- rbind(values,colSums(values))
  result <- cbind(
    data.frame(
      "cols"=c(as.character(args$cols[offset:length(args$cols)]), "total"),
      "n"=c(args$n[offset:length(args$n)],sum(args$n[offset:length(args$n)]))
    ),
    result
  )
  return(result)
}

