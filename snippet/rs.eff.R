library(DescTools)

rs.eff <- function(data,xval="slope",yval){
  rs.int <- AUC(data[,xval],data[,yval],from=1, to=max(data[,xval]))/AUC(data[,xval],data[,yval],from=min(data[,xval]), to=max(data[,xval]))
  rs <- (rs.int - 0.5) * 200
  return(c(rs.int,rs))
}
