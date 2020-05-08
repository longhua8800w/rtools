

ks_plot <- 
function(y,yp){
  
  dat=tibble(grp=y,KSD=yp)
  bad <-yp[y==1]
  good <- yp[y==0]
  

  # create ECDF of data
  cdf1 <- ecdf(bad) 
  cdf2 <- ecdf(good) 
  # find min and max statistics to draw line between points of greatest distance
  minMax <- seq(min(bad, good), max(bad, good), length.out=length(bad)) 
  x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
  y0 <- cdf1(x0) 
  y1 <- cdf2(x0) 
  
  ggplot(dat, aes(x = KSD, group = grp, color = grp))+
    stat_ecdf(size=1) +
    theme_bw(base_size = 28) +
   # theme(legend.position ="top") +
    xlab("Sample") +
    ylab("ECDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
    ggtitle("K-S Test: bad / good") +
    theme(legend.title=element_blank())
}



ks <- function(pred,depvar){
  require("ROCR")
  pred <- pred%>%replace_na(-999999)
  if(unique(pred)%>%length == 1) return(NA_real_)
  p <- prediction(as.numeric(pred),depvar)
  perf <- performance(p, "tpr", "fpr")
  ks <- (attr(perf, "y.values")[[1]] - (attr(perf, "x.values")[[1]]))%>%abs%>%max
  return(ks)
}


