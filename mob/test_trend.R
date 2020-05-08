source('D:/Program Files/Rhome/tools/mob/code/df2woe.R')
library(tidyverse)

train2 <- readRDS("D:/Program Files/Rhome/tools/mob/data/train2.rds")
test2 <- readRDS("D:/Program Files/Rhome/tools/mob/data/test2.rds")

df=train2
y="y"
#qtl_bins <- bin_df(df,y="y",f="bad")
#qtl_bins <- bin_df(df,y="y",f="qtl")

train2_iso_bins <- bin_df(train2,y="y",f="iso")
train2_gbm_bins <- bin_df(train2,y="y",f="gbm")

test2_iso_bins <- bin_df(test2,y="y",f="iso")
test2_gbm_bins <- bin_df(test2,y="y",f="gbm")

save(train2_iso_bins,train2_gbm_bins,file = "example/train2_bins.RData")


trend <-function(bins){
  bins %>%
    map(~ .x$df) %>%
    map(~ .x %>%
          filter(rule != "is.na($X)") %>%
          pull(bad_rate) %>%
          {
            v <- .;
            diff <-  v[length(v)]-v[1]
          })%>%{ls=.; tibble(vnm=names(ls),trend=unlist(ls)%>%sign)} 
} 




trend_tb <- 
list(train2_iso=train2_iso_bins,
     train2_gbm=train2_gbm_bins,
     test2_iso=test2_iso_bins,
     test2_gbm=test2_gbm_bins)%>%map(trend)%>%{
       ls=.;
       map2(ls,names(ls),~.x%>%rename(!!.y:=trend))
     }%>%reduce(left_join)


kpnmt <- 
trend_tb%>%
  filter(
    train2_iso== train2_gbm,
    train2_gbm==test2_iso,
    test2_iso== test2_gbm
  )%>%pull(vnm)









train2_iso_woe <- woe_df(df = train2,y = "y",bins =train2_iso_bins )%>%rename_all(~str_remove(.x,"_woe"))%>%select(y,kpnmt)
test2_iso_woe <- woe_df(df = test2,y = "y",bins =train2_iso_bins )%>%rename_all(~str_remove(.x,"_woe"))%>%select(y,kpnmt)


train2_gbm_woe <- woe_df(df = train2,y = "y",bins =train2_gbm_bins )%>%rename_all(~str_remove(.x,"_woe"))%>%select(y,kpnmt)
test2_gbm_woe <- woe_df(df = test2,y = "y",bins =train2_gbm_bins )%>%rename_all(~str_remove(.x,"_woe"))%>%select(y,kpnmt)



save(train2_iso_woe,test2_iso_woe,train2_gbm_woe,test2_gbm_woe,file = "example/woe.RData")




skimr::skim(train2_iso_woe)
