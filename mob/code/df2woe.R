#library(mob)
#library(tidyverse)


# df0 <- readRDS("D:/Program Files/Rhome/tools/mob/data/accepts.rds")%>%as_tibble%>%select(-purpose,-app_id)
# 
# 
# df0%>%pull(weight)%>%table(useNA = "ifany")
# 
# df0$weight <- NULL
# df=df0
source('D:/Program Files/Rhome/tools/mob/code/manual_bin.R')
source('D:/Program Files/Rhome/tools/mob/code/bad_bin.R')
source('D:/Program Files/Rhome/tools/mob/code/qtl_bin.R')
source('D:/Program Files/Rhome/tools/mob/code/iso_bin.R')
source('D:/Program Files/Rhome/tools/mob/code/gbm_bin.R')
source('D:/Program Files/Rhome/tools/mob/code/cal_woe.R')

## 生成每个变量的 分组,输入都是数值型的
bin_df <-
  function(df, y = "bad",f="bad") {
    #'  @param  df  input dataframe
    #'  @param  y  target var name
    #'  @param  f  bin function option
    #'  
    
    require(magrittr)
    require(purrr)
    require(dplyr)
    
    vnm <- names(df)%>%{.[which(.!=y)]}
    if(!f%in%c("bad","qtl","iso","gbm")) stop("f only support 'bad','qtl','iso','gbm'")
    
    if (f == "bad") {
  fbin <- bad_bin
} else if (f == "qtl") {
  fbin <- qtl_bin
} else if (f == "iso") {
  fbin <- iso_bin
} else if (f == "gbm") {
  fbin <- gbm_bin
}

    bins <-vnm%>%{nms=.; map(nms, ~ fbin(df, y, .x))%>%set_names(nms) } 
    
    for(nm in vnm){
      bad_bin(df, y, nm)
    }
    
    nm="scoredpurpower"
    
    
    data=df; y=y; x=nm
    
    
    
    return(bins)
  }


woe_df <-
  function(df, y = "bad",bins) {
    #'  @param  df  input dataframe
    #'  @param  y  target var name
    #'  @param  bins  bin list
    #'  
    
    require(magrittr)
    require(purrr)
    require(dplyr)
    
    vnm <- names(df)%>%{.[which(.!=y)]}

    woe_1col <-
      function(df, colnm) {
        #'  @param  df   input dataframe
        #'  @param  vnm   var name to replace with woe
        v_woe <- cal_woe(data=df %>% select(colnm),xname=colnm, spec=bins[[colnm]]$df)
        
        list(
          woe = v_woe$df %>% select(paste0(colnm, "_woe")),
          psi = v_woe$psi
        )
      }
    
    
    woe <- vnm %>%
      map(~ woe_1col(df, .x)) %>%
      transpose()
    
    woe_df=df[y]%>%bind_cols(woe$woe%>% bind_cols)
    return(woe_df)

  }


#bins=bin_df(df,f="qtl")
#woe_df(df, y = "bad",bins)
