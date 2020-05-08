woe_replace <- 
  function(ds,bin_woe){
    # 输入数据集和 var_bin_woe 
    # 函数将 数据集中var_bin_woe中有映射的列的值替换成对应的woe值
    library(tidyverse)
    library(lubridate)
    library(intrval)
    isNumInIntrval <-
      function(x,intv_str){
        requireNamespace("intrval")
        intv=intv_str%>%str_replace_all("inf","Inf")
        intv_symbol=intv%>%str_extract_all("(\\()|(\\))|(\\[)|(\\])")%>%unlist%>%glue::glue_collapse()
        range_str=intv%>%str_remove_all("(\\()|(\\))|(\\[)|(\\])")%>%unlist
        eval(parse(text = str_glue("{x}%{intv_symbol}%c({range_str})") ))
      }
    
    
    ds_vnm <- names(ds)
    bined_vnm <- bin_woe$features%>%unique
    nm2rep <- intersect(bined_vnm,ds_vnm)
    
    var_bin_woe <- bin_woe%>%filter(features%in%nm2rep)
    
    #var_bin_woe$variable%>%unique == nm2rep
    #nm2rep 和 var_bin_woe$variable 两者顺序一致
    ## 对应每个列，构造cut 的break 入参
    nest_df <- var_bin_woe%>%group_by(features)%>%nest%>%rename(varbin2woe=data)
    varbin2woe_lst=nest_df$varbin2woe
    #每列做不同的分组
    df2aply <-ds%>%select(nm2rep)
    
    val2accdIntvBinWoe <-
      function(x,d){
        for (i in 1:nrow(d)) {
          if(isNumInIntrval(x,d$intervals[i])) return(d$woe[i])
        }
      }
    
    l=list()
    for(i in 1:length(nm2rep)){
      v=df2aply[[i]]
      d=varbin2woe_lst[[i]]
      l[[nm2rep[i]]] <- map_dbl(v,~val2accdIntvBinWoe(.x,d))
      
    }
    
    df_woe <- l%>%tbl_df()
    
    woe_aplyed <- ds%>%select(-nm2rep)%>%bind_cols(df_woe) %>% select(seqnum,observation_time,name,idcard,tel,everything())
  }
