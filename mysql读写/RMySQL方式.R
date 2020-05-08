library(RMySQL)
library(tidyverse)
library(dplyr)

con <- DBI::dbConnect(RMySQL::MySQL(), dbname = "outer_data_sample", username = "root",
          password = NULL, host = '10.192.86.59')
dbSendQuery(con, 'set names gbk')
dbListTables(con)

 ykd001_dxm_blacklist <- read_csv("ykd001_dxm_blacklist.csv", 
                                   col_types = cols(日期 = col_date(format = "%Y-%m-%d")))%>%set_names(str_glue('VAR{1:ncol(.)}'))
 
 
 
 
 dbWriteTable(con,"ykd001_dxm_blacklist",ykd001_dxm_blacklist)
 
 
 
 

 dd <- tbl(con, "ykd001_dxm_blacklist")%>%collect%>%as_tibble(.name_repair = "minimal")
 
 

 dbDisconnect(con)
 
 
 
 ### 连接本地数据库 -----------------
 
 con1 <- DBI::dbConnect(RMySQL::MySQL(), dbname = "data", username = "root",
                       password = '123456', host = 'localhost')
 ##dbDisconnect(con1)
 dbSendQuery(con1, 'set names gbk')
 
 dbListTables(con1)

 dbGetQuery(con1, 'drop table if exists ykd001_dxm_blacklist')
 
 
 dbWriteTable(con1,"ykd001_dxm_blacklist",ykd001_dxm_blacklist)
 
 
 
 
 dd <- tbl(con1, "ykd001_dxm_blacklist")%>%collect%>%as_tibble(.name_repair = "minimal")
 
 
 
 
 
 
 
 
 
 
  