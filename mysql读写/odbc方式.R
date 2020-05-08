library(RODBC)
library(tidyverse)

channel<- odbcConnect('mysql',DBMSencoding="utf-8")
sqlTables(channel)

sqlQuery(channel, "set character set utf8")

library(readr)
ykd001_dxm_blacklist <- read_csv("ykd001_dxm_blacklist.csv",  col_types = cols(日期 = col_date(format = "%Y-%m-%d")))



ykd001_dxm_blacklist_ <- ykd001_dxm_blacklist%>%set_names(paste0("V",1:ncol(.)))%>%mutate_all(as.character)

sqlDrop(channel,"ykd001_dxm_blacklist")
sqlSave(channel, ykd001_dxm_blacklist_,"ykd001_dxm_blacklist_1",nastring = "")


sqlSave(conn, df, "表名")




data<-sqlFetch(channel,"ykd001_dxm_fraud")


con<- odbcConnect("per",uid="root",pwd="xxxxxx",DBMSencoding="utf-8")









