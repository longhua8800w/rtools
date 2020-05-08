library(tidyverse)
library(lubridate)
library(tidyselect)

## 改名 去掉项目全局的前缀 读入 ------

# wd=getwd()
# setwd("D:/Program Files/Rhome/biz_proj/credit_ykd001_181101_190831_01/ds/zcaf/0_raw")
# file.rename(path_ls, path_ls%>%str_remove('zcaf_'))
#setwd(wd)

### 文件读取   ---------


credit_qh360_190701_191031_01 <- read_csv("E:/Rhome/Rhome/biz_proj/credit_qh360_190701_191031_01/target/credit_qh360_190701_191031_01.csv", 
                                           col_types = cols(back_trace_dt = col_date(format = "%Y-%m-%d"), 
                                                            credit_dt = col_date(format = "%Y-%m-%d")))%>%
  mutate_at("back_trace_dt",as.character)

data_dir="0_raw/"
path_ls <- dir(data_dir)%>%str_subset('\\.csv$')
rdcsv <- safely(~ read_csv(paste0(data_dir,.x)))
lsf0 <- map(path_ls, rdcsv) %>% transpose()

infile_res <- lsf0 %>% map(~ set_names(.x, path_ls %>% str_remove(".csv")))
# infile_res$error%>%compact()

ds_ls0 <- infile_res$result 


# "这时间戳 不大对劲先忽略这俩"
# ds_ls0$zfb$数据获取时间戳[1]%>%as_datetime(./1000 )
# 
# 1579421831.414%>%as_datetime( )
#   

# ds_ls0$zfb$头像url[1]
# ds_ls0$wx$头像url[1]








dtpd = 
  ds_ls0$dtpd%>%mutate_at("back_trace_dt",~.x%>%as_date%>%as.character)%>%
  left_join(credit_qh360_190701_191031_01%>%select(no=sp_seqno,partition,name_md5, id_no_md5, mobile_md5, back_trace_dt,y),
            .)%>%select(-cell_h3)

xfsr =
  ds_ls0$xfsr%>%mutate_at("back_trace_dt",~.x%>%str_remove(' .+')%>%as_date%>%as.character)%>%
  left_join(credit_qh360_190701_191031_01%>%select(no=sp_seqno,partition,name_md5, id_no_md5, mobile_md5, back_trace_dt,y),
            .)%>%select(-cell_h3)

id=c("no",'name_md5','id_no_md5','mobile_md5','back_trace_dt')

meta <- 
  tibble(
    tbnm=c(rep("dtpd",ncol(dtpd)), rep("xfsr",ncol(xfsr)) ),
    vnm=c(dtpd%>%names,xfsr%>%names) ,
    cnm=vnm,
  )%>%distinct%>%filter(!vnm%in%c(id,"y","partition"))

data <- 
  list(dtpd=dtpd,xfsr=xfsr)%>%reduce(full_join)



write_excel_csv(meta,"1_import/meta.csv")
write_excel_csv(data,"1_import/data.csv")









