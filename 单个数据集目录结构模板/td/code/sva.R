
#### 加载工具包 -----
library(tidyverse)
library(tidyselect)
#library(skimr)
library(rlang)
library(caret)
#library(imputeTS)
library(Information)
library(scorecard)
library(haven)
## 恢复 上次场景环境对象 -------
data_dir="1_import/"
path_ls <- dir(data_dir)%>%str_subset(".+\\.csv")
rdcsv <- safely(~ read_csv(paste0(data_dir,.x), , n_max = 20714))
lsf0 <- map(path_ls, rdcsv) %>% transpose()

infile_res <- lsf0 %>% map(~ set_names(.x, path_ls %>% str_remove(".csv")))
# infile_res$error%>%compact()

ds_ls0 <- infile_res$result 

### 全局常量  ------
key <- "no"
biz_key <- c("name_md5", "id_no_md5", "mobile_md5", "back_trace_dt")
cp=c("train","test")


data <- ds_ls0$data
meta <- ds_ls0$meta


data_ls <- data %>%
  select(-all_of(biz_key)) %>%
  filter(partition%in%cp)%>%
  split(.$partition) %>%
  map(~ .x %>% select(-partition))

##### ---------
source('E:/Rhome/Rhome/tools/ks.R')

source('E:/Rhome/Rhome/tools/sva_funcs.R')

lr=sva(data_ls)



sva_tot <- meta%>%select(tbnm,cnm,vnm)%>%inner_join(lr$var_stat)%>%arrange(desc(bin_ks))

write_excel_csv(sva_tot,"sva/sva_tot.csv")
write_excel_csv(lr$dtl,"sva/iv_det.csv")


data_ls$train%>%write_sav("trans_file/train.sav")

data_ls$test%>%write_sav("trans_file/test.sav")


aa <- read_sav("trans_file/train.sav")

aa$y%>%table
