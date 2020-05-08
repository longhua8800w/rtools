library(tidyverse)


data <- read_csv("1_import/data.csv",guess_max = 18290)%>%mutate_at(c("no","back_trace_dt"),as.character)
meta <- read_csv("1_import/meta.csv")


data%>%select_if(is.character)%>%skimr::skim()



id <- c("no","name_md5","id_no_md5","mobile_md5","back_trace_dt")

exclude= c(id,"partition")

library(tidyverse)
library(lubridate)

## 读入 ------
vec=compose(unlist,~ str_split(.x,",") )
collapse_value=function(v){ v1=map(v,vec)%>%unlist;  paste0(v1%>%{v1[!is.na(v1)]}%>%unique%>%sort,collapse = ",")}

union_value=function(str1,str2){ v1=vec(str1);v2=vec(str2);v3=c(v1,v2)%>%unique;  paste0(v3%>%{v3[!is.na(v3)]}%>%unique%>%sort,collapse = ",")}

intersect_value=function(str1,str2){ v1=vec(str1);v2=vec(str2);v3=intersect(v1,v2)%>%unique;  paste0(v3%>%{v3[!is.na(v3)]}%>%unique%>%sort,collapse = ",")}


strv_contains <-function(str1,str2){map2_lgl(str1,str2,function(str1,str2){all(vec(str2)%in%vec(str1))}) }     

strv_union <- function(str){
  str%>%map(vec)%>%reduce(.f=~c(.x,.y))%>%unique%>%sort%>%paste0(collapse = ",")
}













lvlnm <- 
  data%>%select_if(is.character)%>%
  select(-one_of(exclude))%>%map(~.x%>%table(useNA = "ifany"))%>%
  map_int(length)

## 字符型 处理
data_chr <- data%>%select_if(is.character)%>%select(-one_of(exclude))

## 含汉字的
data_chr_zh <- data_chr%>%select_if(~.x%>%str_detect('[\u4e00-\u9fa5]')%>%any(.,na.rm = T))
data_chr_nzh <- data_chr%>%select_if(~.x%>%str_detect('[\u4e00-\u9fa5]')%>%any(.,na.rm = T)%>%{!.})

setdiff(data%>%select_if(is.character)%>%names,exclude)

colnm_emu <- 
  data_chr_nzh %>% {
  d <- .
  d %>%
    summarise_at(
      setdiff(d %>% select_if(is.character) %>% names(), exclude),
      ~ .x %>%
        unique() %>%
        sort() %>%
        paste0(collapse = ",")
    ) %>%
    gather(key = vnm, value = emu_label) %>%
    mutate_at("emu_label", str_squish) %>%
    group_by(emu_label) %>%
    summarise(nmv = vnm %>% paste0(collapse = ","))
  }%>%mutate(
    emu_nm=
      case_when(
        "a,b,c,d,e,f,g,h,i,j,k,l"%>%strv_contains(emu_label)~ "amtlvl") 
    )%>%
  group_by(emu_nm)%>%
  summarise(nmv=nmv%>%strv_union,
            domain=emu_label%>%strv_union)%>%
  select(emu_nm,domain,nmv)


data1 <- 
data %>% mutate_at(vec(colnm_emu$nmv), ~ .x %>%
  recode(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9, j = 10, k = 11, l = 12) %>%
  as.numeric())%>%{tb=.
  numnm=tb%>%select_if(is.character%>%negate)%>%names
  tb%>%select(id,partition,numnm)}


data1%>%map_chr(typeof)%>%table


derv_code <- read_csv("D:/Program Files/Rhome/br_deriv/order1_tb.csv")%>%
  select(code)%>%separate(code,into=c("vnm","expr_cd") ,sep=" = ")

lc=derv_code$expr_cd%>%map(rlang::parse_expr)%>%set_names(derv_code$vnm)


data2 <- data1%>%mutate(!!!lc)



write_excel_csv(data2,"3_num/data.csv")




