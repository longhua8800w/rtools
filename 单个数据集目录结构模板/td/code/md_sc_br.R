#### 加载工具包 -----
library(tidyverse)
library(tidyselect)
#library(skimr)
library(rlang)
library(caret)
library(imputeTS)
library(Information)
library(scorecard)
library(haven)


###############
data0 <- read_csv("3_num/data.csv",guess_max = 18290)%>%mutate_at(c("no","back_trace_dt"),as.character)

key <- "no"
biz_key <- c("name_md5", "id_no_md5", "mobile_md5", "back_trace_dt")
cp=c("train","test")

data1 <- 
  data0%>%{tb=.
  tb%>%select(-all_of(tb%>%select_if(is.logical)%>%names))
  }


key_data_ls0 <- 
 data1 %>%
  filter(partition%in%cp)%>%
  split(.$partition) %>%
  map(~ .x %>% select(-partition))


# trian0 以前被去掉的是被认为没有意义的数据





## 按抽样比例恢复 样本
key_data_ls=list()
key_data_ls$train <- key_data_ls0$train
### 把验证集的G额外复制一遍
key_data_ls$test <-  key_data_ls0$test%>%{tb=.; tb0=tb%>%filter(y==0); bind_rows(tb,tb0)}






write_excel_csv(key_data_ls$train,"4_understand/model/scorecard/sc/train.csv")
write_excel_csv(key_data_ls$test,"4_understand/model/scorecard/sc/test.csv")

write_sav(key_data_ls$train,"4_understand/model/scorecard/sc/train.sav")
write_sav(key_data_ls$test,"4_understand/model/scorecard/sc/test.sav")



data_ls <- key_data_ls %>% map(~ .x %>% select(-all_of(biz_key)))



train0 <- data_ls$train
test0 <- data_ls$test

Y <- train0%>%select(y)
X0 <- train0%>%select(-y)

#########################

source('D:/Program Files/Rhome/tools/ks.R')

tb_ks <- function(ds){ds%>%
    {
      tb  <-  . ;
      vars <- tb %>%select(-y);
      y=tb%>%pull(y);
      tibble(vnm = names(vars), ks = vars %>% map_dbl(~ ks(.x, y)))
    }}

tb_psi <- function(ls){
  #'@ list of two named tb
  psi=safely(perf_psi)
  psi_ls <- 
    map(
      ls$train%>%select(-y)%>%names,
      function(colnm){psi(score = ls%>%map(~.x%>%select(colnm)), label=ls%>%map(~.x%>%pull(y)),fill = T,show_plot =F)}
    )
  
  psi_ls1=psi_ls%>%transpose
  psi_ls1$result%>%map(~.$psi)%>%bind_rows%>%as_tibble%>%set_names(c("vnm","compare","psi"))
}

tb_fin_iv <- function(tb){
  iv_ls <- tb%>%create_infotables( y="y")
  list(
    var=iv_ls$Summary%>%as_tibble%>%set_names(c("vnm","iv")),
    bin=iv_ls$Tables%>%{ls=.;
    ls%>%map(~.x%>%set_names(c("bin","n","percent","woe","iv")))%>%
      map2(.y=names(ls),~.x%>%mutate(vnm=.y)%>%select(vnm,everything()) )%>%
      bind_rows%>%as_tibble
    }
  )
  
}



cmp_iv <- 
  function(data_ls){
    bins = woebin(data_ls$train, y="y")
    
    test_bin <- woebin_ply(data_ls$test %>% select(y, bins %>% names()) %>% select(-no), bins, to = "bin") %>%
      rename_all(~ .x %>% str_remove("_bin$"))
    
    
    format_ply_test <- function(vnm = "cccashl_v_1") {
      v <- parse_expr(vnm)
      
      test_iv <- test_bin %>%
        select(y, !!v) %>%
        group_by(!!v) %>%
        summarise_all(list(n = length, b = sum, g = ~ sum(1 - .x))) %>%
        mutate(
          p1 = b / sum(b), p0 = g / sum(g), p = n / sum(n), woe = log(p1 / p0),
          iv = (p1 - p0) * log(p1 / p0), total_iv=sum(iv) 
        ) %>%
        select(bin = !!v, count=n, count_distr = p, good=g,bad=b,badprob=p1,woe, bin_iv=iv,total_iv)%>%
        mutate(breaks=bin%>%str_remove('\\)')%>%str_extract('[0-9a-zA-Z]+$'),
               is_special_values = (bin=="missing") )%>%
        select(bin,count,count_distr,good,bad,badprob,woe,bin_iv,total_iv,breaks,is_special_values)
      
      return(test_iv)
    }
    
    '[-Inf,2)'%>%str_remove('\\)')%>%str_extract('[0-9a-z]+$')
    
    test_ply <- names(test_bin %>% select(-y)) %>% {
      v <- .
      v %>%
        map(format_ply_test) %>%
        set_names(v) %>%
        map2(v, ~ .x %>%
               mutate(variable = .y) %>%
               select(variable, everything()))
    }%>%bind_rows%>%rename_at(vars(-variable,-bin),~paste0("test_",.x))
    
    bins %>%
      bind_rows() %>%
      filter(variable != "no") %>%
      left_join(test_ply) %>%
      select(-breaks, -is_special_values, -test_breaks, -test_is_special_values)%>%as_tibble
    
  }


cp_iv_det <- cmp_iv(data_ls)

cp_iv <- cp_iv_det %>%
  group_by(variable) %>%
  summarise_at(vars(contains("total")), max) %>%
  rename_all(~ .x %>% str_remove("total_")) %>%
  rename(vnm = variable,train_iv=iv)




iv_ls <- data_ls[cp]%>%map(~.x%>%select(-key))%>%map(tb_fin_iv)

iv <- iv_ls%>%transpose%>%{.$var[cp]}%>%{ls=.; ls%>%map2(names(ls),~.x%>%set_names(c("vnm",str_glue('iv_{.y}'))))}%>%reduce(full_join)

ks_ls <- data_ls%>%map(~.x%>%select(-key))%>%map(tb_ks)


ks <- ks_ls%>%{.[cp]}%>%{ls=.; ls%>%map2(names(ls),~.x%>%set_names(c("vnm",str_glue('ks_{.y}'))))}%>%reduce(full_join)
psi_train_test <- tb_psi(data_ls0%>%map(~.x%>%select(-key))%>%{.[cp]})
psi <- psi_train_test%>%select(vnm,psi_train_test=psi)


stb <- list(iv,ks,psi)%>%reduce(inner_join)%>%{tb=.; tb[tb%>%complete.cases,]}
###########################

cvkpnm <- 
  X0%>%summarise_all(~sd(.,na.rm=T)/mean(.,na.rm = T))%>%
  gather(key = "vnm", value = "cv")%>%mutate(p=ecdf(cv)(cv))%>%filter(cv>0.2)%>%pull(vnm)

X1 <- X0%>%select(one_of(cvkpnm))

nzv <- nearZeroVar(X1)
X2 <- X1[-nzv]
X3 <- X2 %>%na_mean
corr <- cor(X3)


highlyCor <- findCorrelation(corr, cutoff = .75)

X21 <- X2[,-highlyCor]
X31 <- X3[,-highlyCor]

comboInfo <- findLinearCombos(X31)

train1 <- train0%>%select(y,X21%>%names)
test1 <- test0%>%select(names(train1))

# 根据区分能力和区分能力稳定性筛选 ----

keep_nm =intersect(names(train1),stb%>%filter(ks_train>0.005 , ks_test>0.005,abs((ks_train-ks_test)/ks_train)<3, psi_train_test<0.1)%>%pull(vnm))

train2=train1%>%select(y,one_of(keep_nm))
test2 = test1%>%select(names(train2))
# train test 趋势筛选 -----

sptb <- 
  list(train2=train2,test2=test2)%>%map(
    ~.x%>%{tb=.;
    l=tb%>%select(-y)%>%na_mean%>%map(~cor(.x,tb$y,method = "spearman"));
    tibble(vnm=names(l),rho=unlist(l))
    }
    
  )%>%map2(.,names(.),~.x%>%set_names(c("vnm",paste0(.y,"_sprm"))))%>%
  reduce(inner_join)

ktb <- 
  list(train2=train2,test2=test2)%>%map(~.x%>%{tb=.;
  l=tb%>%select(-y)%>%na_mean%>%map(~lm(y~x,data = tibble(x=.x, y=tb$y))%>%coef%>%{.[2]});
  tibble(vnm=names(l),rho=unlist(l))
  })%>%map2(.,names(.),~.x%>%set_names(c("vnm",paste0(.y,"_k"))))%>%
  reduce(inner_join)
#same trend
# 去掉 判断 线性趋势不一致
dft_vnm <- 
  sptb%>%left_join(ktb)%>%filter(train2_sprm*test2_sprm< 0 |train2_k*test2_k< 0|abs(train2_sprm) <0.01|abs(train2_sprm)<0.01)%>%pull(vnm)


kpnm2 <- keep_nm%>%vars_select(-one_of(dft_vnm))

### 计算粗分 iv ----

train3=train2%>%select(y,kpnm2)
test3 = test2%>%select(names(train3))



bins <- woebin(train3, y="y")
#The Infinite or NaN values are replaced with -999 in the following columns:
#r_0m03m06_id_max_inteday, r_0d15m06_id_nbank_week_orgnum

dt_list <- list(train=train3,test=test3)
label_list <- map(dt_list, ~.x$y)

dt_woe_list <- dt_list%>%map(~.x%>%woebin_ply(bins) %>% rename_all(~.x%>%str_remove("_woe"))%>%as_tibble)

# 1: In rep_blank_na(dt) :
#   The Infinite or NaN values are replaced with -999 in the following columns:
#   r_0m03m06_id_max_inteday, r_0d15m06_id_nbank_week_orgnum
# 2: In rep_blank_na(dt) :
#   The Infinite or NaN values are replaced with -999 in the following columns:
#   r_0m03m06_id_max_inteday, r_0d15m06_id_nbank_week_orgnum


#### 掐尖法 筛选变量 ---------
nmsl <- list()
m = glm( y ~ ., family = binomial(), data = dt_woe_list$train)
temp=dt_woe_list$train
for(i in 1:5){
  nmsl[[i]] <- varImp( m)%>%as.data.frame%>%rownames_to_column%>%arrange(desc(Overall))%>%slice(1:30)%>%pull(rowname)
  temp=temp%>%select(-one_of(nmsl[[i]][(1:4)]))
  m = glm( y ~ ., family = binomial(), data = temp)
}

kpnm1 <- nmsl%>%unlist%>%unique
####
m1=glm( y ~ ., family = binomial(), data = dt_woe_list$train%>%select(y,one_of(kpnm1)))

m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)

kpnm3=m2%>%summary%>%coefficients%>%as.data.frame%>%
  rownames_to_column%>%filter(rowname!="(Intercept)",Estimate>0)%>%pull(rowname)



dt_list1 <- dt_list %>%map(~.x%>%select(y,all_of(kpnm3))) 
label_list1 <- map(dt_list1, ~.x$y)
dt_woe_list1 <- dt_woe_list%>%map(~.x%>%select(y,kpnm3)) 

m3 = glm( y ~ ., family = binomial(), data = dt_woe_list1$train)


m_step1 = step(m3, direction="both")
m4 = eval(m_step1$call)
vif(m4, merge_coef = TRUE)

m_inputnm=
m4%>%summary%>%coefficients%>%as.data.frame%>%
  rownames_to_column%>%pull(rowname)

m_inputnm%>%write_lines("trans_file/br_inputnm.txt")

pred_list1 = map(dt_woe_list1, function(x) predict(m4, x, type='response'))

perf = perf_eva(pred = pred_list1, label =label_list1 )

card = scorecard(bins, m2,points0 = 400,pdo = 45)

## credit score
score_list = map(dt_list, ~scorecard_ply(.x, card))
## psi
perf_psi(score = score_list, label = label_list)
#### 不做衍生 先跑 stepwise------



train <- data%>%filter(partition=="train") %>%select(id)%>%left_join(train0)
test <- data%>%filter(partition=="test") %>%select(id)%>%left_join(test0)


write_sav(train,"trans_file/train.sav")
write_sav(test,"trans_file/test.sav")

write_lines(input_nm,"trans_file/input_nm.txt")


meta <- read_csv("1_import/meta.csv")


m_inputnm %>%
  str_subset("^[a-z0-9_]+$") %>%
  sort() %>%
  enframe(value = "vnm") %>%
  left_join(meta) %>%
  select(-tbnm) %>%
  write_excel_csv("4_understand/model/scorecard/sc/inputnm.csv")

