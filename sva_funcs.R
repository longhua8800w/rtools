
tb_fin_ks <- function(ds){ds%>%
    {
      tb  <-  . ;
      vars <- tb %>%select(-y);
      y=tb%>%pull(y);
      tibble(vnm = names(vars), ks = vars %>% map_dbl(~ ks(.x, y)))
    }}


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




# 入参格式
# data_ls
# $test
# # A tibble: 3,659 x 5
# no     y fx_score ty_score xj_score
# <dbl> <dbl>    <dbl>    <dbl>    <dbl>
#   1  7847     0      577   0.0300   0.0717
# 2  7848     0      608   0.159    0.341 
# 3  7849     0      538   0.286    0.392 
# 4  7850     0      592   0.133    0.189 
# 5  7851     0      538   0.111    0.166 
# 6  7852     0      574   0.0157   0.0671
# 7  7853     0      515   0.168    0.312 
# 8  7854     0      648   0.0661   0.135 
# 9  7855     0      539   0.138    0.251 
# 10  7856     0      583   0.0843   0.133 
# # ... with 3,649 more rows
# 
# $train
# # A tibble: 7,846 x 5
# no     y fx_score ty_score xj_score
# <dbl> <dbl>    <dbl>    <dbl>    <dbl>
#   1     1     0      595   0.0540   0.148 
# 2     2     0      581   0.0699   0.104 
# 3     3     0      514   0.0899   0.176 
# 4     4     0      521   0.0319   0.0948
# 5     5     0      620   0.0625   0.144 
# 6     6     0      535   0.0482   0.167 
# 7     7     0      572   0.0710   0.116 
# 8     8     0      581   0.0479   0.0715
# 9     9     0      579   0.0571   0.213 
# 10    10     0      564   0.0277   0.0673
# # ... with 7,836 more rows




bin_sva <- 
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
    

    test_ply <- names(test_bin %>% select(-y)) %>%
      {
        v <- .
        v %>%
          map(format_ply_test) %>%
          set_names(v) %>%
          map2(v, ~ .x %>%
                 mutate(
                   variable = .y,
                   cpg = cumsum(good) / sum(good),
                   cpb = cumsum(bad) / sum(bad),
                   gbd = abs(cpg - cpb),
                   bin_ks = max(gbd)
                 ) %>%
                 select(variable, everything()))
      } %>%
      bind_rows() %>%
      rename_at(vars(-variable, -bin), ~ paste0("test_", .x))
    
    dtl <- bins %>%
      map(~ .x %>%
            mutate(
              cpg = cumsum(good) / sum(good),
              cpb = cumsum(bad) / sum(bad),
              gbd = abs(cpg - cpb),
              bin_ks = max(gbd)
            ) %>%
            select(variable, everything())) %>%
      bind_rows() %>%
      filter(variable != "no") %>%
      left_join(test_ply) %>%
      select(-breaks, -is_special_values, -test_breaks, -test_is_special_values)%>%
      group_by(variable) %>%
      mutate(psi = (count_distr - test_count_distr) * log(count_distr / test_count_distr),
             total_psi=sum(psi)) %>%
      as_tibble()
    
    var_stat <- 
      dtl%>%
      group_by(variable) %>%
      summarise_at(vars(contains("total"),contains("bin_ks")), max) %>%
      rename_all(~ .x %>% str_remove("total_")) %>%
      rename(vnm = variable,train_iv=iv)
    
    
    res <- 
      list(
        var_stat=var_stat,
        dtl=dtl
      )
    
    return(res)
  }
















sva <- 
  function(data_ls){
    
    bin_res <- bin_sva(data_ls)
    
    
    
    fin_iv_tb <- data_ls %>%
      map(~ .x %>% select(-all_of(key))) %>%
      map(tb_fin_iv) %>%
      transpose() %>%
      {
        .$var[cp]
      } %>%
      {
        ls <- .
        ls %>% map2(names(ls), ~ .x %>% set_names(c("vnm", str_glue("fin_iv_{.y}"))))
      } %>%
      reduce(full_join)
    
    
    
    fin_ks_tb <- data_ls %>%
      map(~ .x %>% select(-key)) %>%
      map(tb_fin_ks) %>%
      {
        .[cp]
      } %>%
      {
        ls <- .
        ls %>% map2(names(ls), ~ .x %>% set_names(c("vnm", str_glue("fin_ks_{.y}"))))
      } %>%
      reduce(full_join)
    
    
    
    
    stat <- 
      list(bin_res$var_stat,
           fin_iv_tb,
           fin_ks_tb )%>%reduce(full_join)
    
    
    res=list(var_stat=stat, 
             dtl=bin_res$dtl)
    
    return(res)
    
  }



