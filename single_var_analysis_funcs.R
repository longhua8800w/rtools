
cmp_iv <- 
  function(data_ls){
    bins = woebin(data_ls$train, y="y")
    
    test_bin <- woebin_ply(data_ls$test %>% select(y, bins %>% names()), bins, to = "bin") %>%
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
      left_join(test_ply) %>%
      select(-breaks, -is_special_values, -test_breaks, -test_is_special_values)%>%as_tibble
    
  }




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


