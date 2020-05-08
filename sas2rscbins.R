
# sas_bins 的数据结构
# A tibble: 35 x 2
#                 bin                                      woe
#                <chr>                                    <dbl>
#  1 low <= I_CNT_PART_LOAN_IMBANK_180D < 0 + Missing     -0.305 
#  2 0 <= I_CNT_PART_LOAN_IMBANK_180D < 3.5                0.354 
#  3 3.5 <= I_CNT_PART_LOAN_IMBANK_180D < 6.5             -0.0767
#  4 6.5 <= I_CNT_PART_LOAN_IMBANK_180D < high            -0.586 
#  5 low <= I_FREQ_REC_LOAN_UNCONSUFIN_180D < 0 + Missing -0.305 
#  6 0 <= I_FREQ_REC_LOAN_UNCONSUFIN_180D < 6.5            0.209 
#  7 6.5 <= I_FREQ_REC_LOAN_UNCONSUFIN_180D < 10.5        -0.218 
#  8 10.5 <= I_FREQ_REC_LOAN_UNCONSUFIN_180D < high       -0.345 
#  9 low <= I_RATIO_YS_LOAN_IMBANK_790D < 0.5429           0.145 
# 10 0.5429 <= I_RATIO_YS_LOAN_IMBANK_790D < high         -0.245 
# ... with 25 more rows

# PATH="E:/Rhome/Rhome/tools/"
# sas_bins <- read_csv( paste0(PATH,"test_data/sas_bins.csv") )


sas2rscbins <- 
  function(sas_bins){
    
    require(tidyverse)
  sc_bins <- 
    sas_bins %>%
    mutate(variable = bin %>% str_match("<= ([a-zA-Z0-9_]+) <") %>% .[, 2]) %>%
    fill(variable)%>%
    select(variable, bin, woe)%>%
    separate_rows(bin,sep=" \\+ ")%>%
    mutate_all(tolower)%>%
    mutate( bin= bin%>% 
              str_match("([a-z0-9.]+) <= ([a-zA-Z0-9_]+) < ([a-z0-9.]+)")%>%{ mat=.
              str_glue("[{mat[,2]},{mat[,4]})")%>%str_replace("low","-Inf")%>%str_replace("high"," Inf")%>%str_replace("\\[NA,NA\\)","missing")
              },                                                                                 # cut 会在 Inf前加空格 不知道为啥
            woe=as.numeric(woe)
    )%>%split(.$variable)
  
  return(sc_bins)
}


#sas2rscbins(sas_bins)
