
#setwd("E:/Rhome/Rhome/tools")
#text =read_file("test_data/cd_woe_0414.sas")

grsasbincd2tb <- function(text){
  require(tidyverse)
  sas_woe_bin <-
    text %>%
    str_split("length") %>%
    unlist() %>%
    map(~ .x %>% str_split("end")) %>%
    unlist() %>%
    str_subset("=") %>%
    {
      l <- .
      r1 <- l %>%
        map(~ .x %>%
              str_match_all("\nWOE_([A-Z_0-9]+) = ([0-9-.]+)") %>%
              .[[1]] %>%
              .[, 2:3]) %>%
        transpose
      r2 <- l %>%
        map(~ .x %>%
              str_match_all('\nLBL_([A-Z_0-9]+) = "(.+)"') %>%
              .[[1]] %>%
              .[, 2:3]) %>%
        transpose
      tibble(vnm = r1[[1]] %>% unlist(), woe = r1[[2]] %>% unlist(), bin = r2[[2]] %>% unlist())
    } %>%
    mutate_at("woe", as.numeric) %>%
    select(vnm, bin, woe) #%>%select(-vnm)
  
  
  return(sas_woe_bin)
}

  
#btb=grsasbincd2tb(text)
