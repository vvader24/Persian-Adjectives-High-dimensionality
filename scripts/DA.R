library(fs)
library(tidyverse)
files <- dir_ls(here::here("data"))
sheets <- readxl::excel_sheets(files[2])
l <- map(sheets, ~readxl::read_xlsx(files[2], sheet = .x, n_max = 360))

d <- l[[7]]

rename_df <- function(d) {
  col_miss_log <- apply(d, 2, function(x) sum(is.na(x)) == nrow(d))
  col_miss <- seq_along(d)[col_miss_log]
  d_subset <- d[ ,-c(1, col_miss, col_miss + 1)]
  nms1 <- gsub("^(\\d?\\d).+", "g_\\1", names(d_subset)[seq(1, col_miss - 2)])
  nms2 <- gsub("^(\\d?\\d).+", "ng_\\1", names(d_subset)[seq(col_miss - 1, length(names(d_subset)))])
  names(d_subset) <- c(nms1, nms2)
  d_subset
}

add_sheetname <- function(d, sheetname) {
  d <- rename_df(d)
  prefix <- gsub("\\d", "", names(d))
  digits <- gsub("\\D", "", names(d))
  nms <- paste0(prefix, sheetname, "_", digits)
  names(d) <- nms
  d
}

add_sheetname(l[[7]], sheets[7])

l2 <- map2(l, sheets, add_sheetname)
names(l2[[7]])


#----- try congruence coefficient 

library(combinat)
permn(letters[1:17])

map(1:17, ~ c(paste0(letters[1:17]))) %>%
  cross() %>%
  keep(~ length(unique(.x)) == 17) %>%
  map(unlist)



permutations <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}



matrix(letters[permutations(17)],ncol=17)

library("qgraph")
library(igraph)
adult_zeroorder <- factor.congruence(Fr_vx4 , Mr_vx4)
qgraph(adult_zeroorder, layout="spring",
       # groups = list(Depression = 1:16,
       #               "OCD" = 17:26),
       color = c("lightblue",
                 "lightsalmon"),
       edge.labels=TRUE)

# create the network object
network <- graph.adjacency(factor.congruence(Fr_vx4 , Mr_vx4), mode="undirected", weighted=TRUE)

# plot it
plot(network, edge.label=round(E(network)$weight, 3))

qgraph(m)
qgraph(adult_zeroorder ,edge.labels=TRUE)

######------------DISCARDED CODE-------Vinita
#Get the new variable names for every excel sheet 

sheet = sheet_Original 

get_VarNames(sheet_Original )
get_VarNames <- function(sheet){
  new_factorNames = list()
  for(i in 1:length(new_factorNames)){
    
    for(j in 1:length(sheet)){
      
      n = 1:as.numeric(glue(parse_number(sheet[j])))
      
      #graduate factors
      g_factors = vector(length = str_match(sheet[j], "\\d+"))
      for(k in 1:length(g_factors)){
        g_factors[k] <- glue("g_",glue(sheet[j]),"_",glue(n[k]))
      }
      
      #non-graduate factors
      ng_factors = vector(length = str_match(sheet[j], "\\d+"))
      for(k in 1:length(ng_factors)){
        ng_factors[k] <- glue("ng_",glue(sheet[j]),"_",glue(n[k]))
      }
      
      name <- glue(sheet[j])
      tmp <- list(g_factors = g_factors, ng_factors = ng_factors)
      new_factorNames[[name]] <- tmp
    }
    
  }
  return(new_factorNames)
  
}

rename_with <- function(dataType){
  if(dataType=="original"){
    #Get all the sheet names from the excel files in a data frame
    sheet_Original <- excel_sheets(path = here::here("data", "EducationStratifiedStructureMatrices_Original.xlsx"))
    list = get_VarNames(sheet= sheet_Original)
  }
  
  if(dataType=="ipsatized"){
    #Get all the sheet names from the excel files in a data frame
    sheet_Ipsatized <- excel_sheets(path = here::here("data", "EducationStratifiedStructureMatrices_Ipsatized.xlsx"))
    list = get_VarNames(sheet= sheet_Ipsatized)
  }
  
  return(list)
}


VarNames_Orig <- list.flatten(rename_with(dataType = "original")) %>%
  map_dfr(~ .x %>% as_tibble(), .id = "name") %>% 
  mutate(sheet = gsub("\\..*","",name)) %>% 
  vec_split(x =value, by = sheet)

v = vec_split(x =f$value, by = f$sheet)

v




VarNames_Ips <- list.flatten(rename_with(dataType = "ipsatized")) %>%
  map_dfr(~ .x %>% as_tibble(), .id = "name") %>% 
  mutate(sheet = gsub("\\..*","",name))


#calling the right data from the right sheet
callData <- function(dataType, sheetName){
  if(dataType == "original"){
    data <- rio::import(here::here("data", "EducationStratifiedStructureMatrices_Original.xlsx"), setclass = "tbl_df", sheet = {{sheetName}}) %>% janitor::clean_names()
  }
  
  if(dataType == "ipsatized"){
    data <- rio::import(here::here("data", "EducationStratifiedStructureMatrices_Ipsatized.xlsx"), setclass = "tbl_df", sheet = {{sheetName}})%>% janitor::clean_names()
  }
  
  return(data)
}

#cleaning the data

d = callData("original", glue(sheet_Original[1])) %>% 
  # Remove columns where all values are NA
  select(where(~!all(is.na(.)))) %>% 
  #think of how to specify the first column name here!!
  filter(!str_detect(grad_1, "xtract")) %>% 
  #select only numeric columns
  select_if(is.numeric) 
#rename columns

library(huxtable)
d %>% 
  setNames(v[[1]][1])


map(rename_with(.cols = c(1: ncol(.)), ~VarNames_Orig[1]), d)


# Optimum_cc function which is not compatible for structure beyond 11 factors 
# ----Functions - Congruence coefficient

x = Fr_vx4 ; y = Mr_vx4
optimum_cc <- function(x, y){
  
  #build data
  congruence_mat = psych::factor.congruence({{x}} , {{y}})
  mat_long = reshape2::melt(congruence_mat, na.rm=TRUE, value.name="cc") %>%
    arrange(desc(abs(cc)))
  
  #build a dataframe for combinations
  Var1 = rep(names({{x}}), factorial(ncol({{x}})))
  Var2 = combinat::permn(c(names({{y}})))%>%unlist()
  group = rep(c(1:factorial(ncol({{x}}))), times = c(rep(ncol({{x}}),factorial(ncol({{x}})))))
  df_comb = data.frame(Var1,Var2, group)
  
  #dataframe with all combinations
  all_comb = left_join(df_comb, mat_long)
  
  #optimum group number
  group_no <- all_comb %>%
    group_by(group) %>%
    summarise(cc_sum = sum(abs(cc))) %>%
    arrange(desc(cc_sum)) %>%
    head(1) %>%
    pull(group)
  
  #pull the combination with the optimum cc value
  all_comb %>%
    filter(group == group_no)
  
}

#Here is how you can use it, just provide the two vectors
#optimum_cc(x = Fr_vx5 , y = Mr_vx5)

#Compute congruence coefficient for grads and non-grads for every rotation
compute_fc <- function(df){
  grads <- df%>% 
    select(starts_with("g_")) 
  
  non_grads <- df %>% 
    select(starts_with("ng_")) 
  
  optimum_cc(x = grads, y=non_grads)
}
