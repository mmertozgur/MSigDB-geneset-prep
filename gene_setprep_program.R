library(tidyverse)
library(dplyr)
library(msigdbr)
library(edgeR)

msigdb_handler <-  function(category,subcategory) {
  hallmark_sets <- as.data.frame(msigdbr(species = "Homo sapiens",category = category, subcategory = subcategory))
  hallmark_sets <-hallmark_sets %>% select(c(gs_name,gene_symbol))
  hsets_names4freq <- as.data.frame(table(hallmark_sets %>% select(gs_name)))
  alist <- list("data" = hallmark_sets, "names" = hsets_names4freq)
  return(alist)
}

#####################################################################################

msigdb_collection.list = c("H","C1","C2","C3","C4","C5","C6","C7","C8") 
m_list = list()
main_list = list()
for (i in msigdb_collection.list) {
  print(i)
  category = as.data.frame(msigdbr(species = "Homo sapiens",category = i)) %>% 
    select(gs_cat, gs_subcat, gs_name, gene_symbol) 
  subcategory = names(table(category$gs_subcat))
  
  if (length(subcategory) > 1) {
    
    for (s in subcategory) {
      set_holder = filter(category, gs_subcat ==paste0(s)) %>% select(gs_name, gene_symbol)
      
      m_list[[paste0(s)]] = set_holder
    }
    main_list[[paste0(i)]] = m_list
  } else {
    set_holder =  category %>% select(gs_name, gene_symbol)
    main_list[[paste0(i)]] = set_holder
  }
  
  m_list = list()
}

saveRDS(main_list, file = "msigdb_collections.rds")
readRDS(file = "msigdb_collections.rds")


View(m[["C5"]][[""]])
View(m[["H"]])

names(m[["C2"]])


gene_list = filter(m[["C5"]][["GO:BP"]],gs_name == "GOBP_ACTIN_FILAMENT_BASED_PROCESS" ) %>% select(gene_symbol)
gene_list = filter(m[["H"]],gs_name == "HALLMARK_ADIPOGENESIS" ) %>% select(gene_symbol)
