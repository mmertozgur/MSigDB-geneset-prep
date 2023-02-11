
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