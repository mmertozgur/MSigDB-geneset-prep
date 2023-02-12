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

#H / C5 - " GO:BP " GO:CC " GO : MF "/ C6 / C7 - " IMMUNESIGDB " VAX "
h = msigdb_handler("H", "")
c5_go_bp = msigdb_handler("C5", "GO:BP")
c5_go_cc = msigdb_handler("C5",  "GO:CC")
c5_go_mf = msigdb_handler("C5", "GO:MF")
c6 = msigdb_handler("C6", "")
c7_immunesigdb = msigdb_handler("C7", "IMMUNESIGDB")
c7_vax = msigdb_handler("C7", "VAX")

msigdb_gene_sets = c( "h" = h, "c5_go_bp" = c5_go_bp, "c5_go_cc" = c5_go_cc, "c5_go_mf" = c5_go_mf  , "c6" = c6,
                      "c7_immunesigdb" = c7_immunesigdb, "c7_vax" = c7_vax)


eg_1 = filter(msigdb_gene_sets$h.data, gs_name == "HALLMARK_ALLOGRAFT_REJECTION") %>% select(gene_symbol)
eg_2 = filter(msigdb_gene_sets$c5_go_cc.data,gs_name == "GOCC_ACROSOMAL_VESICLE") %>% select(gene_symbol)
eg_3 = filter(msigdb_gene_sets$c7_immunesigdb.data,gs_name == "GOLDRATH_NAIVE_VS_EFF_CD8_TCELL_DN") %>% select(gene_symbol)

saveRDS(msigdb_gene_sets, file("msigdb_gene_sets.rds"))


