

library(maaslin3)

library(ggplot2)
theme_set(theme_bw(base_family = "Helvetica"))

#Seagrassを基準にする場合 Bacteria編

A <- read.csv("Bacteria_seagrass_D4_family.csv", header = T, row.names = 1)
A <- read.csv("Bacteria_seagrass_D5_genus.csv", header = T, row.names = 1)
A <- read.csv("Eukaryota_seagrass_D4.csv", header = T, row.names = 1)
A <- read.csv("Eukaryota_seagrass_D15.csv", header = T, row.names = 1)
A <- read.csv("Chemical_seagrass.csv", header = T, row.names = 1)

meta= read.csv("Seagrass_list_cnd.csv", header = T, row.names = 1) 

meta$cnd <- factor(meta$cnd, levels = c("seagrass","non"))  

meta$env <- factor(meta$env, levels = c(
    "Okayama_Y","Noto", "Kitsuki", "Hakata","Okayama_G", 
    "Saiki", "Tokyo_KC", "Tokyo_HK"
  ))
  
  
#Bacteria
  fit_out <- maaslin3(
    input_data = A,
    input_metadata = meta,
    output = 'Seagrass_Bacteria_data',
    formula = '~ cnd + env + shore',  
    normalization = 'TSS',
    transform = 'LOG',
    augment = TRUE,
    standardize = TRUE,
    max_significance = 0.05,
    median_comparison_abundance = TRUE,
    median_comparison_prevalence = FALSE,
    max_pngs = 100,
    cores = 1,
    save_models = TRUE
  )
  
  
#Eukaryota
  fit_out <- maaslin3(
    input_data = A,
    input_metadata = meta,
    output = 'Seagrass_Eukaryota_data',
    formula = '~ cnd + env + shore',  
    normalization = 'TSS',
    transform = 'LOG',
    augment = TRUE,
    standardize = TRUE,
    max_significance = 0.05,
    median_comparison_abundance = TRUE,
    median_comparison_prevalence = FALSE,
    max_pngs = 100,
    cores = 1,
    save_models = TRUE
  )
  
#Chemical
  fit_out <- maaslin3(
    input_data = A,
    input_metadata = meta,
    output = 'Seagrass_Chemical_data',
    formula = '~ cnd + env + shore',  
    normalization = 'TSS',
    transform = 'LOG',
    augment = TRUE,
    standardize = TRUE,
    max_significance = 0.05,
    median_comparison_abundance = TRUE,
    median_comparison_prevalence = FALSE,
    max_pngs = 100,
    cores = 1,
    save_models = TRUE
  )
  
 

