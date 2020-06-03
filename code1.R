
Packages <- c("data.table", "dplyr", "qwraps2")
lapply(Packages, library, character.only = T)

# path = "/Volumes/GoogleDrive/My Drive/Desktop/2020 spring/medication_modeling_DRAFT_202005061320/"
# setwd(path)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path = "medication_modeling_DRAFT_202005110947/"
list.files(path)

drugs = fread(paste0(path,"medication_modeling_drugs_DRAFT_202005061320.csv"))
demog = fread(paste0(path,"medication_modeling_demographics_DRAFT_202005061320.csv"))
ebo   = fread(paste0(path,"medication_modeling_ebo_DRAFT_202005061320.csv"))


### IMPORTANT
# For CURRENT med, on med for more than a year (encounter date - start dat)
# -> define outcome category stable

# can we predict who stop medication - Taper off / stopped from side effect
# physician or patient decision?
# discontinue of medication: how is that reflected in the data? 
# Last contact date
table(drugs[, .N, by = cohort_id]$N) #num of records per person

ud = unique(drugs, by=c("cohort_id", "dmardname"))
table(ud[, .N, by = cohort_id]$N) # num of unique contact date

tmp = drugs[,.N,by = dmardstopreason]

tmp = drugs[,.N,by = list(dmardname,dmardstopreason)]

tmp =tmp[order(dmardname,dmardstopreason)]
#tmp[dmardstopreason == "inefficacy",]
tmp[dmardstopreason == "tapered off",]

TargetMed = c("Azathioprine", "Methotrexate", "Mycophenolate", "Rituximab", "IVIG")

sum(drugs$dmardstartdate!= "NULL")
mvs = unique(drugs$dmardname)
pd = drugs[, ]



## Create binary antibody variables 

# Jo-1, NXP-2, TIF1g, Mi2, PM-SCL, and MDA5.
# Mi2 requires both Mi2alpha AND Mi2beta >/=15
# PmScl requires both PM75 AND PM100 are >/=15
# Positive: >/=15
# Negative <15

# PL-7, PL-12, OJ, EJ, SAE, SRP
# Positive:  >/=36
# Negative <36

# [Autoantibody groups]
# ASyS: Jo1 + PL7 + PL12 + OJ + EJ + PM-Scl
# Dermatomyositis: MDA5, TIF1g, NXP2, SAE, MI2
# IMNM: SRP + HMGCR

table(ebo$`rosen_anti-hmgcr`, ebo$`hx_ab_anti-hmgcr`) 

ebo <- ebo %>% filter(ebo_date != "NULL") %>%
  
  mutate(jo1 = ifelse(`ebo_anti-jo1` >= 15, 1, 0),
         nxp2 = ifelse(`ebo_anti-nxp2` >= 15, 1, 0),
         tif1 = ifelse(`ebo_anti-tif1` >= 15, 1, 0),
         mi2 = ifelse(`ebo_anti-mi2a` >= 15 & `ebo_anti-mi2b` >= 15, 1, 0),
         pmscl = ifelse(`ebo_anti-pm100` >= 15 & `ebo_anti-pm75` >= 15, 1, 0),
         mda5 = ifelse(`ebo_anti-mda5` >= 15, 1, 0),
         
         pl7 = ifelse(`ebo_anti-pl7` >= 36, 1, 0),
         pl12 = ifelse(`ebo_anti-pl12` >= 36, 1, 0),
         oj = ifelse(`ebo_anti-oj` >= 36, 1, 0),
         ej = ifelse(`ebo_anti-ej` >= 36, 1, 0),
         sae = ifelse(`ebo_anti-sae` >= 36, 1, 0),
         srp = ifelse(`ebo_anti-srp` >= 36, 1, 0),
         
         hmgcr = ifelse(`rosen_anti-hmgcr` == "Positive"|`hx_ab_anti-hmgcr` == "yes", 1, 0), 
         
         asys = pmax(jo1, pl7, pl12, oj, ej, pmscl),
         dermatomyositis = pmax(mda5, tif1, nxp2, sae, mi2),
         imnm = pmax(srp, hmgcr))


## create summary table
ebo_summary <-  ebo %>% select(asys, dermatomyositis, imnm) %>% sapply(as.character) %>%
  as.data.frame() %>% qsummary(., n_perc_args = list(digits = 1, show_symbol = TRUE))

st <- ebo %>% summary_table(., ebo_summary)
print(st)









