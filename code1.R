library(data.table)
setwd(path)
path = "/Volumes/GoogleDrive/My Drive/Desktop/2020 spring/medication_modeling_DRAFT_202005061320/"
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