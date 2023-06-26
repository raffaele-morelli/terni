library(readxl)
TerniCarcinogenicRisk <- read_excel("data/TERNI PM Elements Data.xlsx", 
                                     sheet = "CR - Carcinogenic Risk")
saveRDS(TerniCarcinogenicRisk, file = "data/TerniCarcinogenicRisk.RDS")

TerniNonCarcinogenicRisk <- read_excel("data/TERNI PM Elements Data.xlsx", 
                                     sheet = "NCR - Non-Carcinogenic Risk", 
                                     col_types = c("date", "date", "text", 
                                                   "text", "text", "text", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "text", "numeric", "numeric", "numeric"))
saveRDS(TerniNonCarcinogenicRisk, file = "data/TerniNonCarcinogenicRisk.RDS")

TerniELEMENTI <- read_excel("data/TERNI PM Elements Data.xlsx", 
                                     sheet = "ELEMENTI", col_types = c("date", 
                                                                       "date", "text", "text", "text", "text", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "text", "numeric", 
                                                                       "numeric", "text", "numeric", "numeric", 
                                                                       "numeric"))

saveRDS(TerniELEMENTI, file = "data/TerniELEMENTI.RDS")


TerniSamplingSites <- read_excel("data/TERNI PM Elements Data.xlsx", 
                                     sheet = "Sampling Sites")
saveRDS(TerniSamplingSites, file = "data/TerniSamplingSites.RDS")
