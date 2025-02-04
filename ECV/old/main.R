### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls())
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "data.table") %>% sapply(library, character.only = TRUE)

### PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999)
important_variables <- c("PB040", "HY040N", "DB040", "HY090G", "HY100N", "HY130N", "HH021", "PY035N", "PB140", "PL031", "PL040", "PL051", "PY010N", "PB110")

sel_year <- 2022
survey_ecv <- fread(paste0("output/filtered_data", sel_year, ".csv"))
metadata_hogar <- read.csv("metadata/metadata.hogar.h-d.csv")
metadata_personal <- fread("metadata/metadata.personal.r-p.csv")

## PB040 FACTORE ELEVACIÓN, HB070 CÓDIGO DEL REPORTADOR
survey_ecv$tenancy <- survey_ecv$HH021
survey_ecv$AGE <- survey_ecv$AGE %>% as.factor()
survey_ecv[tenancy != 1, "tenancy"] <- 0
survey_total <- as.svydesign2(svydesign(
    ids = ~1,
    data = survey_ecv,
    weights = ~ survey_ecv$PB040
))

# AGE PB140 YEAR OF BIRTH
# AGE $AGE CLUSTER OF AGE
# CLASS PL031
# GENDER PB150
# REGION DB040
# HOUSEHOLD INCOME HY020

# GRADO URBANIZACIÓN DB100
# 1 - hig-population density(+50k + 500 h/km)
# 2 - mid-population density(+50k or adyacent + 100 h/km)
# 3 - low-population denstiy(else)

# TENANCY HH021
# 1. En propiedad sin hipoteca
# 2. En propiedad con hipoteca
# 3. En alquiler o realquiler a precio de mercado
# 4. En alquiler o realquiler a precio inferior al de mercado
# 5. En cesión gratuita

test <- svyglm(tenancy ~ HY020 + AGE + PB150 + PL031 + DB040 + DB100 + RB290, design = survey_total)
test %>% summary()