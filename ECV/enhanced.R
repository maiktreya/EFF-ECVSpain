library(data.table)
library(magrittr)
library(survey)
library(stringr)

datos_hogar <- fread("DATASETS/ECV/esudb23d.csv")
detalle_hog <- fread("DATASETS/ECV/esudb23h.csv")
datos_perso <- fread("DATASETS/ECV/esudb23r.csv")
detalle_per <- fread("DATASETS/ECV/esudb23p.csv")

datos_hogar[, new_iden := as.numeric(DB030)]
detalle_hog[, new_iden := as.numeric(HB030)]
datos_perso[, new_iden := as.numeric(str_extract(RB030, "^.+(?=\\w{2}$)"))][, PB030 := RB030][,RB030 := NULL]
detalle_per[, new_iden := as.numeric(str_extract(PB030, "^.+(?=\\w{2}$)"))]
merged_households <- merge(
  datos_hogar,        # Primary household data
  detalle_hog,        # Additional household details
  by = "new_iden",    # Merge key
  all.x = TRUE        # Keep all households from datos_hogar
)

merged_persons <- merge(
  datos_perso,        # Primary person data
  detalle_per,        # Additional person details
  by = c("new_iden", "PB030"),  # Composite key
  all.x = TRUE        # Keep all individuals from datos_perso
)

final_data <- merge(
  merged_persons,     # Personal data with details
  merged_households,  # Household data
  by = "new_iden",    # Household ID
  all.x = TRUE        # Keep all persons (even if household data is missing)
)

survey_total <- svydesign(
    ids = ~1,
    data = final_data,
    weights = ~ final_data$PB040
)
