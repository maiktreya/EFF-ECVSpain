# Load required libraries
library(data.table) # For efficient data manipulation
library(magrittr) # For piping (%>%)
library(survey) # For survey data analysis
library(stringr) # For string manipulation

# define parameters
quantile_cuts <- c(0.25, 0.5, 0.75, 0.9)
sel_year <- "22" # set year

#  Load the datasets
ipc <- readxl::read_xlsx("DATASETS/ECV/IPC.xlsx") %>% data.table()
datos_hogar <- fread(paste0("DATASETS/ECV/esudb", sel_year, "d.csv")) # Household-level data
detalle_hog <- fread(paste0("DATASETS/ECV/esudb", sel_year, "h.csv")) # Additional household details

# Create `new_iden` for household data
datos_hogar[, new_iden := as.numeric(DB030)] # Create `new_iden` in datos_hogar
detalle_hog[, new_iden := as.numeric(HB030)] # Create `new_iden` in detalle_hog

# Merge household-level datasets
merged_households <- merge(
  datos_hogar, # Primary household data
  detalle_hog, # Additional household details
  by = "new_iden", # Merge key (household ID)
  all.x = TRUE # Keep all rows from datos_hogar (left join)
)[!is.na(DB090)][, renta_real := 0][, renta_real := vhRentaa / ipc[AÑO_RENTA == as.numeric(paste0(20, sel_year))]$deflactor_IPC]

# Create the survey design object
survey_total <- svydesign(
  ids = ~1, # No clustering
  data = merged_households, # Final dataset
  weights = ~DB090 # Weights column
)

# Generate quantiles
quantiles_renta <- svyquantile(~vhRentaa, design = survey_total, quantiles = quantile_cuts)$vhRentaa[, "quantile"]
quantiles_renta_real <- svyquantile(~renta_real, design = survey_total, quantiles = quantile_cuts)$renta_real[, "quantile"]
quantiles <- data.table(tramo = quantile_cuts, renta_nom = quantiles_renta, renta_real = quantiles_renta_real)

# Renta media del tramo
renta90 <- svymean(~renta_real, design = subset(survey_total, renta_real > quantiles[tramo == "0.9", renta_real]), na.rm = TRUE)[1]
renta50 <- svymean(~renta_real, design = subset(survey_total, renta_real > quantiles[tramo == "0.5", renta_real]), na.rm = TRUE)[1]

# Implied ratio quantiles
ratio <- quantiles[tramo == "0.9", renta_real] / quantiles[tramo == "0.5", renta_real] %>% round(3) # ratio cuantiles
ratios <- data.table(ratio_quantiles = ratio, ratio_medias = round(renta90 / renta50, 3)) # Implied ratio mean from quantiles

# Comprobar ine
rentaINE <- svymean(~vhRentaa, design = survey_total, na.rm = TRUE)[1] / ipc[AÑO_RENTA == as.numeric(paste0(20, sel_year))]$deflactor_IPC

# Show results
ratios %>% print()
paste0("Renta:", round(rentaINE)) %>% print() # deber ser 32216 para 2022
quantiles %>% print()
