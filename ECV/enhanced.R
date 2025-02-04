# Load required libraries
library(data.table)   # For efficient data manipulation
library(magrittr)     # For piping (%>%)
library(survey)       # For survey data analysis
library(stringr)      # For string manipulation

# Step 1: Load the datasets
datos_hogar <- fread("DATASETS/ECV/esudb23d.csv")  # Household-level data
detalle_hog <- fread("DATASETS/ECV/esudb23h.csv")  # Additional household details
datos_perso <- fread("DATASETS/ECV/esudb23r.csv")  # Person-level data
detalle_per <- fread("DATASETS/ECV/esudb23p.csv")  # Additional person details

# Step 2: Create `new_iden` for household data
datos_hogar[, new_iden := as.numeric(DB030)]  # Create `new_iden` in datos_hogar
detalle_hog[, new_iden := as.numeric(HB030)]  # Create `new_iden` in detalle_hog

# Step 3: Create `new_iden` and `PB030` for personal data
datos_perso[, new_iden := as.numeric(str_extract(RB030, "^.+(?=\\w{2}$)"))]  # Household ID
datos_perso[, PB030 := RB030][, RB030 := NULL]  # Rename RB030 to PB030 for consistency and drop the origial
detalle_per[, new_iden := as.numeric(str_extract(PB030, "^.+(?=\\w{2}$)"))]  # Household ID

# Step 4: Merge household-level datasets
merged_households <- merge(
  datos_hogar,        # Primary household data
  detalle_hog,        # Additional household details
  by = "new_iden",    # Merge key (household ID)
  all.x = TRUE        # Keep all rows from datos_hogar (left join)
)

# Step 5: Merge person-level datasets
merged_persons <- merge(
  datos_perso,        # Primary person data
  detalle_per,        # Additional person details
  by = c("new_iden", "PB030"),  # Composite key (household + person ID)
  all.x = TRUE        # Keep all rows from datos_perso (left join)
)

# Step 6: Link persons to households
final_data <- merge(
  merged_persons,     # Personal data with details
  merged_households,  # Household data
  by = "new_iden",    # Household ID
  all.x = TRUE        # Keep all persons (even if household data is missing)
)

# Step 7: Drop rows with missing weights in `PB040`
# Filter out rows where `PB040` is NA
final_data <- final_data[!is.na(PB040)]

# Step 8: Create survey design object
# Ensure `PB040` is numeric (if not already)
final_data[, PB040 := as.numeric(PB040)]

# Create the survey design object
survey_total <- svydesign(
  ids = ~1,                     # No clustering
  data = final_data,            # Final dataset
  weights = ~PB040              # Weights column
)

# Step 9: Verify the survey design object
svytotal(~as.factor(HH021), design  = survey_total) %>% prop.table %>% print()