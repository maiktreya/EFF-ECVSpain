### EFF STUDY 2023 MIGUEL GARCIA DUCH

## NEEDED LIBRARIES
library("magrittr")
library("data.table")
library("survey")
c("LIBRARY/my_functions.R", "LIBRARY/widgets.R") %>% sapply(source)

## GLOBAL VARIABLES DECLARATION
start_time <- Sys.time()
wealth_filters <- wealth_filters2 <- wealth_filters3 <- wealth_filters4 <- wealth_filters5 <- wealth_filters6 <- wealth_filters7 <- wealth_filters8 <- alt_results <- gini_fin_list <- gini_val_list <- data.table()
years <- c( 2011, 2014, 2017, 2020)
reduced <- NA

## STRUCTURED LOOP OVER SURVEYS WITH MEMORY CLEANING EACH ITERATION, FIRST INCLUDING ALL PEOPLE, THEN REDUCED TO THOSE WHERE VARS ARE > 0
for (reduced in c(FALSE, TRUE)) {
    for (i in seq_along(years)) {
        ## DEFINE CHANGING PARAMETERS AND DATA FOR EACH ANNUAL ITERATION
        sel_year <- years[i]
        saved_vars <- c(
            "start_time", "reduced", "years", "wealth_filters", "wealth_filters2", "wealth_filters3", "wealth_filters4",
            "wealth_filters5", "wealth_filters6", "wealth_filters7", "wealth_filters8", "alt_results", "gini_val_list", "gini_fin_list"
        )
        full_mean <- paste0("DATASETS/FAST_EFF/", sel_year, "-EFF.microdat.csv") %>% fread()

        ## PERFORM ANALYSIS
        source("TRANSFORMERS/EFF-OUT_main.R") # GENERAL MEANS
        source("TRANSFORMERS/EFF-OUT_filters.R") # FILTERS BY USE AND ORIGIN OF PROPERTY
        source("TRANSFORMERS/EFF-OUT_filters2.R") # FILTERS BY CLASS AND AGE

        ## CLEAN MEMORY OF INNECESARY VARIABLES
        if (sel_year != 2020) rm(list = setdiff(ls(), saved_vars))
    }
}
## EXPORT OBTAINED RESULTS TO CSV
fwrite(alt_results, file = "OUTPUT/CSV/alt_results.csv")
fwrite(wealth_filters, file = "OUTPUT/CSV/filtros_tipo_uso.csv")
fwrite(wealth_filters2, file = "OUTPUT/CSV/clase.csv")
fwrite(wealth_filters3, file = "OUTPUT/CSV/age.csv") # nolint
fwrite(wealth_filters4, file = "OUTPUT/CSV/clase_binary.csv")
fwrite(wealth_filters5, file = "OUTPUT/CSV/class_age_ownership.csv")
fwrite(wealth_filters6, file = "OUTPUT/CSV/filtros_hipotecados.csv")
fwrite(wealth_filters7, file = "OUTPUT/CSV/filtros_desposeidos_clase.csv")
fwrite(wealth_filters8, file = "OUTPUT/CSV/class_age_ownership_var.csv")

## BENCHMARK PERFORMANCE THROUGH TOTAL TIME OF EXECUTION AND SAVE FINAL STATE
(Sys.time() - start_time) %>% print()
unlink("SAVES/*.RData*") # nolint
paste0("SAVES/", start_time, ".RData") %>% save.image()
