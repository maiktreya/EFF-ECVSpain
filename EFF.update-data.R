### EFF STUDY 2023 MIGUEL GARCIA DUCH

## NEEDED LIBRARIES
library("magrittr")
library("data.table")
library("survey")
c("LIBRARY/my_functions.R", "LIBRARY/widgets.R") %>% sapply(source)

## GLOBAL VARIABLES DECLARATION
start_time <- Sys.time()
wealth_filters <- wealth_filters2 <- wealth_filters3 <- wealth_filters4 <- wealth_filters5 <- wealth_filters6 <- wealth_filters7 <- alt_results <- data.table()
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
reduced <- NA

## STRUCTURED LOOP OVER SURVEYS WITH MEMORY CLEANING EACH ITERATION, FIRST INCLUDING ALL PEOPLE, THEN REDUCED TO THOSE WHERE VARS ARE > 0
for (reduced in c(TRUE, FALSE)) {
    for (i in seq_along(years)) {

        ## DEFINE CHANGING PARAMETERS AND DATA FOR EACH ANNUAL ITERATION
        sel_year <- years[i]
        saved_vars <- c("start_time", "reduced", "years")
        full_mean <- paste0("DATASETS/FAST_EFF/", sel_year, "-EFF.microdat.csv") %>% fread()

        ## PERFORM ANALYSIS
        source("GENERATORS/EFF-OUT_full.R") # nolint

        ## CLEAN MEMORY OF INNECESARY VARIABLES
        if (sel_year != 2020) rm(list = setdiff(ls(), saved_vars))
    }
}

## BENCHMARK PERFORMANCE THROUGH TOTAL TIME OF EXECUTION AND SAVE FINAL STATE
(Sys.time() - start_time) %>% print()
paste0("SAVES/", start_time, ".RData") %>% save.image()