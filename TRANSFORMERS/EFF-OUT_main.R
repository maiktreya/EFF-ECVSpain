#### DEFINE NEEDED LOCAL VARIABLES
quantile_cuts <- c(.5, .75, .8, .9, .95, .99, .999)
alt_results_year <- alt_percents <- list()
alt_medians <- data.table()

source("SOURCE/GENERATORS/SELECTORS/EFF_treatment.R")
# IMPORT AND DEFINE THE SURVEY OBJECT
survey_weights <- as.svydesign2(svydesign(
        ids = ~1,
        data = as.data.frame(full_mean),
        weights = ~ full_mean$facine3
))
#### COMPUTE OR EXCLUDE 0 VALUES
ifelse(reduced == T, survey_weights$variables[survey_weights$variables == 0] <- NA, survey_weights$variables[is.na(survey_weights$variables)] <- 0)
main_mean <- data.frame(survey_weights$variables)
estimable_vars <- colnames(main_mean)[4:length(colnames(main_mean))] # IGNORE 3 FIRST NON-NUMERIC VARS
estimable_vars <- estimable_vars[!(estimable_vars %in% c("p7_4a"))] # EXCLUDE ANY CONFLICTING VARIABLE NOT NECESSARY

#### LOOP OVER VARIABLES TO GET STATISTICS
for (k in estimable_vars) {
        ###### PREPARE FACTOR VARIABLES FOR COMPUTING PERCENTS
        main_factor <- factor(main_mean[, k])
        main_factor <- factor(ifelse(main_mean[, k] > 0, "1", "0"))
        if (length(levels(main_factor)) != 2) main_factor <- rnorm(length(main_mean[, "facine3"]))
        ###### ESTIMATE PERCENTS
        alt_percents[k] <- svymean(~main_factor, survey_weights, na.rm = T)
        ###### ESTIMATE MEANS
        alt_results_year[k] <- svymean(~ main_mean[, k], survey_weights, na.rm = T)[1] %>% unname()
        ###### ESTIMATE MEDIANS AND QUANTILES
        alt_medians[, k] <- svyquantile(~ main_mean[, k], survey_weights, quantile_cuts, na.rm = T)[[1]][, "quantile"]
}

#### FORMAT RESULTS AND APPEND THEM TO PREVIOUS RELEASES
pre_results <- t(alt_medians)
colnames(pre_results) <- quantile_cuts
pre_results <- cbind(pre_results, "means" = as.numeric(alt_results_year), "perct" = as.numeric(alt_percents)) %>% data.table(keep.rownames = T)
pre_results[, year := sel_year]
ifelse(reduced == T, pre_results[, kind := "greater-0"], pre_results[, kind := "all-values"])
alt_results <- rbind(alt_results, pre_results)