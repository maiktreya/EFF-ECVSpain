## CONDITIONAL FILTERS ON SURVEY WITH SVYBY FUNCTION
##### NAMES CONTAINERS
source("SOURCE/SELECTORS/names-filters2.R")

## CONVERSION TO FACTORIAL, TREATMENT, AND DEBT RATIOS DEFINITIONS
source("SOURCE/SELECTORS/EFF_treatment.R")

############### DEFINE SURVEY OBJECT
survey_weights <- as.svydesign2(svydesign(
    ids = ~1,
    data = as.data.frame(full_mean),
    weights = ~ full_mean$facine3
))
ifelse(reduced == T, survey_weights$variables[survey_weights$variables == 0] <- NA, survey_weights$variables[is.na(survey_weights$variables)] <- 0)
main_mean <- data.frame(survey_weights$variables)

## CLASS VARIABLES
filters_class <- data.table()
for (i in seq_along(filter_names_class)) {
    cat <- paste0("~", filter_names_class[i]) %>% as.formula()
    ca2 <- paste0("~as.integer(as.logical(", filter_names_class[i], "))") %>% as.formula()
    fil <- paste0("~", filter_cat["class"]) %>% as.formula()
    fun <- svyby(cat, fil, survey_weights, svymean, keep.var = F, na.rm = reduced)[2]
    med <- svyby(cat, fil, survey_weights, svyquantile, quantiles = .5, na.rm = reduced)[2]
    per <- svyby(ca2, fil, survey_weights, svymean, quantiles = .5, na.rm = reduced)[2]
    filters_class[, paste0(filter_names_class[i], "-MEAN") := fun][, paste0(filter_names_class[i], "-MED") := med][, paste0(filter_names_class[i], "-PER") := per]
}
filters_class[, year := sel_year][, categories := tipo_empleo]
## CLASS_BIN VARIABLES
filters_bin <- data.table()
for (i in seq_along(filter_names_bin)) {
    cat <- paste0("~", filter_names_bin[i]) %>% as.formula()
    ca2 <- paste0("~as.integer(as.logical(", filter_names_class[i], "))") %>% as.formula()
    fil <- paste0("~", filter_cat["class_bin"]) %>% as.formula()
    fun <- svyby(cat, fil, survey_weights, svymean, keep.var = F, na.rm = reduced)[2]
    med <- svyby(cat, fil, survey_weights, svyquantile, quantiles = .5, na.rm = reduced)[2]
    per <- svyby(ca2, fil, survey_weights, svymean, quantiles = .5, na.rm = reduced)[2]
    filters_bin[, paste0(filter_names_bin[i], "-MEAN") := fun][, paste0(filter_names_bin[i], "-MED") := med][, paste0(filter_names_class[i], "-PER") := per]
}
filters_bin[, year := sel_year]
## AGE VARIABLES
filters_age <- data.table()
for (i in seq_along(filter_names_age)) {
    cat <- paste0("~", filter_names_age[i]) %>% as.formula()
    ca2 <- paste0("~as.integer(as.logical(", filter_names_class[i], "))") %>% as.formula()
    fil <- paste0("~", filter_cat["age"]) %>% as.formula()
    fun <- svyby(cat, fil, survey_weights, svymean, keep.var = F, na.rm = reduced)[2]
    med <- svyby(cat, fil, survey_weights, svyquantile, quantiles = .5, na.rm = reduced)[2]
    per <- svyby(ca2, fil, survey_weights, svymean, quantiles = .5, na.rm = reduced)[2]
    filters_age[, paste0(filter_names_age[i], "-MEAN") := fun][, paste0(filter_names_age[i], "-MED") := med][, paste0(filter_names_class[i], "-PER") := per]
}
filters_age[, year := sel_year][, categories := rango_edad]
##  DESPOSEIDOS Y HERENCIA
filters_depo <- data.table()
filters_depo1 <- data.table(prop.table(svytable(~p2_2, survey_weights)))[, var := "totalpop"]
filters_depo2 <- data.table(prop.table(svytable(~ p2_2 + nsitlabdom, survey_weights)))[, var := "classes"]
filters_depo3 <- data.table(prop.table(svytable(~nsitlabdom, survey_weights)))[, var := "share_pop_class"]
filters_depo <- rbind(filters_depo1, filters_depo2, filters_depo3, fill = TRUE)[, year := sel_year][, reduced := reduced]

## HIPOTECADOS
filters_hipo <- data.table()
survey_weights_hipo <- subset(survey_weights, dvivpral > 0)
for (i in seq_along(filter_names_class)) {
    cat <- paste0("~", filter_names_class[i]) %>% as.formula()
    ca2 <- paste0("~as.integer(as.logical(", filter_names_class[i], "))") %>% as.formula()
    fil <- paste0("~", filter_cat["class"]) %>% as.formula()
    fun <- svyby(cat, fil, survey_weights_hipo, svymean, keep.var = F, na.rm = reduced)[2]
    med <- svyby(cat, fil, survey_weights_hipo, svyquantile, quantiles = .5, na.rm = F)[2]
    per <- svyby(ca2, fil, survey_weights_hipo, svymean, quantiles = .5, na.rm = reduced)[2]
    filters_hipo[, paste0(filter_names_class[i], "-MEAN") := fun][, paste0(filter_names_class[i], "-MED") := med][, paste0(filter_names_class[i], "-PER") := per]
}
filters_hipo[, year := sel_year][, categories := tipo_empleo]
## MULTI DIMENSIONAL CONTINGENCY TABLES COMBINING CLASS AND AGE
filter_multi1 <- svyby(~p2_1, ~ nsitlabdom + bage, survey_weights, svymean, keep.var = F, na.rm = reduced) %>% data.table()
filter_multi1 <- filter_multi1[, year := sel_year][, var := "2_1_cesion"][, reduced := as.character(reduced)]
filter_multi2 <- svyby(~p2_33, ~ nsitlabdom + bage, survey_weights, svymean, keep.var = F, na.rm = reduced) %>% data.table()
filter_multi2 <- filter_multi2[, year := sel_year][, var := "p2_2_inheritance"][, reduced := as.character(reduced)]
filter_multi3 <- svyby(~s6_owner, ~ nsitlabdom + bage, survey_weights, svymean, keep.var = F, na.rm = reduced) %>% data.table()
filter_multi3 <- filter_multi3[, year := sel_year][, var := "multiprop"][, reduced := as.character(reduced)]
filter_multi4 <- svyby(~valor, ~ nsitlabdom + bage, survey_weights, svymean, keep.var = F, na.rm = reduced) %>% data.table()
filter_multi4 <- filter_multi4[, year := sel_year][, var := "valor_pensiones"][, reduced := as.character(reduced)]
filter_multi5 <- svyby(~riquezafin, ~ nsitlabdom + bage, survey_weights, svymean, keep.var = F, na.rm = reduced) %>% data.table()
filter_multi5 <- filter_multi5[, year := sel_year][, var := "riquezafin"][, reduced := as.character(reduced)]
filter_multi <- rbind(filter_multi1, filter_multi2, filter_multi3, filter_multi4, filter_multi5)
###### AGGREGATE ANNUAL RESULTS TO JOINT FINAL TABLE
wealth_filters2 <- rbind(wealth_filters2, filters_class)
wealth_filters3 <- rbind(wealth_filters3, filters_age)
wealth_filters4 <- rbind(wealth_filters4, filters_bin)
wealth_filters5 <- rbind(wealth_filters5, filter_multi)
wealth_filters6 <- rbind(wealth_filters6, filters_hipo)
wealth_filters7 <- rbind(wealth_filters7, filters_depo)