`%>%` <- magrittr::`%>%` # nolint
c("dplyr", "survey", "tidyr", "httr") %>% sapply(library, character.only = T)
c("lib/my_functions.R", "lib/widgets.R") %>% sapply(source)

### PARAMETERS AND VARIABLES TO INITIALIZE
mode <- T
reduced <- F
class_binary <- F
geogr <- ""
ft_results <- c()
year_bind <- c()
years <- c(2004:2022)
quantile_cuts <- c(.25, .5, .75, .9, .99, .999)
needed_variables <- c("PB040", "HY040N", "DB040", "HY090G", "HY100N", "HY130N", "HH021", "PY035N", "PB140", "PL031", "PL040", "PL051", "PY010N", "PB110")
# PL031 IS PL032 IN 2022 and PL051 IS DIVIDED IN PL051A Y PL051B

alt_rename <- c("HY090G", "HY100N", "HH021", "PL031", "PL051")
needed_rename <- c("HY090N", "HB010", "HH020", "PL030", "PL050")
description_names <- c(
    "weights",
    "renta neta alquiler",
    "intereses y ganancias K",
    "intereses pag. Hip Viv Pr",
    "pagos a otros hog",
    "reg.tenencia",
    "aport ppension",
    "año_nac", "
     trabajo_clase",
    " ultimo_trab",
    "categoria",
    "rentahog"
)
results_names <- c(
    "year",
    "RENTAL ALQ. VS CLASE",
    "",
    "RENTAL ALQ. VS REGIMEN DE TENENCIA",
    "",
    "RENTAL ALQ. VS AGE",
    "",
    "K RENT VS CLASE",
    "",
    "K RENT VS REGIMEN DE TENENCIA",
    "",
    "K RENT VS AGE",
    "",
    "RENT VS CLASE",
    "",
    "RENT VS REGIMEN DE TENENCIA",
    "",
    "RENT VS AGE",
    "",
    "PENSIONES VS CLASE",
    "",
    "PENSIONES VS REGIMEN DE TENENCIA",
    "",
    "PENSIONES VS AGE",
    "",
    "RENTAL ALQ.%",
    "",
    "",
    "K RENT %",
    "",
    "",
    "RENT %",
    "",
    "",
    "PENSIONES %",
    ""
)

############ API REQUESTS ##############
for (i in seq_along(years)) {
    sel_year <- years[i]
    all_data_persona <- read.csv2(paste0("data/esudb", substr(sel_year, 3, 4), "p.csv"), dec = ",", sep = ",", , quote = "\"")
    all_data_persona_b <- read.csv2(paste0("data/esudb", substr(sel_year, 3, 4), "r.csv"), dec = ",", sep = ",", , quote = "\"")
    all_data_hogar <- read.csv2(paste0("data/esudb", substr(sel_year, 3, 4), "h.csv"), dec = ",", sep = ",", , quote = "\"")
    all_data_hogar_b <- read.csv2(paste0("data/esudb", substr(sel_year, 3, 4), "d.csv"), dec = ",", sep = ",", , quote = "\"")
    all_data_hogar_b$DB040 <- all_data_hogar_b$DB040 %>%
        substr(3, 4) %>%
        as.numeric()
    all_data_persona$RB220 <- all_data_persona_b
    all_data_hogar <- cbind(all_data_hogar, all_data_hogar_b)
    all_data_persona$PB030A <- all_data_persona$PB030 %% 10
    all_data_hogar_cabeza <- subset(all_data_hogar, all_data_hogar$HB070 == all_data_hogar$HB080)
    all_data_persona_cabeza <- subset(all_data_persona, all_data_persona$PB030A == 1)
    i_result <- subset(all_data_hogar_cabeza, all_data_hogar_cabeza$HB070 %in% all_data_persona_cabeza$PB030)
    bb <- subset(all_data_persona_cabeza, all_data_persona_cabeza$PB030 %in% all_data_hogar_cabeza$HB070)
    filtered_data <- cbind(i_result, bb)
    filtered_data$PB040 <- filtered_data$PB040 %>% as.numeric()
    filtered_data$PY010N <- filtered_data$PY010N %>% as.numeric()
    filtered_data[is.na(filtered_data)] <- 0
    for (i in c(1:5)) {
        if (!(alt_rename[i] %in% colnames(filtered_data))) {
            filtered_data[alt_rename[i]] <- filtered_data[needed_rename[i]]
        }
    }
    # filtered_data <- filtered_data[needed_variables]
    filtered_data[filtered_data$PL031 %in% c(2, 5), "PL031"] <- 1
    filtered_data[filtered_data$PL031 %in% c(3, 4), "PL031"] <- 2
    filtered_data[filtered_data$PL040 %in% c(2), "PL031"] <- 3
    filtered_data[filtered_data$PL051 %in% c(1, 11, 12, 13, 14, 15, 16, 17, 18, 19), "PL031"] <- 4
    filtered_data[filtered_data$PL031 %in% c(7), "PL031"] <- 5
    filtered_data[filtered_data$PL031 %in% c(6, 8, 9, 10, 11), "PL031"] <- 6
    filtered_data[filtered_data$PL040 %in% c(3, 4), "PL040"] <- 2

    if (sel_year %in% c(2004:2010)) {
        filtered_data[filtered_data$HH021 %in% c(1, 2, 3), "HH021"] <- 0
        filtered_data[filtered_data$HH021 %in% c(4), "HH021"] <- 1
    }
    if (sel_year %in% c(2011:2020)) {
        filtered_data[filtered_data$HH021 %in% c(1, 2, 3, 4, 6), "HH021"] <- 0
        filtered_data[filtered_data$HH021 %in% c(5), "HH021"] <- 1
    }

    bb <- filtered_data$PB110 - filtered_data$PB140
    bb <- replace(bb, bb > 76, 6)
    bb <- replace(bb, bb > 66, 5)
    bb <- replace(bb, bb > 56, 4)
    bb <- replace(bb, bb > 46, 3)
    bb <- replace(bb, bb > 36, 2)
    bb <- replace(bb, bb > 7, 1)
    filtered_data$AGE <- bb
    filtered_data$PB040 <- as.numeric(filtered_data$PB040)
    filtered_data$PY035N <- as.numeric(filtered_data$PY035N)
    filtered_data$PY010N <- as.numeric(filtered_data$PY010N)
    filtered_data$HY040N <- as.numeric(filtered_data$HY040N)
    filtered_data$HY090G <- as.numeric(filtered_data$HY090G)
    filtered_data$HY100N <- as.numeric(filtered_data$HY100N)
    filtered_data$HY040N <- as.numeric(filtered_data$HY040N)
    filtered_data$PL031 <- factor(filtered_data$PL031, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "manager", "retired", "inactive"))
    filtered_data$AGE <- factor(filtered_data$AGE, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"))
    if (geogr == "MADRID") filtered_data <- subset(filtered_data, filtered_data$DB040 %in% 30)
    if (class_binary == T) filtered_data$PL031 <- filtered_data$PL040
    survey_weights <- as.svydesign2(svydesign(
        ids = ~1,
        data = data.frame(filtered_data),
        weights = ~ as.numeric(filtered_data$PB040)
    ))
    if (reduced == T) {
        survey_weights[survey_weights$variables == 0] <- NA
        filtered_data[filtered_data == 0] <- NA
    }
    survey_weights$variables[, "HY040N1"] <- I(survey_weights$variables[, "HY040N"] > 0)
    survey_weights$variables[, "HY090G1"] <- I(survey_weights$variables[, "HY090G"] > 0)
    survey_weights$variables[, "PY010N1"] <- I(survey_weights$variables[, "PY010N"] > 0)
    survey_weights$variables[, "PY035N1"] <- I(survey_weights$variables[, "PY035N"] > 0)
    survey_weights$variables[, "HH0211"] <- I(survey_weights$variables[, "HH021"] > 0)
    # source("src/filtersECV.R")
    ################3 TESTS 20-07-2023
    library(data.table)
    filtered_data <- data.table(filtered_data)
    fwrite(filtered_data, file= paste0("output/filtered_data",sel_year,".csv") , append = T)
}
