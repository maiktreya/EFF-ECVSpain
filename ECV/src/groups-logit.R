
survey_cabezas <- subset(survey_total, HB070 == HB080)
survey_jovenes_cabezas <- subset(survey_total, PB140 %in% c(1990:2003) & HB070 == HB080)
survey_jovenes <- subset(survey_total, PB140 %in% c(1990:2003))
survey_varon <- subset(survey_total, varon == 1)
survey_mujer <- subset(survey_total, mujer == 1)

# HH021 = 5 cesion, PE030 año nacimiento
total <- svytable(~HH021, survey_total) %>% prop.table()
cabezas <- svytable(~HH021, survey_cabezas) %>% prop.table()
jovenes <- svytable(~HH021, survey_jovenes) %>% prop.table()
jovenes_cabezas <- svytable(~HH021, survey_jovenes_cabezas) %>% prop.table()
varon <- svytable(~HH021, survey_varon) %>% prop.table()
mujer <- svytable(~HH021, survey_mujer) %>% prop.table()

### PRTNT RESULTS BASED ON ALTERNATIVE SUBSETS AND COMBINE CESION AND SOCIAL RENT
final_table <- rbind(total, cabezas, jovenes, jovenes_cabezas, varon, mujer) %>% data.table()
colnames(final_table) <- c("prop1", "prop2", "alq1", "alq2", "ces")
final_table[, total := prop1 + prop2 + alq1 + alq2 + ces][, new_ces := alq2 + ces][, prop := prop1 + prop2]
final_table <- final_table[, .(prop, alq1, alq2, total, new_ces)] %>%
    t() %>%
    data.table() %>%
    round(., 3)
colnames(final_table) <- c("total", "cabezas", "jovenes", "jovenes_cabezas", "varon", "mujer")
final_table <- final_table[, .(vars = c("prop1", "alq1", "alq2", "total", "new_ces"), total, varon, mujer, jovenes)]
print(final_table)

#### CHECK SIZES OF SUBSETS
survey_total$variables %>%
    nrow() %>%
    print()
survey_cabezas$variables %>%
    nrow() %>%
    print()
survey_jovenes_cabezas$variables %>%
    nrow() %>%
    print()
survey_jovenes$variables %>%
    nrow() %>%
    print()



#####################################
survey_jovenes$variables["PE030"] %>% unique()


# sel_year <- 2020
# all_data_persona <- paste0("data/esudb", substr(sel_year, 3, 4), "p.csv") %>% fread()
# all_data_hogar <- paste0("data/esudb", substr(sel_year, 3, 4), "h.csv") %>% fread()
# all_data_hogar_b <- paste0("data/esudb", substr(sel_year, 3, 4), "d.csv") %>% fread()
# all_data_hogar_b$DB040 <- all_data_hogar_b$DB040 %>%
#    substr(3, 4) %>%
#    as.numeric()
# all_data_hogar <- cbind(all_data_hogar, all_data_hogar_b)
# all_data_persona$PB030A <- all_data_persona$PB030 %% 10
# all_data_hogar_cabeza <- subset(all_data_hogar, all_data_hogar$HB070 == all_data_hogar$HB080)
# all_data_persona_cabeza <- subset(all_data_persona, all_data_persona$PB030A == 1)
# i_result <- subset(all_data_hogar_cabeza, all_data_hogar_cabeza$HB070 %in% all_data_persona_cabeza$PB030)
# bb <- subset(all_data_persona_cabeza, all_data_persona_cabeza$PB030 %in% all_data_hogar_cabeza$HB070)
# filtered_data <- cbind(i_result, bb)
# filtered_data$PB040 <- filtered_data$PB040 %>% as.numeric()
#
# survey_ecv <- as.data.frame(filtered_data)
#
# survey_weights <- as.svydesign2(svydesign(
#    ids = ~1,
#    data = survey_ecv,
#    weights = ~ survey_ecv$PB040
# ))
#
#
#
# svymean(~HH021, subset(survey_weights, PE030 %in% c(1990:2000)))
#
# svytable(~HH021, subset(survey_weights, PE030 %in% c(1989:2000))) %>% prop.table()
