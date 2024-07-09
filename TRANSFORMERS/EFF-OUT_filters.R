## CONDITIONAL FILTERS ON SURVEY WITH SVYBY FUNCTION


tiene_viv_prop <- main_mean$np2_1
income_unit <- main_mean$renthog
wealth_unit <- main_mean$riquezanet
set_occupation_head <- factor(main_mean$nsitlabdom,
    levels = c(1:6),
    labels = c("empleado por cuenta ajena", "empleado por cuenta propia", "jubilado", "inactivo-parado", "autonomo", "directivo")
)
set_age_head <- factor(main_mean$bage,
    levels = c(1:6),
    labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75")
)
set_perc_riq <- factor(main_mean$percriq,
    levels = c(1, 2, 3, 4, 5),
    labels = c("0-20", "20-40", "40-60", "60-80", "90-100")
)
set_perc_ren <- factor(main_mean$percrent,
    levels = c(1, 2, 3, 4, 5),
    labels = c("0-20", "20-40", "40-60", "60-80", "90-100")
)
set_hasfixed_income <- factor(main_mean$np4_33,
    levels = c(0, 1),
    labels = c("Don't", "Has fixed income earnings")
)
set_mortage <- factor(main_mean$dpdtehipo,
    levels = c(0, 1),
    labels = c("Don't", "Has mortage debt")
)
set_multihouse <- factor(main_mean$np2_32,
    levels = c(0, 1),
    labels = c("Don't", "Has a second real state property")
)
set_via_property <- factor(main_mean$p2_35_1,
    levels = c(1, 2, 3),
    labels = c("compra", "herencia", "regalo-donacion")
)
set_use_property <- factor(main_mean$p2_42_1,
    levels = c(1, 2, 3, 4, 5, 6, 7),
    labels = c("agricola", "vacaciones", "uso profesional", "cesion", "alquiler", "desocupada", "futura")
)
set_landlord_rent_val <- main_mean$p7_2
set_landlord_rent_val[is.na(set_landlord_rent_val)] <- 0
set_landlord_rent <- I(main_mean$p7_2 > 0)
set_landlord_rent[is.na(set_landlord_rent)] <- 0
set_landlord_rent <- factor(set_landlord_rent,
    levels = c(0, 1),
    labels = c("Don't", "Has landlord income")
)

kind_use_property <- cbind(
    c("agricola", "vacaciones", "uso profesional", "cesion", "alquiler", "desocupada", "futura"),
    data.frame(svymean(~set_use_property, survey_weights,  na.rm = reduced))[, 1]
) %>% t()
kind_via_property <- cbind(
    c("compra", "herencia", "regalo-donacion"),
    data.frame(svymean(~set_via_property, survey_weights,  na.rm = reduced))[, 1]
) %>% t()
mortage <- svyby(~renthog, ~set_mortage, survey_weights, svymean, keep.names = FALSE, keep.var = FALSE, na.rm = reduced) %>%
    t()

multihouse <- svyby(~renthog, ~set_multihouse, survey_weights, svymean, keep.names = FALSE, keep.var = FALSE, na.rm = reduced) %>%
    t()
via_property <- svyby(~renthog, ~set_via_property, survey_weights, svymean, keep.names = FALSE, keep.var = FALSE, na.rm = reduced) %>%
    t()
use_property <- svyby(~renthog, ~set_use_property, survey_weights, svymean, keep.names = FALSE, keep.var = FALSE, na.rm = reduced) %>%
    t()
landlord_rent <- svyby(~renthog, ~set_landlord_rent, survey_weights, svymean, keep.names = FALSE, keep.var = FALSE, na.rm = reduced) %>%
    t()
landlord_q_wealth <- svyby(~p7_2, ~set_perc_riq, survey_weights, svymean, keep.names = FALSE, keep.var = FALSE, na.rm = reduced) %>%
    t()
landlord_q_income <- svyby(~p7_2, ~set_perc_ren, survey_weights, svymean, keep.names = FALSE, keep.var = FALSE, na.rm = reduced) %>%
    t()

wealth_filters_year <- t(cbind(
    mortage,
    multihouse,
    via_property,
    use_property,
    landlord_rent,
    kind_via_property,
    kind_use_property,
    landlord_q_wealth,
    landlord_q_income
))
wealth_filters <- rbind(wealth_filters, wealth_filters_year)
