 #
    ft1 <- as.data.frame(svyby(~HY040N, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[-1] # RENTAL ALQ. VS CLASE
    ft2 <- as.data.frame(svyby(~HY040N, ~AGE, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[-1] # RENTAL ALQ. VS AGE
    ft3 <- as.data.frame(svyby(~HY090G, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[-1] # K RENT VS CLASE
    ft4 <- as.data.frame(svyby(~HY090G, ~AGE, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[-1] # K RENT VS AGE
    ft5 <- as.data.frame(svyby(~PY035N, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[-1] # PENSIONES VS CLASE
    ft6 <- as.data.frame(svyby(~PY035N, ~AGE, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[-1] # PENSIONES VS AGE
    ft7 <- as.data.frame(svyby(~HY040N1, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # RENTAL ALQ.%
    ft8 <- as.data.frame(svyby(~HY090G1, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # K RENT %
    ft9 <- as.data.frame(svyby(~PY035N1, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # PENSIONES %
    ft10 <- as.data.frame(svyby(~HH0211, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # TENANCY %
    ft11 <- as.data.frame(svyby(~HY040N1, ~AGE, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # RENTAL ALQ.%
    ft12 <- as.data.frame(svyby(~HY090G1, ~AGE, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # K RENT %
    ft13 <- as.data.frame(svyby(~PY035N1, ~AGE, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # PENSIONES %
    ft14 <- as.data.frame(svyby(~HH0211, ~AGE, subset(survey_weights, PL031 == "worker"), svymean, keep.names = F, keep.var = F, na.rm = mode))[c(-1, -2)] # TENANCY %
    #ft15 <- as.data.frame(svyby(~HH021, ~PL031, survey_weights, svymean, keep.names = F, keep.var = F, na.rm = mode))[-1][c(-1, -2)] # RENTAL ALQ. VS REGIMEN DE TENENCIA
    ft15 <- svymean(~HH021, survey_weights)[1]
    ft_year <- cbind.fill(ft1, ft2, ft3, ft4, ft5, ft6, ft7, ft8, ft9, ft10, ft11, ft12, ft13, ft14, ft15)
    ft_year <- cbind(ft_year, YEAR = rep(sel_year, 6))
    ft_results <- rbind(ft_results, ft_year)

### PREPARE AND EXPORT RESULTS
if (geogr == "MADRID") {
    write.csv(ft_results, paste0("filters.MADRID.ECV.csv"))
} else {
    write.csv(ft_results, paste0("filters.ECV.csv"))
}