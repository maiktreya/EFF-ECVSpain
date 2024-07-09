### ESTIMATING PROPORTIONS OWNED BY QUANTILES USING CDF

library(survey)
corte <- svyquantile(~riquezafin, design = survey_weights_gini, quantiles = .9, na.rm = T)$riquezafin[, "quantile"]
subtotal <- svytotal(~riquezafin, design = subset(survey_weights_gini, riquezafin > corte), na.rm = T)[1]
total <- svytotal(~riquezafin, survey_weights_gini, na.rm = T)[1]
subtotal / total

corte <- svyquantile(~riquezafin, design = survey_weights_gini, quantiles = .99, na.rm = T)$riquezafin[, "quantile"]
subtotal <- svytotal(~riquezafin, design = subset(survey_weights_gini, riquezafin > corte), na.rm = T)[1]
total <- svytotal(~riquezafin, survey_weights_gini, na.rm = T)[1]
subtotal / total