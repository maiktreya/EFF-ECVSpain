##### VARIABLE RIQUEZAFIN REDEFINIDA
set <- full_mean
set[is.na(set)] <- 0
if (sel_year %in% c(2002, 2005, 2008, 2011, 2014)) {
   full_mean$riquezafin2 <- full_mean$riquezafin
} else {
   full_mean$riquezafin2 <- set$p4_15 + set$p4_24 + set$p4_35 + set$allf + set$p4_43
}

## DEPOSSED POPULATION
full_mean[, deposed := 0][!(actreales > 0 | riquezafin > 0), deposed := 1]
full_mean[, deposed2 := 0][actreales > 0, deposed2 := 1][riquezafin > 0, deposed2 := 1]

## INCREASED INDEBTEDNESS
full_mean[p2_26 != 1, p2_26 := 0]

### GROSS AND NET HOUSING ASSETS
full_mean$gross_hous_ass <- set$np2_5 + set$otraspr
full_mean$net_hous_ass <- set$np2_5 + set$otraspr - set$dvivpral - set$deuoprop
full_mean$riquezaliq <- set$p4_7_3 + set$salcuentas

### DEBT RATIOS
ratio_deud1 <- set$pagopr_vivp * 12 / set$renthog
ratio_deud2 <- set$pagodeuda * 12 / set$renthog

### OVERBURDEN
over <- ratio_deud1
over[over >= 0.4] <- 1
over[over < 0.4] <- 0
over[which(!is.finite(over))] <- 0
if (reduced == T) over <- over + 1

# NEW MULTIPROPERTY
full_mean[, main_other := 0][np2_1 == 1 & s6_owner, main_other := 1]

### REINCORPORATE VARIABLES TO ORIGINAL TABLE
full_mean$overburden <- over
full_mean$ratio_deuda <- ratio_deud2
