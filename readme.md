# TASKS PENDING EFF

## REMAINING

Porcentaje de la población desposeída (sin activos financieros, sin financieros, sin reales) -> AÑADIR A LA TABLA 1
Liquid assets (current account + savings accounts) compare to evolution of Financial Assets
Calculate gini index for fin wealth
Age + Class (for pensions and financial wealth)

Tabla general EFF  - workers y total
Gráfico edad homeownership workers
Gráfico rental assets ECV workers

Cita respecto a concentración y centralización del capital referido a activos digitales (actuación de actores de mercado etc.)
% of homeowner households which increased level of indebtedness (for classes and total pop) -> PENDIENTE MURILLO



Disculpad, que llevo una semana ajetreada con muchas clases y otras cosas.
Me basé en los datos de la pregunta 2.26 del cuestionario.  "2.26 Usted me ha dicho que su vivienda le costo --------Euros y que su valor actual es de .............. Euros . ¿Este aumento en su patrimonio le ha llevado a usted a adquirir un nuevo endeudamiento en los dos últimos años?"
Hace referencia a las preguntas 2.4 y 2.5

Y por supuesto, enviadme el artículo cuanto lo tengáis, que me interesa.


![Alt text](/OUTPUT/PNG/table.png "Title")

### OLD COMMENTS IN FILE filters2.R

```{r}
survey_weights <- subset(survey_weights, nsitlabdom == 1)
survey_weights <- convey::convey_prep(survey_weights)
filters_gini <- convey::svygini(~riquezafin, survey_weights, na.rm = T
svyby(~valor, ~nsitlabdom, survey_weights, svymean, keep.var = F, na.rm = reduced)[2] %>% t() # nolin
--------------------------------------------------------------------------------------------------------#
--------------------------------------------------------------------------------------------------------#
 riquezabr=riquezabr+actreales+actfinanc #
 vdeuda= dvivpral + deuoprop+ phipo+ pperso+ potrasd + ptmos_tarj
 main_mean["actreales"] <- np2_5 + otraspr + dvivpral + deuoprop
--------------------------------------------------------------------------------------------------------#
--------------------------------------------------------------------------------------------------------#
 p2_23 == 1 alquilan habitacion full_mean[,"p2_23"]
 p3_2 == 10 hipoteca inversa
 p3_2a == 1 hipotecada inversamente la viv.princial full_mean[,"p3_2a_9"
main_mean["np2_5"] main_residence
main_mean["dvivpral"]
main_mean["otraspr"] other properties
main_mean["deuoprop"
survey_workers <- subset(survey_weights, nsitlabdom == 1)
quantiles_finwealth2014 <- svyquantile(~riquezafin, survey_workers, quantiles = seq(0.1, 1, 0.1), na.rm = T)
write.csv2(quantiles_finwealth2014$riquezafin, "quantiles_finwealth2014.csv")
```

### SELECTION B

- % HOMEOWNERSHIP (P2_35_1)
- N OF PROPS BY AGE AND CLASS (p2_33)
- MULTIPROP (s6_owner)
- RENTAL INCOME EFF (p7_2)
- PENSION FUNDS ASSETS (VALOR)
- FINANCIAL ASSETS (riquezafin)
- FINANCIAL ASSETS NO SAVINGS (riquezafin)
- MAIN RESIDENCE DEBT (dvivpral)
- DEBT RATIO (ratio_deuda)
- TOTAL DEBT (vdeuda)
- OVERDB (overburden)
- NET WORTH (riquezanet)
- OTHER REAL ASSETS DEBT (deuoprop)
- MAIN RESIDENCE VALUE (np2)
- OTHER REAL ASSETS VALUE (otraspr)
- HOUSING ASSETS (otraspr + np2)
- INHERITANCE (P2_2)
- MORTAGED (dvivpral)

#### COMPONENTES OF FINANCIAL ASSETS

- "p4_15" -> ACCIONES COTIZADAS EN BOLSA
- "p4_24" -> ACCIONES NO COTIZADAS EN BOLSA
- "p4_35" -> VALORES RENTA FIJA
- "allf"  -> FONDOS DE INVERSION
- "p4_43" -> CARTERAS GESTIONADAS (SOLO 2017)

### IMPLEMENTATIONS REMAINING

- categoria hipotecados (???)
- categoria desposeidos (to build)
- añadir activos no financieros (cuentas a la vista + cuentas ahorro)
