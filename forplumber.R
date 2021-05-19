
# function to select model
model_sel <- function(dff) {
  
  withProgress(message = "Testing",
               detail = "your patience!",
               value = 0,
               {
                 setProgress(value = 1, message = "Formatting Data")
                 
                 df <- dff %>%
                   dplyr::select(!where(is.Date))
                 # Removing columns that have only one value
                 df <- df %>%
                   select(-names(which(apply(
                     df, 2, lenun
                   ) == 1)))
                 
                 df$asset_type <- as.factor(df$asset_type)
                 #df$supplier <- as.factor(df$supplier)
                 df$customer_type <-
                   as.factor(df$customer_type)
                 
                 setProgress(value = 2, message = "Creating initial formula")
                 
                 # some cleaning
                 variables <-
                   colnames(df)[!colnames(df) %in% c("id", "age_of_asset_months", "loan_status")]
                 form <-
                   as.formula(paste0(
                     "Surv(age_of_asset_months, loan_status) ~",
                     paste(variables, collapse = "+")
                   ))
                 surv.res <- coxph(form, data = df, id = id)
                 res <-
                   as.data.frame(summary(surv.res)$coefficients)
                 selvars <-
                   res %>%
                   filter(is.finite(`Pr(>|z|)`)) %>%
                   filter(is.finite(z)) %>%
                   rownames()
                 for (j in 1:length(variables)) {
                   selvars[grep(pattern = variables[j], selvars)] <- variables[j]
                 }
                 selvars <- unique(selvars)
                 
                 form <-
                   as.formula(paste0(
                     "Surv(age_of_asset_months, loan_status) ~",
                     paste(selvars, collapse = "+")
                   ))
                 setProgress(value = 3, message = "Are you still not annoyed?")
                 
                 scores <- rep(NA, 5)
                 variables <- selvars
                 a <- length(variables)
                 f <- vector(mode = "list", length = a)
                 scores = vector(length = a)
                 setProgress(value = 4, message = "No way! Are you still waiting?")
                 for (i in 1:a) {
                   v <- variables[i:a]
                   #n<-paste0("form",i)
                   f[[i]] <-
                     coxph(
                       as.formula(
                         paste0(
                           "Surv(age_of_asset_months, loan_status) ~",
                           paste(v, collapse = "+")
                         )
                       ),
                       data = df,
                       id = id,
                       x = T,
                       y = T
                     )
                   perror <-
                     pec(
                       object = f[[i]],
                       formula = form,
                       splitMethod = "cvK5",
                       data = df
                     )
                   scores[i] <-
                     1 - ibs(perror)["coxph",] / ibs(perror)["Reference",]
                 }
                 setProgress(value = 5, message = "Ok. I give up. You win!")
                 final.model <-
                   f[[which(scores == max(scores, na.rm = T))]]
                 setProgress(value = 6, message = "Ha ha! Not so fast!")
               })
  
  final.model
  
}


# function to creadte the predicted table

predic_t<- function(dff,gdpfor,prfor,maxdate, final.model) {
  
  withProgress(message = "Still working",
               detail = "Yes, I'm slow :(",
               value = 0,
               {
                 setProgress(value = 1, message = "Formatting")
                 df <- dff %>%
                   dplyr::select(!where(is.Date))
                 df$asset_type <- as.factor(df$asset_type)

                 df$customer_type <-
                   as.factor(df$customer_type)

                 gdp.forecast <-
                   as.data.frame(gdpfor) %>%
                   mutate(qtr = rownames(.)) %>%
                   mutate(stringr::str_replace(qtr, " ", "-"))
                 pats.forecast <-
                   as.data.frame(prfor) %>%
                   mutate(qtr = rownames(.)) %>%
                   mutate(stringr::str_replace(qtr, " ", "-"))
                 setProgress(value = 2, message = "But need to run some calculations")
                 z <-
                   df %>%
                   group_by(id) %>%
                   slice_max(age_of_asset_months, n = 1) %>%
                   ungroup() %>%
                   #emi = loan balance by number of months left (may not always be the case)
                   mutate(emi = balance / (loan_tenure_months -
                                             age_of_asset_months)) %>%
                   mutate(balance.original = balance) %>%
                   mutate(age.original = age_of_asset_months) %>%
                   mutate(risk_current = 1 - exp(
                     -predict(
                       final.model,
                       .,
                       type = "expected",
                       collapse = id
                     )
                   )) %>%
                   mutate(balance = balance - emi * 12) %>%
                   mutate(age_of_asset_months = age_of_asset_months +
                            12) %>%
                   # lag of macroeconomic forecasts used because effect of economy should reflect later in loan performance
                   mutate(
                     gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(11)),]$`Point Forecast`,
                     prices_lag = pats.forecast[pats.forecast$qtr ==
                                                  zoo::as.yearqtr(maxdate + months(11)),]$`Point Forecast`
                   ) %>%
                   mutate(risk_1yr = 1 - exp(
                     -predict(
                       final.model,
                       .,
                       type = "expected",
                       collapse = id
                     )
                   )) %>%
                   mutate(balance = balance - emi * 12) %>%
                   mutate(age_of_asset_months = age_of_asset_months +
                            12) %>%
                   mutate(
                     gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(23)),]$`Point Forecast`,
                     prices_lag = pats.forecast[pats.forecast$qtr ==
                                                  zoo::as.yearqtr(maxdate + months(23)),]$`Point Forecast`
                   ) %>%
                   mutate(risk_2yr = 1 - exp(
                     -predict(
                       final.model,
                       .,
                       type = "expected",
                       collapse = id
                     )
                   )) %>%
                   mutate(balance = balance - emi * 12) %>%
                   mutate(age_of_asset_months = age_of_asset_months +
                            12) %>%
                   mutate(
                     gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(35)),]$`Point Forecast`,
                     prices_lag = pats.forecast[pats.forecast$qtr ==
                                                  zoo::as.yearqtr(maxdate + months(35)),]$`Point Forecast`
                   ) %>%
                   mutate(risk_3yr = 1 - exp(
                     -predict(
                       final.model,
                       .,
                       type = "expected",
                       collapse = id
                     )
                   )) %>%
                   mutate(balance = balance - emi * 12) %>%
                   mutate(age_of_asset_months = age_of_asset_months +
                            12) %>%
                   mutate(
                     gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(47)),]$`Point Forecast`,
                     prices_lag = pats.forecast[pats.forecast$qtr ==
                                                  zoo::as.yearqtr(maxdate + months(47)),]$`Point Forecast`
                   ) %>%
                   mutate(risk_4yr = 1 - exp(
                     -predict(
                       final.model,
                       .,
                       type = "expected",
                       collapse = id
                     )
                   )) %>%
                   mutate(balance = balance - emi * 12) %>%
                   mutate(age_of_asset_months = age_of_asset_months +
                            12) %>%
                   mutate(
                     gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(59)),]$`Point Forecast`,
                     prices_lag = pats.forecast[pats.forecast$qtr ==
                                                  zoo::as.yearqtr(maxdate + months(59)),]$`Point Forecast`
                   ) %>%
                   mutate(risk_5yr = 1 - exp(
                     -predict(
                       final.model,
                       .,
                       type = "expected",
                       collapse = id
                     )
                   )) %>%
                   group_by(id) %>%
                   tidyr::pivot_longer(
                     cols = c(
                       "risk_current",
                       "risk_1yr",
                       "risk_2yr",
                       "risk_3yr",
                       "risk_4yr",
                       "risk_5yr"
                     )
                   ) %>%
                   mutate(r_n = row_number()) %>%
                   mutate(t.emi = emi + emi * 12 * (r_n - 1)) %>%
                   mutate(balance = ifelse(t.emi == max(emi), balance, balance -
                                             t.emi)) %>%
                   mutate(balance = ifelse(balance <= 0, 0, balance)) %>%
                   filter(t.emi > 0)
                 setProgress(value = 3, message = "At last. Now wait for the plot please.")
               })
  z
  
}
  
# function to process simulated data

# supporting function

sim_fin_supp<-function(simdata, name.fil="risk_current"){
  simdata %>%
    filter(name == name.fil) %>%
    ungroup() %>%
    select(matches("sim?")) %>%
    colSums()
}

#main function

sim_fin<- function(simdata){
  dtc <- sim_fin_supp(simdata,"risk_current")
  
  dt1 <- sim_fin_supp(simdata,"risk_1yr")
  
  dt2 <- sim_fin_supp(simdata,"risk_2yr")
  
  dt3 <- sim_fin_supp(simdata,"risk_3yr")
  
  dt4 <- sim_fin_supp(simdata,"risk_4yr")
  
  dt5 <- sim_fin_supp(simdata,"risk_5yr")
  
  # creating density
  hisc <- density(dtc)
  his1 <- density(dt1)
  his2 <- density(dt2)
  his3 <- density(dt3)
  his4 <- density(dt4)
  his5 <- density(dt5)
  
  # data frame from density
  dtc <- data.frame(x = hisc$x, y = hisc$y)
  dtc$r <- rep("risk_0yr", times = nrow(dtc))
  dt1 <- data.frame(x = his1$x, y = his1$y)
  dt1$r <- rep("risk_1yr", times = nrow(dt1))
  dt2 <- data.frame(x = his2$x, y = his2$y)
  dt2$r <- rep("risk_2yr", times = nrow(dt2))
  dt3 <- data.frame(x = his3$x, y = his3$y)
  dt3$r <- rep("risk_3yr", times = nrow(dt3))
  dt4 <- data.frame(x = his4$x, y = his4$y)
  dt4$r <- rep("risk_4yr", times = nrow(dt4))
  dt5 <- data.frame(x = his5$x, y = his5$y)
  dt5$r <- rep("risk_5yr", times = nrow(dt5))
  
  df <- rbind(dtc, dt1, dt2, dt3, dt4, dt5)
  return(df)
}



