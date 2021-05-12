no_of_factors<-function(x) {
  ifelse(is.factor(x),length(unique(x)),0)
}

pred_table_code<- withProgress(message = "Preparing final table",
                                      detail = "Exposure at Risk", value = 0,
                                      {
                                        setProgress(value = 1, message = "Formatting")
                                        df<-dataset_with_eco()%>%
                                          dplyr::select(!where(is.Date))
                                        df$asset_type<-as.factor(df$asset_type)
                                        df$supplier<-as.factor(df$supplier)
                                        df$customer_type<-as.factor(df$customer_type)
                                        
                                        setProgress(value = 2, message = "Running calculations")
                                        z<-
                                          df%>%
                                          group_by(id)%>%
                                          # Keeping the latest transaction per id
                                          slice_max(age_of_asset_months,n=1)%>%
                                          ungroup()%>%
                                          # Storing asset age in separate column for future use
                                          mutate(a_in_mon=age_of_asset_months)%>%
                                          # Calculate risks from current to next 5 years and till the end
                                          mutate(risk_current=1-exp(-predict(final.model, ., type="expected", collapse = id)))%>%
                                          mutate(age_of_asset_months=ifelse(age_of_asset_months<loan_tenure_months, age_of_asset_months+12,0))%>%
                                          mutate(risk_1yr=1-exp(-predict(final.model, ., type="expected", collapse = id))) %>% 
                                          mutate(age_of_asset_months=ifelse(age_of_asset_months<loan_tenure_months, age_of_asset_months+24,0))%>%
                                          mutate(risk_2yr=1-exp(-predict(final.model, ., type="expected", collapse = id))) %>% 
                                          mutate(age_of_asset_months=ifelse(age_of_asset_months<loan_tenure_months, age_of_asset_months+36,0))%>%
                                          mutate(risk_3yr=1-exp(-predict(final.model, ., type="expected", collapse = id))) %>% 
                                          mutate(age_of_asset_months=ifelse(age_of_asset_months<loan_tenure_months, age_of_asset_months+48,0))%>%
                                          mutate(risk_4yr=1-exp(-predict(final.model, ., type="expected", collapse = id))) %>% 
                                          mutate(age_of_asset_months=ifelse(age_of_asset_months<loan_tenure_months, age_of_asset_months+60,0))%>%
                                          mutate(risk_5yr=1-exp(-predict(final.model, ., type="expected", collapse = id))) %>%
                                          mutate(age_of_asset_months=ifelse(age_of_asset_months<loan_tenure_months, loan_tenure_months,0))%>%
                                          mutate(risk_till_end=1-exp(-predict(final.model, ., type="expected", collapse = id)))%>%
                                          group_by(id)%>%
                                          tidyr::pivot_longer(cols = c("risk_current", "risk_1yr", "risk_2yr", "risk_3yr", "risk_4yr", 
                                                                       "risk_5yr", "risk_till_end"))%>%
                                          # Calculating EMI (Monthly fixed installments)
                                          mutate(emi=unique(balance)/(unique(loan_tenure_months)-unique(a_in_mon)))%>%
                                          mutate(emi=cumsum(emi))%>%
                                          # Calculating future balance
                                          mutate(balance=ifelse(emi==max(emi),0,balance-emi))%>%
                                          mutate(balance=ifelse(balance<=0,0,balance))%>%
                                          # Calculating exposure as balance at the time of interest * risk at that time
                                          mutate(exposure=round(balance*value,2))%>%
                                          filter(emi>0)
                                        setProgress(value = 3, message = "Done")
                                      })