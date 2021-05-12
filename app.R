# Estimate exposure at risk and create report (IFRS)
# Application developed by Asitav Sen

library(shiny)
library(promises)
library(future)
library(dplyr)
library(ggplot2)
library(lubridate)
library(survival)
library(pec)
library(DT)
library(shinycssloaders)
library(imfr)
library(patchwork)

plan(multisession)

source("helpers.R")
source("mod_basicstat.R")

# Define UI for application that draws a histogram
ui <- 
    navbarPage("easyIFRS",
               apptab,
               tabPanel("About")
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    # Data
    ndt<-read.csv("./data/transactions.csv")
    
    collateral<-read.csv("./data/collateral.csv")
    
    countries<-read.csv("./data/countries.csv")
    
    new.data<- reactive({
        
        withProgress(message = "Trying to read",
                     detail = "Hope the handwriting is legible!", value = 0,
                     {
                         setProgress(value = 1, message = "Patience test 1 of 2")
                         ndt<-
                             ndt%>%
                             # Change format of columns mentioned as date 
                             mutate(
                                 origination_date=ymd(origination_date),
                                 maturity_date=ymd(maturity_date),
                                 report_date=ymd(report_date)
                             )%>%
                             # Add age of loan, loan tenure in months, which could be important parameters
                             mutate(age_of_asset_months=round(as.numeric(report_date-origination_date)/30))%>%
                             mutate(loan_tenure_months=round(as.numeric(maturity_date-origination_date)/30))%>%
                             group_by(id)%>%
                             # Arranging to avoid mistakes in lag
                             arrange(report_date)%>%
                             # Add lag of bureau score and total number of defaults. Lag added for delta creation
                             mutate(cum_default=cumsum(default_flag),
                                    bureau_score_lag=ifelse(is.na(lag(bureau_score_orig,1)),
                                                            bureau_score_orig,lag(bureau_score_orig,1)))%>%
                             # Adding delta of bureau score
                             mutate(bureau_score_delta=bureau_score_lag-bureau_score_orig)%>%
                             # Adding quarter info for matching later with macroeconomic data
                             mutate(qtr = paste0(year(report_date),"-Q",quarter(report_date)))%>%
                             # Removing dummy
                             dplyr::select(-bureau_score_orig)
                         setProgress(value = 2, message = "Patience test 1 of 2")
                     })
        ndt
    })
    
    # Show uploaded data
    output$up_data<-renderDataTable({
        datatable(
            new.data(),
            options = list(
                pageLength = 5,
                scrollX = TRUE,
                filter = list(position = 'top', clear = FALSE),
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
        )
    })
    
    # Show some stats
    
    basicstatServer("nofloans", dt=new.data())
    
    # Adding macroeconomic data. Currently GDP and prices data are extracted
    dataset_with_eco<- reactive({
        input$fetchimf
        req(new.data(),input$fetchimf)
        
        # Defining database and other parameters for query of macroeconomic data
        databaseID <- "IFS"
        startdate = min(new.data()$report_date)
        enddate = max(new.data()$report_date)
        country=countries[countries$Country==input$country,]$Alpha.2.code
        withProgress(message = "Trying Hard",
                     detail = "Hang on", value = 0,
                     {
                         setProgress(value = 1, message = "working..")
            imf.data<-imf_data(
                databaseID,
                c("NGDP_R_K_IX", 
                  "PCPI_IX"),
                country = "US",
                start = startdate,
                end = enddate,
                freq = "Q",
                return_raw = FALSE,
                print_url = FALSE,
                times = 3
            )
            
            # New dataset by joining GDP and prices data
            dataset_with_eco<-
                new.data()%>%
                left_join(imf.data, by=c("qtr"="year_quarter")) %>% 
                rename(gdp=NGDP_R_K_IX, prices=PCPI_IX)%>%
                select(-iso2c) %>% 
                group_by(id)%>%
                mutate(gdp_lag=lag(gdp,1), prices_lag=lag(prices,1))%>%
                dplyr::select(-qtr)%>%
                dplyr::select(-c(gdp,prices))
            
            # removing rows with no macroeconomic data
            dataset_eco<-dataset_with_eco[!is.na(dataset_with_eco$gdp_lag) & !is.na(dataset_with_eco$prices_lag), ]
                         setProgress(value = 2, message = "working..")
                         })

        return(dataset_eco) 
    })
    
    # Show full data
    
    output$fulldata<-renderDataTable({
        validate(
            need(
                !is.null(dataset_with_eco()),
                message = "Data Not ready yet"
            )
        )
        withProgress(message = "Working",
                     detail = "trying to add macroeconomy data", value = 0,
                     {
                         setProgress(value = 1, message = "Working")
                         a<-dataset_with_eco()
                         setProgress(value = 2, message = "Almost done")
                     })
        datatable(
            a,
            options = list(
                pageLength = 5,
                scrollX = TRUE,
                filter = list(position = 'top', clear = FALSE),
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
        )
    }
    )
    
    # Action button to show when data is available
    
    output$expcalcu<-renderUI({
        req(!is.null(dataset_with_eco()), input$fetchimf)
        fluidRow(
            h3("Probability of asset going bad"),
            actionButton("start_model_selection","Initiate Model Selection"),
            withSpinner(plotOutput("probability_default"), type=7, color="black")
        )
    })
    
        
    
    # Model Selection
    selected_model<-reactive({
        req(input$start_model_selection)
        withProgress(message = "Model selection",
                     detail = "working..", value = 0,
                     {
                         setProgress(value = 1, message = "Formatting Data")
                         
                         df<-dataset_with_eco()%>%
                             dplyr::select(!where(is.Date))
                         
                         df$asset_type<-as.factor(df$asset_type)
                         df$supplier<-as.factor(df$supplier)
                         df$customer_type<-as.factor(df$customer_type)
                         
                         setProgress(value = 2, message = "Creating initial formula")
                         
                         variables<-colnames(df)[!colnames(df) %in% c("id","age_of_asset_months","loan_status", "cum_default")]
                         form<-as.formula(paste0("Surv(age_of_asset_months, loan_status) ~",paste(variables,collapse = "+")))
                         
                         setProgress(value = 3, message = "Heavy operation. May take few minutes.")
                         
                         forms<-vector(mode = "list", length = 5)
                         models<-vector(mode = "list", length = 5)
                         newform<-NULL
                         scores<-rep(NA,5)
                             for(i in 1:5){
                                 # Sampling
                                 train=sample(c(TRUE,FALSE), nrow(df),rep=TRUE, prob=c(0.9,0.1))
                                 test =(! train )
                                 # Fit survival (Coxph) model
                                 surv.res<-coxph(form, data = df[train,], id=id)
                                 # Store result
                                 res<-as.data.frame(summary(surv.res)$coefficients)
                                 # Select variables based on p value
                                 selvars<-
                                     res%>%
                                     filter(!is.na(`Pr(>|z|)`)&`Pr(>|z|)`<0.05)%>%
                                     rownames()
                                 # Renaming factor variables to original variable name
                                 for(j in 1:length(variables)) {
                                     selvars[grep(pattern = variables[j],selvars)]<-variables[j]
                                 }
                                 # Select the new set of variable
                                 selvars<-unique(selvars)
                                 # Create new formula from new set of variables
                                 newform<- as.formula(paste0("Surv(age_of_asset_months, loan_status) ~",paste(selvars,collapse = "+")))
                                 # Result of new fit
                                 new.surv.res<-coxph(newform, data = df[train,], id=id, x=T)
                                 # Store Evaluation results, forms, models
                                 perror<-pec(object = new.surv.res,formula = newform, splitMethod = "cv5", data=df)
                                 
                                 forms[[i]]<-newform
                                 models[[i]]<-new.surv.res
                                 scores[i]<-ibs(perror)["coxph", "crossvalErr"]
                             }
                             # Select the model with min error
                             final.model<-models[[which(scores==min(scores, na.rm = T))]]
                         setProgress(value = 4, message = "Done")
                     })
   
        final.model
    })
    
    # Preparing tables with predictions
    predicted_table<-reactive({
        
        input$start_model_selection
        req(!is.null(selected_model()))
        
        final.model<- selected_model()
        withProgress(message = "Preparing final table",
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
                             slice_max(age_of_asset_months,n=1)%>%
                             ungroup()%>%
                             mutate(a_in_mon=age_of_asset_months)%>%
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
                             mutate(emi=unique(balance)/(unique(loan_tenure_months)-unique(a_in_mon)))%>%
                             mutate(r_n=row_number())%>%
                             mutate(t.emi=emi+emi*12*(r_n-1))%>%
                             mutate(balance=ifelse(t.emi==max(emi),balance,balance-t.emi))%>%
                             mutate(balance=ifelse(balance<=0,0,balance))%>%
                             filter(t.emi>0)
                         setProgress(value = 3, message = "Done")
                     })
        z

    })

    
    # Plot Probability of default
    output$probability_default<- renderPlot({
        input$start_model_selection
        req(predicted_table())
        predicted_table()%>%
            group_by(name)%>%
            #summarise(pd.me=median(value), pd.min=quantile(value,0.05), pd.max=quantile(value, 0.95))%>%
            slice_head(n=6)%>%
            mutate(name=factor(name, levels = c("risk_current", "risk_1yr", "risk_2yr", "risk_3yr", "risk_4yr", 
                                                "risk_5yr")))%>%
            filter(!is.na(name))%>%
            ggplot(aes(x=name, y=value, fill=name))+
            geom_violin()+
            geom_boxplot(width=0.1, color="black", alpha=0.2)+
            labs(x="",
                 y="Probability",
                 title = "Probability of default")+
            theme_bw()+
            theme(legend.title = element_blank(), legend.position = "bottom")
    })
    
    # Show scenario creation options when exposures and risks are calculated
    
    output$scenario_opts<-renderUI({
        req(!is.null(predicted_table()), input$start_model_selection)
        fluidRow(
            br(),
            h3("Possible Scenarios"),
            br(),
            column(
                width = 3,
                sliderInput("discount_rate","Select Discount rate / WACC", min=0.00, max=20.00, value = 4.25,
                            step=0.25),
                br(),
                h3("Select expected change in collateral value"),
                p("-ve means reduction in value"),
                br(),
                sliderInput("mode_dep","Most Probable", min=-0.5, max=0.5, value = -0.3,
                            step=0.1),
                sliderInput("min_dep","Minimum", min=-0.5, max=0.5, value = -1,
                            step=0.1),
                sliderInput("max_dep","Most Probable", min=-0.5, max=0.5, value = 0,
                            step=0.1),
                actionButton("update", "Update")
            ),
            column(
                width = 9,
                plotOutput("exposure_on_default")
            )
        )
    })
    
    # Exposure on Default
    
    output$exposure_on_default<- renderPlot({
        input$update
        req(!is.null(predicted_table()))
        
            predicted_table()%>%
            filter(name!="risk_till_end")%>%
            mutate(pv.balance=balance/(1+discount_rate_pa)^(r_n-1))%>%
            mutate(exposure_on_default=pv.balance*value)%>%
            group_by(name)%>%
            summarise(pv.balance=sum(pv.balance), exposure_on_default=sum(exposure_on_default))%>%
            mutate(name=factor(name, levels = c("risk_current", "risk_1yr", "risk_2yr", "risk_3yr", "risk_4yr", 
                                                "risk_5yr")))%>%
            pivot_longer(cols=c("pv.balance","exposure_on_default"), names_to="type", values_to="amount")%>%
            ggplot(aes(x=name, y=amount/1000000, fill=type, label=paste0(round(amount/1000000)," M")))+
            geom_col(position = "dodge")+
            geom_text(aes(y=(amount/1000000)+5), position = position_dodge(width = 1))+
            labs(x="",
                 y="Amount",
                 title = "Exposure on default",
                 subtitle = "Amounts discounted")+
            theme_bw()+
            theme(legend.title = element_blank(), legend.position = "bottom")
    })
    
    output$credit_loss<-renderUI({
        req(!is.null(predicted_table()), input$update)
        fluidRow(
            h3("Simulated Credit Loss with probability"),
            br(),
            selectInput("riskperiod","Select time", choices = c("risk_current", "risk_1yr", "risk_2yr", "risk_3yr", "risk_4yr", 
                                                                "risk_5yr"),
                        selected = "risk_1yr"),
            br(),
            plotOutput("simres",
                       brush = brushOpts(id = "sim_res_sel"))
            
        )
    })
    
    
    
    simdata<-reactive({
        
        input$update
        
        zz<-
            predicted_table()%>%
            ungroup()%>%
            filter(name!="risk_till_end")%>%
            group_by(id)%>%
            mutate(pv.balance=balance/(1+discount_rate_pa)^(r_n-1))%>%
            left_join(collateral, by="id")%>%
            ungroup()%>%
            select(name,pv.balance, value, collateral)
            
        sim.t<-data.frame(matrix(ncol=1000,nrow=nrow(zz1)))
        colnames(sim.t)<-paste0("sim",seq(1:1000))
        sim.col.prob<-(1-rtriangle(1000, a=c.value.min, b=c.value.max,c=c.value.mp))
        
        for(i in 1:1000){
                sim.t[,i]<-round(zz$value*(zz$pv.balance-zz$collateral*sim.col.prob[i]),2)
        }
        
        sim.t[sim.t<0]<-0
        sim.t<-cbind(zz,sim.t)
        sim.t
    })
    
    simresdata<-reactive({
        dt<-simdata()%>%
            filter(name==input$riskperiod)
        hist.pro <- density(colSums(dt))
        hist.pro
    })
    
    output$simres<-renderPlot({
        
        req(!is.null(simresdata()))
        hist.pro<-simresdata()
        pro_dens <- data.frame(hist.pro$x, hist.pro$y)
        
        ggplot(pro_dens,
               aes(
                   x = hist.pro.x,
                   y = hist.pro.y,
                   ymin = 0,
                   ymax = max(hist.pro.y)
               )) + geom_area(aes(y = hist.pro.y)) +
            labs(x="Amount",
                 y="Chance",
                 title = "Simulated expected credit loss",
                 subtitle = "Amounts discounted")+
            theme_bw()
    })
    
    output$cumprob<-renderText({
        res<-brushedPoints(simresdata(), input$sim_res_sel, "x","y")
        if (nrow(res) == 0){
            return()
        }
        paste0("Probability of the loss te be between",min(res$x)," and ", max(res$x), "is" ,sum(res$y))
    })


}

# Run the application 
shinyApp(ui = ui, server = server)
