# Module to show some stats
basicstatUI<- function(id){
  ns <- NS(id)
  fluidRow(
    h3("No of Assets"),
    br(),
    withSpinner(plotOutput(ns("statplot")), type=7, color="black"),
    verbatimTextOutput(ns("balancetext"))
  )
}

basicstatServer<- function(id, dt){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      df<- reactive({
        dt%>%
          group_by(id)%>%
          slice_max(report_date,n=1)%>%
          ungroup()%>%
          mutate(yr=lubridate::year(origination_date))
      })
      
      
      output$statplot<-renderPlot({
        
        req(!is.null(df))
        
        p1<-
          df()%>%
          group_by(yr,loan_status)%>%
          summarise(no_of_loans=n())%>%
          ungroup() %>%
          ggplot(aes(x=yr, y=no_of_loans, label=no_of_loans, fill=factor(loan_status, levels = c("0","1"),
                                                                         labels = c("Good","Bad"))))+
          geom_col(position = "dodge")+
          geom_text(aes(y=no_of_loans+20),position = position_dodge(width = 1))+
          labs(title = "No. of assets by year",
               x="Year",
               y="#")+
          theme_bw()+
          theme(legend.position = "none")
        p2<-
          df()%>%
          group_by(asset_type,loan_status)%>%
          summarise(no_of_loans=n())%>%
          ungroup() %>%
          ggplot(aes(x=asset_type, y=no_of_loans, label=no_of_loans, fill=factor(loan_status, levels = c("0","1"),
                                                                                 labels = c("Good","Bad"))))+
          geom_col(position = "dodge")+
          geom_text(aes(y=no_of_loans+20),position = position_dodge(width = 1))+
          labs(title = "No. of assets by asset type",
               x="Asset Type",
               y="#")+
          theme_bw()+
          theme(legend.position = "none")
        
        p3<-
          df()%>%
          group_by(customer_type,loan_status)%>%
          summarise(no_of_loans=n())%>%
          ungroup() %>%
          ggplot(aes(x=customer_type, y=no_of_loans, label=no_of_loans, fill=factor(loan_status, levels = c("0","1"),
                                                                                    labels = c("Good","Bad"))))+
          geom_col(position = "dodge")+
          geom_text(aes(y=no_of_loans+20),position = position_dodge(width = 1))+
          labs(title = "No. of assets by customer type",
               x="Customer Type",
               y="#")+
          theme_bw()+
          theme(legend.position = "bottom")
        p4<- p1 /( p2 | p3)
        
        p4
        
      })
      
      output$balancetext<- renderText({
        d<-df()
        d<-
          d%>%
          slice_max(report_date, n=1)
        paste0(
          "Total assets (no.): ", nrow(d) , "\n",
          "Total balance outstanding: ", round(sum(d$balance)/1000000,2), " M"
          
        )
      })
      
    }
  )
  
}