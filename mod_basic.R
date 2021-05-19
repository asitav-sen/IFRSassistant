# Module for a graph

# Module to show some stats
basicstatUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    h3("No of Assets"),
    br(),
    p(
      "This section shows some basic information about the portfolio."
    ),
    plotOutput(ns("statplot")),
    verbatimTextOutput(ns("balancetext"))
  )
}

basicstatServer <- function(id, dt) {
  moduleServer(id,
               function(input, output, session) {
                 df <- reactive({
                   dt %>%
                     ungroup() %>%
                     arrange(id, report_date) %>%
                     group_by(id) %>%
                     slice_max(report_date, n = 1) %>%
                     mutate(yr = lubridate::year(origination_date))
                 })
                 
                 
                 output$statplot <- renderPlot({
                   req(!is.null(df))
                   withProgress(message = "Plotting some graphs",
                                detail = "Won't take long",
                                value = 0,
                                {
                                  setProgress(value = 1, message = "1 of 3..")
                                  
                                  p1 <-
                                    df() %>%
                                    group_by(yr, loan_status) %>%
                                    summarise(no_of_loans = n()) %>%
                                    ungroup() %>%
                                    ggplot(aes(
                                      x = yr,
                                      y = no_of_loans,
                                      label = no_of_loans,
                                      fill = factor(
                                        loan_status,
                                        levels = c("0", "1"),
                                        labels = c("Good", "Bad")
                                      )
                                    )) +
                                    geom_col(position = "dodge") +
                                    geom_text(aes(y = no_of_loans + 20), position = position_dodge(width = 1)) +
                                    labs(title = "No. of assets by year",
                                         x = "Year",
                                         y = "#") +
                                    theme_bw() +
                                    theme(legend.position = "none")
                                  setProgress(value = 2, message = "2 of 3..")
                                  
                                  p2 <-
                                    df() %>%
                                    group_by(asset_type, loan_status) %>%
                                    summarise(no_of_loans = n()) %>%
                                    ungroup() %>%
                                    ggplot(aes(
                                      x = asset_type,
                                      y = no_of_loans,
                                      label = no_of_loans,
                                      fill = factor(
                                        loan_status,
                                        levels = c("0", "1"),
                                        labels = c("Good", "Bad")
                                      )
                                    )) +
                                    geom_col(position = "dodge") +
                                    geom_text(aes(y = no_of_loans + 20), position = position_dodge(width = 1)) +
                                    labs(title = "No. of assets by asset type",
                                         x = "Asset Type",
                                         y = "#") +
                                    theme_bw() +
                                    theme(legend.position = "none")
                                  
                                  setProgress(value = 3, message = "3 of 3..")
                                  
                                  p3 <-
                                    df() %>%
                                    group_by(customer_type, loan_status) %>%
                                    summarise(no_of_loans = n()) %>%
                                    ungroup() %>%
                                    ggplot(
                                      aes(
                                        x = customer_type,
                                        y = no_of_loans,
                                        label = no_of_loans,
                                        fill = factor(
                                          loan_status,
                                          levels = c("0", "1"),
                                          labels = c("Good", "Bad")
                                        )
                                      )
                                    ) +
                                    geom_col(position = "dodge") +
                                    geom_text(aes(y = no_of_loans + 20), position = position_dodge(width = 1)) +
                                    labs(title = "No. of assets by customer type",
                                         x = "Customer Type",
                                         y = "#") +
                                    theme_bw() +
                                    theme(legend.position = "bottom",
                                          legend.title = element_blank())
                                  
                                  setProgress(value = 4, message = "Patching..")
                                  
                                  p4 <- p1 / (p2 | p3)
                                  
                                  setProgress(value = 5, message = "Done..")
                                  
                                })
                   
                   
                   p4
                   
                 })
                 
                 output$balancetext <- renderText({
                   paste0(
                     "Total assets (no.): ",
                     length(unique(df()$id)) ,
                     "\n",
                     "Total balance outstanding: ",
                     round(sum(df()$balance) / 1000000, 2),
                     " M"
                     
                   )
                 })
                 
                 
               })
  
}