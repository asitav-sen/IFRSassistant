
# 2nd modal
modal2<-modalDialog(
  title = "File formatting",
  fluidRow(
    
    column(
      width = 6,
      p("Transactions file"),
      varSelectInput("transid","Select id column", NULL),
      varSelectInput("reportdate","Select report date column", NULL),
      varSelectInput("origindate","Select origin date column", NULL),
      varSelectInput("maturitydate","Select maturity date column", NULL),
      varSelectInput("assettype","Select asset classifier column", NULL),
      varSelectInput("customertype","Select customer classifier column", NULL),
      varSelectInput("otherfact","Select any other classifier column", NULL, multiple = T)
    ),
    column(
      width = 6,
      p("Transactions file"),
      varSelectInput("bureauscore","Select bureau score column", NULL),
      varSelectInput("balance","Select asset balance column", NULL),
      varSelectInput("status","Select loan status column", NULL),
      varSelectInput("defaultflag","Select default flag column", NULL),
      radioButtons("dateformat","Select date format",choices = c("dmy","ymd"), selected = "ymd", inline = T),
      p("Collateral File"),
      varSelectInput("collateralid","Select id column", NULL),
      varSelectInput("collateralvalue","Select Collateral value column", NULL)
    )
  ),
  easyClose = F,
  size = "l",
  footer = tagList(
    actionButton("confirmupload", "Confirm"),
    actionButton("closemodal2","Close")
  )
)

modal1<- modalDialog(
  title = "Upload files",
  fluidRow(
    column(
      width = 6,
      p("Add Transaction file"),
      fileInput("transaction", "Choose CSV File of transactions",
                multiple = F,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      p("Add Collateral file"),
      fileInput("collaterals", "Choose CSV File of collaterals",
                multiple = F,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxInput("header", "Header", TRUE),
      fluidRow(
        column(
          width = 6,
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ",")
        ),
        column(
          width = 6,
          radioButtons("quote", "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"')
        )
      )
    ),
    column(
      width = 6,
      fluidRow(
        tags$div(
          "Following columns are required in the transaction file",
          tags$ul(
            tags$li("A unique identifier of asset (id)"), 
            tags$li("Date on which last report was generated. The last reporting date of all the assets must be same."), 
            tags$li("Origination Date"),
            tags$li("Maturity Date"),
            tags$li("Default flag i.e. whether previously defaulted"),
            tags$li("Bureau/internal credit score"),
            tags$li("Current loan status (1 is bad and 0 is good)"),
            tags$li("Asset classification"),
            tags$li("Customer classification"),
            tags$li("Asset balance")
          ),
          "The collateral file should contain present value of the collateral/possible price of sales of the asset/sales of hypothecated asset etc."
        )
      )
    )
  ),
  easyClose = F,
  size = "l",
  footer = tagList(
    actionButton("uploadfiles", "Proceed"),
    actionButton("closemodal1","Close")
  )
)