ui <- fluidPage(
  titlePanel("PANDEMICS: infection spreads at ISPM!"),
  tags$p(),tags$br(),
  sidebarLayout(
    sidebarPanel(
      # input new patient
      numericInput("id", "Id:",value=textOutput("newid"), min=0),
      numericInput("sid", "Source Id:",value=NA,  min=0),
      selectInput("gender", "Gender:",c("Male" = 1,"Female" = 0)),
      numericInput("age", "Age:",value=NA, min=0,max=120),
      
      # action button
      actionButton("add", "Refresh",width="100%"),
      
      # show table
      tags$p(),tags$br(),
      DTOutput('x1'),
      
      width=3),
    
    
    mainPanel(
      simpleNetworkOutput("simple", width = "100%", height = "500px"),
      plotOutput("plots", width = "100%", height = "400px")
    )
  )
)