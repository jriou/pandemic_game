library(DT)
library(networkD3)
require(visNetwork)

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
      div(
        DTOutput('x1')),
        actionButton("deleteRows", "Delete Rows"),
        selectInput("ngraph", "Network graph type:",
                  c("Force D3" = "force",
                    "Vis" = "vis")),
      width=3),
    
    mainPanel(
      conditionalPanel(condition = "input.ngraph == 'vis'",
        visNetworkOutput("visnetwork",height = "500px")
      ),
      conditionalPanel(condition = "input.ngraph == 'force'",
       forceNetworkOutput("force", width = "100%", height = "520px")
      ),
      plotOutput("plots", width = "100%", height = "400px")
    )
  )
)