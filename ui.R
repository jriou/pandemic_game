library(DT)
library(networkD3)
require(visNetwork)


ui <- fluidPage(
  includeCSS("custom.css"),
  titlePanel("PANDEMICS: infection spreads at ISPM!"),
  tags$p(),tags$br(),
  sidebarLayout(
    sidebarPanel(
      # input new patient
      div(class= 'input-right', 
        numericInput("id", "Id:", value = NA,  min=0, width = '100%'),
        numericInput("sid", "Source Id:",value=NA,  min=0, width = '100%')),
      selectInput("gender", label = NULL,c("Male" = 1,"Female" = 0)),
      div(class= 'input-right',
        numericInput("age", "Age:",value=NA, min=0,max=120, width = '100%'),
        numericInput("floor", "Floor:",value=NA, min=0,max=5, width = '100%')),
      textInput("comment", label = NULL, value = NA, placeholder = 'Comment'),
      
      # action button
      actionButton("add", "Refresh",width="100%"),
      
      # show table
      tags$p(),tags$br(),
      div(
        DTOutput('x1')),
        actionButton("deleteRows", "Delete Rows"),
        selectInput("ngraph", label = NULL,
                  c("Vis- Network Graph" = "vis",
                    "Force D3 Network Graph (refersh pls)" = "force")),
      checkboxInput("flooricons","Show icons with floor", value = F),
      width=3),
    
    mainPanel(
      
      #uiOutput("networkgraph"),
      
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