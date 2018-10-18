library(DT)
library(networkD3)
require(visNetwork)


ui <- fluidPage(
  includeCSS("custom.css"),
  titlePanel("PANDEMIE: Infektion breitet sich im ISPM aus!"),
  tags$p(),tags$br(),
  sidebarLayout(
    sidebarPanel(
      # input new patient
      div(class= 'input-right', 
          numericInput("id", "Id:", value = NA,  min=0, width = '100%'),
          numericInput("sid", "Quellen Id:",value=NA,  min=0, width = '100%')),
      selectInput("gender", label = NULL,c("Mann" = 1,"Frau" = 0)),
      div(class= 'input-right',
          numericInput("age", "Alter:",value=NA, min=0,max=120, width = '100%'),
          numericInput("floor", "Stockwerk:",value=NA, min=-1,max=5, width = '100%')),
      textInput("comment", label = NULL, value = NA, placeholder = 'Kommentar'),
      
      # action button
      actionButton("add", "Aktualisieren",width="100%"),
      
      # show table
      tags$p(),tags$br(),
      div(
        DTOutput('x1')),
      actionButton("deleteRows", "Zeilen loeschen"),
      selectInput("ngraph", label = NULL,
                  c("Vis- Network Graph" = "vis",
                    "Force D3 Network Graph (refersh pls)" = "force")),
      checkboxInput("flooricons","Icons mit Stockwerk anzeigen", value = F),
      sliderInput("exclude_time","Ohne Infektion in den letzten x Minuten",value=60,min=10,max=240),
      numericInput("final_time","Alle Sticker aufgebraucht",value=1700),
      width=4),
    
    mainPanel(
      
      #uiOutput("networkgraph"),
      
      conditionalPanel(condition = "input.ngraph == 'vis'",
                       visNetworkOutput("visnetwork",height = "500px")
      ),
      conditionalPanel(condition = "input.ngraph == 'force'",
                       forceNetworkOutput("force", width = "100%", height = "520px")
      ),
      column(width=6,
             plotOutput("plot_g1", width = "100%", height = "200px"),
             plotOutput("plot_g34", width = "100%", height = "400px")),
      column(width=6,
             plotOutput("plot_g2", width = "100%", height = "200px"),
             plotOutput("plot_g5", width = "100%", height = "400px"))
    )
  )
)