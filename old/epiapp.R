library(shiny)
library(shinydashboard)
scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
dir.create("temp", showWarnings = FALSE) # create temp dir where timestamped csv files are dumped, to be able to roll back changes.


data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE)


ui <- fluidPage(
  titlePanel("Lets try to load a set of data and make interactive"),
  
  fluidRow(
    column(3,
      dataTableOutput("table")),
    column(2,
        textInput("id", "Id:", value = "", width = NULL, placeholder = NULL),
        h5("Current time:"),
        textOutput("currentTime"),
        selectInput("gender", "Gender:",
                    c("Male" = 1,
                      "Female" = 0)),
        textInput("age", "Age:", value = "", width = NULL, placeholder = NULL),
        textInput("sid", "Source Id:", value = "", width = NULL, placeholder = NULL),
        actionButton("add", "Add data!")
      ),
  column(7, style = "background-color:#44EEFF;",
         h1("REST OF STUFF HERE?"))
  ),
  textOutput("selected_var")
)

server <- function(input, output, session) {
  output$table <- renderDataTable(data1)
  
  output$currentTime <- renderText({
    # invalidateLater causes this output to automatically
    # become invalidated when input$interval milliseconds
    # have elapsed
    invalidateLater(as.integer(2000))
    format(Sys.time(), "%H:%M")
  })
  
  observeEvent(input$add, {

    time1<- format(Sys.time(), "%H:%M")
    filename<-paste0("temp/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data_set.csv")

    data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE) 
    data1<- rbind(data1, data.frame(id=input$id,time=time1
                                      ,gender=input$gender,age=input$age,sid=input$sid))
    write.csv(file="data.csv", data1, row.names=FALSE)
    write.csv(file=filename, data1, row.names=FALSE) #save timestamped version.
    output$table <- renderDataTable(data1)
  })
  
  
}

shinyApp(ui, server)