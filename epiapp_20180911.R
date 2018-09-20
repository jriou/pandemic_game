#################################################
#                                               #
#       First version of ISPM epi game app      #
#                 11.09.2018                    #
#                                               #
#################################################

library(shiny)
#library(shinydashboard)
library(ggplot2)
#devtools::install_github('rstudio/DT') install package 'DT' from github
library(DT) # use package for editable DF.
# install.packages("networkD3")
# library(networkD3)
# 
# networkData <- data.frame(sid, id)
# networkData <- data.frame(data1$sid, data1$id)
# simpleNetwork(networkData)


scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
dir.create("temp", showWarnings = FALSE) # create temp dir where timestamped csv files are dumped, to be able to roll back changes.


data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE)
rv<-reactiveValues(data2=data1) #rv$dat2 is a reactive dataframe.

ui <- fluidPage(
  titlePanel("Epi game first version: input/change data, updating plots"),
  
  fluidRow(
    column(3,
        DTOutput('x1'),
        h4("Add new person:", style = "background-color:#44EEFF;"),
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
  column(9, style = "background-color:#44EEFF;",
         h1("REST OF STUFF HERE?"),
         plotOutput("plot1"),
         h2("add more interactive plots... and add some design :-)"))
  ),
  textOutput("selected_var")
)

server <- function(input, output, session) {
      #x = data1
      #x$Date = Sys.time() + seq_len(nrow(x))
      #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE)
  updatePlots<-function(){
    output$plot1<-renderPlot({
      ggplot(rv$data2,aes(x=as.numeric(age)))+geom_histogram()},height = 400,width = 600)
  }
  
  output$x1 = renderDT(data1, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
  
  output$plot1<-renderPlot({
    ggplot(data1,aes(x=age))+geom_histogram()},height = 400,width = 600)

  output$currentTime <- renderText({
    # invalidateLater causes this output to automatically
    # become invalidated when input$interval milliseconds
    # have elapsed
    invalidateLater(as.integer(2000))
    format(Sys.time(), "%H:%M")
  })
      proxy = dataTableProxy('x1')
 # observe(rv$data2,{
 #   output$plot1<-renderPlot({ggplot(rv$data2,aes(x=age))+geom_histogram(colour='red')})
 # })

  observeEvent(input$x1_cell_edit, { # allows data editing.
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      rv$data2[i, j] <<- DT::coerceValue(v, rv$data2[i, j]) # replace cell value that was clicked
      replaceData(proxy, rv$data2, resetPaging = FALSE)  # update displayed data
      write.csv(file="data.csv", rv$data2, row.names=FALSE) # write the data to file
      #cat(paste(rv$data2))
      #output$x1 = renderDT(rv$data2, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
      updatePlots()
    })
  observeEvent(input$add, {

    time1<- format(Sys.time(), "%H:%M")
    filename<-paste0("temp/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data_set.csv") #create timestamped filename to backup data.

    #data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE) 
    rv$data2<- rbind(rv$data2, data.frame(id=as.numeric(input$id),time=time1
                                      ,gender=input$gender,age=input$age,sid=input$sid))
    write.csv(file="data.csv", rv$data2, row.names=FALSE) # write the data to file.
    write.csv(file=filename, rv$data2, row.names=FALSE) #save timestamped version.
    #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE) # order last first
    #output$x1 = renderDT(rv$data2, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
    replaceData(proxy, rv$data2, resetPaging = FALSE) #update displayed data again.
    
    updateTextInput(session, "id", value = as.numeric(input$id)+1) # set id to next.
    updateTextInput(session, "age", value = "")
    updateTextInput(session, "sid", value = "")
    
    updatePlots()
  })
  
}

shinyApp(ui, server)