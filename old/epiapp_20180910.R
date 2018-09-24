library(shiny)
library(shinydashboard)
library(ggplot2)
devtools::install_github('rstudio/DT')
library(DT) # use package for editable DF.
scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
dir.create("temp", showWarnings = FALSE) # create temp dir where timestamped csv files are dumped, to be able to roll back changes.


data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE)

# library(shiny)
# library(DT)
# shinyApp(
#   ui = fluidPage(
#     DTOutput('x1')
#   ),
#   server = function(input, output, session) {
#     x = iris
#     x$Date = Sys.time() + seq_len(nrow(x))
#     output$x1 = renderDT(x, selection = 'none', editable = TRUE)
#     
#     proxy = dataTableProxy('x1')
#     
#     observeEvent(input$x1_cell_edit, {
#       info = input$x1_cell_edit
#       str(info)
#       i = info$row
#       j = info$col
#       v = info$value
#       x[i, j] <<- DT::coerceValue(v, x[i, j])
#       replaceData(proxy, x, resetPaging = FALSE)  # important
#     })
#   }
# )

ui <- fluidPage(
  titlePanel("Lets try to load a set of data and make interactive"),
  
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
         plotOutput("plot1"))
  ),
  textOutput("selected_var")
)

server <- function(input, output, session) {
      #x = data1
      #x$Date = Sys.time() + seq_len(nrow(x))
      #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE)
  output$x1 = renderDT(data1, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
  
  output$plot1<-renderPlot({
    ggplot(data1,aes(x=age))+geom_histogram(colour='red')},height = 400,width = 600)

  output$currentTime <- renderText({
    # invalidateLater causes this output to automatically
    # become invalidated when input$interval milliseconds
    # have elapsed
    invalidateLater(as.integer(2000))
    format(Sys.time(), "%H:%M")
  })
      proxy = dataTableProxy('x1')
 
      observeEvent(input$x1_cell_edit, { # allows data editing.
        info = input$x1_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        data1[i, j] <<- DT::coerceValue(v, data1[i, j])
        #replaceData(proxy, data1, resetPaging = FALSE)  # important
        write.csv(file="data.csv", data1, row.names=FALSE)
        cat(paste(data1))
        output$x1 = renderDT(data1, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
        
      })
  observeEvent(input$add, {

    time1<- format(Sys.time(), "%H:%M")
    filename<-paste0("temp/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data_set.csv")

    data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE) 
    data1<- rbind(data1, data.frame(id=as.numeric(input$id),time=time1
                                      ,gender=input$gender,age=input$age,sid=input$sid))
    write.csv(file="data.csv", data1, row.names=FALSE)
    write.csv(file=filename, data1, row.names=FALSE) #save timestamped version.
    #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE) # order last first
    output$x1 = renderDT(data1, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
    #replaceData(proxy, data1, resetPaging = FALSE)
    renderPlot({
      ggplot(data1,aes(x=age))+geom_histogram(colour='red')},height = 400,width = 600)
  })
  
  
}

shinyApp(ui, server)