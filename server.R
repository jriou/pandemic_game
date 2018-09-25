# update 25.09: added back dataframe as reactive value
library(shiny)
library(networkD3)
library(DT) # use package for editable DF.
library(cowplot)


scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
dir.create("temp", showWarnings = FALSE) # create temp dir where timestamped csv files are dumped, to be able to roll back changes.


data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE)



server <- function(input, output, session) {
  rv<-reactiveValues(data2=data1) #rv$dat2 is a reactive dataframe.
  #x = data1
  #x$Date = Sys.time() + seq_len(nrow(x))
  #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE)
  
  output$simple = renderSimpleNetwork({
    simpleNetwork(data.frame(src = rv$data2$sid, target = rv$data2$id),fontSize=12,fontFamily="Helvetica",opacity=.8)
  })
  output$plots = renderPlot({
    # epidemic curve
    d_epicurve = rv$data2
    d_epicurve$time2 = as.POSIXct(d_epicurve$time,format="%H:%M")
    g1 = ggplot(d_epicurve) +
      geom_bar(aes(x=time2),fill="steelblue",alpha=.8,colour="black") +
      theme_cowplot() +
      scale_x_datetime() +
      scale_y_continuous(expand=c(0,0)) +
      labs(x="Time of reporting",y="N")
    
    # distribution of secondary cases
    data<-rv$data2
    n_obs = length(data$id)
    d_split = split(data$id,data$sid)[-1]
    n_edges = c(unlist(lapply(d_split,length)),rep(0,n_obs-length(names(d_split))))
    Rnought = mean(n_edges)
    RnoughtCI = Rnought + qnorm(0.975) * c(-sd(n_edges),sd(n_edges))/sqrt(n_obs)
    dist_edges = data.frame(table(n_edges))
    Rn = paste0(" = ",round(Rnought,2)," [",round(RnoughtCI[1],2),"-",round(RnoughtCI[2],2),"]")
    g2 = ggplot(dist_edges) +
      geom_bar(aes(x=n_edges,y=Freq),stat="identity",fill="steelblue",alpha=.8,colour="black") +
      theme_cowplot() +
      scale_y_continuous(expand=c(0,0)) +
      scale_x_discrete() +
      labs(x="Number of secondary cases by case",y="N") +
      geom_text(x=max(as.numeric(dist_edges$n_edges)),y=max(dist_edges$Freq)-1.05,label=expression(R[0]),hjust=1,size=5) +
      geom_text(x=max(as.numeric(dist_edges$n_edges)),y=max(dist_edges$Freq)-1,label=Rn,hjust=0,size=5) +
      geom_point(aes(x=max(as.numeric(dist_edges$n_edges))+1.5,y=max(dist_edges$Freq)-1),col="white")
    
    # age gender
    #d_ag = data()
    d_ag=rv$data2
    d_ag$agecut = cut(d_ag$age,breaks=seq(0,90,by=5))
    d_ag$gender2 = factor(d_ag$gender,labels=c("Women","Men"))
    g3 = ggplot(d_ag) +
      geom_bar(aes(x=agecut),colour="black",fill="steelblue",alpha=.8) +
      theme_cowplot() +
      labs(x="Age",y="N")
    g4 = ggplot(d_ag) +
      geom_bar(aes(x=gender2,fill=gender2),colour="black",alpha=.8) +
      scale_fill_manual(values=c("tomato","steelblue"),labels=c("Women","Men"),guide=FALSE) +
      theme_cowplot() +
      labs(x="Gender",y="N")
    
    plot_grid(g1,g2,g3,g4)
  })
  output$x1 = renderDT(data1, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
  # 
  # output$plot1<-renderPlot({
  #   ggplot(data1,aes(x=age))+geom_histogram()},height = 400,width = 600)
  
  proxy = dataTableProxy('x1')
  
  
  observeEvent(input$x1_cell_edit, { # allows data editing.
    info = input$x1_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    rv$data2[i, j] <<- DT::coerceValue(v, rv$data2[i, j]) # replace cell value that was clicked
    replaceData(proxy, rv$data2, resetPaging = FALSE)  # update displayed data
    write.csv(file="data.csv", rv$data2, row.names=FALSE) # write the data to file
  })
  
  observeEvent(input$add, {
    
    old_data<-rv$data2
    updated_data = na.omit(rbind(old_data,
                                 data.frame(time=format(Sys.time(), "%H:%M"),
                                            id=input$id,
                                            sid=input$sid,
                                            gender=input$gender,
                                            age=input$age)))
    
    write.csv(file="data.csv", updated_data, row.names=FALSE) # write the data to file.
    write.csv(updated_data,file=paste0("temp/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data.csv"),row.names=FALSE)#save timestamped version.
    
    #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE) # order last first
    #output$x1 = renderDT(rv$data2, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
    rv$data2<-updated_data
    replaceData(proxy, rv$data2, resetPaging = FALSE) #update displayed data again.
    
    updateTextInput(session, "id", value = as.numeric(input$id)+1) # set id to next.
    updateTextInput(session, "age", value = NA)
    updateTextInput(session, "sid", value = NA)
    
    #updatePlots()
  })
  
}