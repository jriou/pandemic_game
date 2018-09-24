library(shiny)
library(networkD3)
library(DT) # use package for editable DF.
library(cowplot)

dir.create("temp", showWarnings = FALSE)

server <- function(input, output, session) {
  
  # reactive data
  data = eventReactive(input$add, {
    
    # get old data file
    if(!file.exists("data.csv")) {
      old_data = data.frame(time=NULL,id=NULL,sid=NULL,gender=NULL,age=NULL)
    } else {
      old_data = read.csv("data.csv")
    }
    
    # append new patient
    updated_data = na.omit(rbind(old_data,
                                 data.frame(time=format(Sys.time(), "%H:%M"),
                                            id=input$id,
                                            sid=input$sid,
                                            gender=input$gender,
                                            age=input$age)))
    
    # save current data
    write.csv(updated_data,file="data.csv",row.names=FALSE)
    
    # save time stamp
    write.csv(updated_data,file=paste0("temp/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data.csv"),row.names=FALSE)
    
    # updates
    updateNumericInput(session, "id", value = NA)
    updateNumericInput(session, "age", value = NA)
    updateNumericInput(session, "sid", value = NA)
    return(updated_data)
  })
  
  # render simple network plot
  output$simple = renderSimpleNetwork({
    simpleNetwork(data.frame(src = data()$sid, target = data()$id),fontSize=12,fontFamily="Helvetica",opacity=.8)
  })
  
  # render other plots
  output$plots = renderPlot({
    # epidemic curve
    d_epicurve = data()
    d_epicurve$time2 = as.POSIXct(d_epicurve$time,format="%H:%M")
    g1 = ggplot(d_epicurve) +
      geom_bar(aes(x=time2),fill="steelblue",alpha=.8,colour="black") +
      theme_cowplot() +
      scale_x_datetime() +
      scale_y_continuous(expand=c(0,0)) +
      labs(x="Time of reporting",y="N")
    
    # distribution of secondary cases
    n_obs = length(data()$id)
    d_split = split(data()$id,data()$sid)[-1]
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
    d_ag = data()
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
  
  # render edges
  # split
  
  # render age sex
  
  # render DT table
  output$x1 = renderDT(data(), selection = 'none', editable = TRUE, rownames= FALSE, options = list(order = list(list(0, 'desc'))))
  
}
