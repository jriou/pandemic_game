# update 25.09: added back dataframe as reactive value
library(shiny)
library(networkD3)
library(dplyr)
library(DT) # use package for editable DF.
library(cowplot)
library(scales)
library(htmlwidgets)
require(visNetwork)
require(reshape2)
library("circlize")
library(grid)

scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
dir.create("temp", showWarnings = FALSE) # create temp dir where timestamped csv files are dumped, to be able to roll back changes.

source("mixing_matrix.R")




server <- function(input, output, session) {
  
  # set initial id value
  data1<- read.csv("data.csv",header=TRUE, stringsAsFactors=FALSE)
  updateTextInput(session, "id", value = max(data1$id)+1)

  rv<-reactiveValues(data2=data1) #rv$dat2 is a reactive dataframe.
  
  
  
  #x = data1
  #x$Date = Sys.time() + seq_len(nrow(x))
  #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE)
  
  
  observeEvent(rv$data2, {
  output$force = renderForceNetwork({
    d= rv$data2


    # recalculate id and source id to as forceNetwork doesnt handle inconsistencies well
    gid = c(1:NROW(d))
    # translate the orginal source ids to correspond to the new ids
    gsid = sapply(d$sid, function (x) {  t = which(d$id  == x);   if(is.vector(t)){ return(t[1]) }  })


    # assign here to have all nodes
    nodes = data.frame('name' = as.factor(paste0("id_",as.character(d[gid,]$id))), 'group' = as.factor(d$gender), 'size' = rep(1,NROW(d)))

    # get rid of inexisting source ids (invalid links)
    if(any(is.na(gsid))){
      t = which(is.na(gsid))
      gsid = gsid[-t]
      gid = gid[-t]
    }

    links = data.frame('source' = gsid -1, 'target' = gid-1, 'value' = rep(1, NROW(gid)))
    #print(links)
    #print(nodes)
    

    forceNetwork(Links = links, Nodes = nodes,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8, colourScale="d3.scaleOrdinal(d3.schemeCategory10);",
                 arrows = T, zoom = T, charge = -40, fontSize=12,fontFamily="Helvetica",
                 linkDistance =JS('function(d) {', 'return 20', '}'), bounded = T)


    # simpleNetwork(data.frame(src = rv$data2$sid, target = rv$data2$id),fontSize=12,fontFamily="Helvetica",opacity=.8)
  })
  
  # render network graphs depending on selection - unfortunately there seems to be some kind bugs in shiny and/or vis
  # output$networkgraph = renderUI({
  #   
  #     if(input$ngraph == "vis"){ 
  #        removeUI("#visnetwork", multiple = T, immediate = T)
  #        visNetworkOutput("visnetwork",height = "500px")
  #     }
  #     else if(input$ngraph == "force"){
  #       forceNetworkOutput("force", width = "100%", height = "520px")
  #     }
  #       
  # })
    
  output$visnetwork <- renderVisNetwork({
      
        d= rv$data2
    
        # recalculate id and source id t
        gid = c(1:NROW(d))
        
        # translate the orginal source ids to correspond to the new ids
        gsid = sapply(d$sid, function (x) {  t = which(d$id  == x);   if(is.vector(t)){ return(t[1]) }  })

        # replace NA with -1 for next statement
        gsid[is.na(gsid)] = -1
        
        # does the node not have children? (check if 0 rows have a correpsonding source id)
        gcl = sapply(gid, function (x) {
          abs(difftime(Sys.time() , as.POSIXct(d[x,"time"],format="%H:%M"),units="hours"))<input$exclude_time/60 | 
            as.POSIXct(d[x,"time"],format="%H:%M")>as.POSIXct(as.character(input$final_time),format="%H%M")
          })
        gcl = which(gcl,TRUE)
        
        gender = ifelse(d$g == 1, "male", ifelse(d$g == 0, "female",  "undefined"))
        floor = ifelse(d$floor %in% c(-1,0,1,2,3,4,5), d$floor, "u")
        
        # assign here to have all nodes displayed even if they are not connect to any other
        nodes = data.frame(id = gid, 
                           label = paste0("id_",as.character(d[gid,]$id)), 
                           group = ifelse(d[gid,]$gender == 1, "Maenner", "Frauen"),
                           title = paste0("Stockwerk: ", d$floor, "<br>", "Zeit: ", d$time,"<br>", d$comment))
        
        if(input$flooricons == T){
            nodes$size = 15
            nodes$shape = "image"
            nodes$image = paste0("set3/", floor, "_",  gender,  ".svg") 
            nodes[gcl,]$image = paste0("set3/", floor[gcl], "_", gender[gcl],  "_last.svg")
        }
        else{
          
          nodes$shape = 'icon'
          nodes$icon.face = 'FontAwesome'
          nodes$size = 30
          nodes$icon.code = ""
          nodes$icon.color = "black"
          
          # fontAwesome:  183,182: women, men sympols; 221,222: venus, mars sympols
          nodes[nodes$group == 'Maenner', ]$icon.code = "f183"
          nodes[nodes$group == 'Maenner', ]$icon.color = "steelblue"
          nodes[nodes$group == 'Frauen', ]$icon.code = "f182"
          nodes[nodes$group == 'Frauen', ]$icon.color = "tomato"
          if(length(gcl)>0) nodes[gcl,"icon.color"] = "grey"
        }
        # get rid of inexisting source ids (invalid links)
        if(any(gsid == -1)){
          t = which(gsid == -1)
          gsid = gsid[-t]
          gid = gid[-t]
        }
        
        edges <- data.frame(
          from = gsid,
          to = gid,
          color = ifelse(gender[gsid] == 'female', 'tomato', 'steelblue')
        )
        
        visNetwork(nodes, edges, width = "100%") %>%
          visEdges(arrows = "to") %>%
          visNodes() %>%
          visHierarchicalLayout(direction = "LR", levelSeparation = 100, nodeSpacing = 50, sortMethod = 'directed') %>%
          visOptions(highlightNearest = list(enabled = T, degree = 500, algorithm = "hierarchical"),
                     nodesIdSelection = list(enabled = T, useLabels = T)) %>%
          
          visPhysics(hierarchicalRepulsion = list(nodeDistance = 60)) %>%
          addFontAwesome()
          #visConfigure(enabled = TRUE)  
      
      })
    
  })
  output$plot_g1 = renderPlot({ # this gives some errors in absence of data/data with sufficient labels.
    # epidemic curve
    d_epicurve = rv$data2
    d_epicurve$time2 = as.POSIXct(d_epicurve$time,format="%H:%M")
    d_epicurve$time3 = as.numeric(format(d_epicurve$time2,"%H"))
    ggplot(d_epicurve) +
      geom_bar(aes(x=time3),fill="grey",alpha=.8,colour="black",binwidth=1) +
      theme_cowplot() +
      scale_x_continuous(expand=c(0,0),limits=c(7.3,19),breaks=seq(7.5,17.5,by=2),labels=c("8.00","10.00","12.00","14.00","16.00","18.00")) +
      scale_y_continuous(expand=c(0,0)) +
      labs(x="Registrierungszeitpunkt",y="N")
  })
  output$plot_g2 = renderPlot({
    # distribution of secondary cases
    d_dist = rv$data2
    d_dist = d_dist[!d_dist$sid==0,]
    d_dist$exclude = abs(difftime(Sys.time() , as.POSIXct(d_dist$time,format="%H:%M"),units="hours"))<input$exclude_time/60 | 
      as.POSIXct(d_dist$time,format="%H:%M")>as.POSIXct(as.character(input$final_time),format="%H%M")
    n_sources = sum(!d_dist$exclude)
    all_edges = data.frame(table(d_dist$sid))
    all_edges = data.frame(table(all_edges[!(all_edges$Var1 %in% d_dist$id[d_dist$exclude==1]),"Freq"]))
    dist_edges = rbind(all_edges,data.frame(Var1="0",Freq=n_sources-sum(all_edges$Freq)+1))
    dist_edges$secondary_cases=as.numeric(as.character(dist_edges$Var1))
    
      # mean secondary cases ignoring people arrived less than 1 hour ago or after we run out of stickers
    Rnought =  sum(dist_edges$secondary_cases*dist_edges$Freq) / n_sources
    Rn = paste0(" = ",sprintf("%.2f",Rnought))
    ggplot(dist_edges) +
      geom_bar(aes(x=as.character(secondary_cases),y=Freq),stat="identity",fill="grey",alpha=.8,colour="black") +
      theme_cowplot() +
      scale_y_continuous(expand=c(0,0),limits=c(0,max(dist_edges$Freq)+.7)) +
      scale_x_discrete() +
      labs(x="Anzahl Zweiterkrankter per Fall",y="N") +
      geom_text(x=max(as.numeric(dist_edges$secondary_cases)),y=max(dist_edges$Freq)+.3,label=expression(R[0]),hjust=1,size=5) +
      geom_text(x=max(as.numeric(dist_edges$secondary_cases)),y=max(dist_edges$Freq)+.3,label=Rn,hjust=0,size=5) +
      geom_point(aes(x=max(as.numeric(dist_edges$secondary_cases))+1,y=max(dist_edges$Freq)),col="white")
  })
  output$plot_g34 = renderPlot({
    # age gender
    d_ag=rv$data2
    d_ag$agecut = cut(d_ag$age,breaks=seq(0,90,by=5))
    d_ag$agecut = as.numeric(as.character(
      factor(d_ag$agecut,levels=paste0("(",seq(0,85,by=5),",",seq(5,90,by=5),"]"),labels=seq(2.5,87.5,by=5))))
    d_ag$gender2 = factor(d_ag$gender,labels=c("Frauen","Männer"), levels=c(0,1))
    g3 = ggplot(d_ag) +
      geom_bar(aes(x=agecut),colour="black",fill="grey",alpha=.8,width=5) +
      theme_cowplot() +
      scale_x_continuous(limits=c(0,90),expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) +
      labs(x="Alter",y="N")
    g4 = ggplot(d_ag) +
      geom_bar(aes(x=gender2,fill=gender2),colour="black",alpha=.8) +
      scale_fill_manual(values=c("tomato","steelblue"),labels=c("Frauen","Männer"),guide=FALSE) +
      theme_cowplot() +
      labs(x="Geschlecht",y="N")
    plot_grid(g3,g4,ncol=1)
  })
  output$plot_g5 = renderPlot({
    # migrationplot
    migrationplot_f(ag, rv$data2, col4, col_m, col_f, col_inter)
  })

  output$x1 = renderDT({ 
                      d = data1
                      names(d) = c("Zeit", "Id", "Index Id", "Geschlecht", "Alter",  "Stockwerk", "Kommentar")
                      d} , selection = 'multiple', editable = TRUE, 
                       options = list(
                      language = list(url = 'DT_german.json'),
                       order = list(list(1, 'desc')),
                       columnDefs = list(list(
                                      targets = c(7),
                                      visible = F
                                    ),list(
                                      targets = 4,
                                      render = JS(
                                        "function(data, type, row, meta) {",
                                        "//console.log(data);",
                                        "return type === 'display' && data == 1 ?",
                                        "'Mann' : 'Frau';",
                                        "}" 
                                      )
                                    ), list(
                                      targets = 0,
                                      visible = F
                                    ))                                                               ))
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
    updateTextInput(session, "id", value = max(rv$data2$id)+1)
  })
  
  observeEvent(input$add, {
    
    old_data<-rv$data2
    
    updated_data = rbind(old_data,
                         data.frame(time=format(Sys.time(), "%H:%M"),
                              id=input$id,
                              sid=input$sid,
                              gender=input$gender,
                              age=input$age,
                              floor = input$floor,
                              comment = input$comment)) %>% 
                    filter(!is.na(id) & !is.na(sid) & !is.na(gender))
    
    write.csv(file="data.csv", updated_data, row.names=FALSE) # write the data to file.
    write.csv(updated_data,file=paste0("temp/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data.csv"),row.names=FALSE)#save timestamped version.
    
    #output$x1 = renderDT(data1[seq(dim(data1)[1],1),], selection = 'none', editable = TRUE) # order last first
    #output$x1 = renderDT(rv$data2, selection = 'none', editable = TRUE, options = list(order = list(list(1, 'desc'))))
    rv$data2<-updated_data
    replaceData(proxy, rv$data2, resetPaging = FALSE) #update displayed data again.
    
    
    # if(nrow(updated_data)==nrow(old_data)){
    #   updateTextInput(session, "id", value = input$id)
    # }else{
    #   updateTextInput(session, "id", value = as.numeric(input$id)+1)
    #   }
    updateTextInput(session, "id", value = max(rv$data2$id)+1)
    updateTextInput(session, "age", value = NA)
    updateTextInput(session, "sid", value = NA)
    updateTextInput(session, "floor", value = NA)
    updateTextInput(session, "comment", value = NA)
    
    #updatePlots()
  })
  observeEvent(input$x1_rows_selected, {
    
    
    visNetworkProxy("visnetwork") %>% 
      visSelectNodes(input$x1_rows_selected)
    
  }, ignoreNULL = F)
  observeEvent(input$deleteRows,{
    
    if (!is.null(input$x1_rows_selected)) {
     
      # write backup before deletion
      write.csv(rv$data2,file=paste0("temp/before_rowdeletion_", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data.csv"),row.names=FALSE)#save timestamped version.
      
      # delete rows
      rv$data2 = rv$data2[-as.numeric(input$x1_rows_selected),]
      replaceData(proxy, rv$data2, resetPaging = FALSE)
      updateTextInput(session, "id", value = max(rv$data2$id)+1)
      
      # write updated data 
      write.csv(file="data.csv", rv$data2 , row.names=FALSE) 
    }
  })
}