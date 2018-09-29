# update 25.09: added back dataframe as reactive value
library(shiny)
library(networkD3)
library(dplyr)
library(DT) # use package for editable DF.
library(cowplot)
library(scales)
library(htmlwidgets)
require(visNetwork)

scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
dir.create("temp", showWarnings = FALSE) # create temp dir where timestamped csv files are dumped, to be able to roll back changes.





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

        
        gender = ifelse(d$g == 1, "male", ifelse(d$g == 0, "female",  "undefined"))
        floor = ifelse(d$floor %in% c(0,1,2,3,4,5), d$floor, "undefined")
        
        # assign here to have all nodes displayed even if they are not connect to any other
        nodes = data.frame(id = gid, 
                           label = paste0("id_",as.character(d[gid,]$id)), 
                           group = ifelse(d$gender == 1, "Men", "Women"), size = 15,
                           title = paste0("Floor: ", d$floor, "<br>", "Time: ", d$time,"<br>", d$comment))
        
        if(input$flooricons == T){
            nodes$shape = "image"
            nodes$image = paste0("set2/", gender, "_f", floor, ".svg")
        }

        
        # get rid of inexisting source ids (invalid links)
        if(any(is.na(gsid))){
          t = which(is.na(gsid))
          gsid = gsid[-t]
          gid = gid[-t]
        }
        
        edges <- data.frame(
          from = gsid,
          to = gid
        )


        
        visNetwork(nodes, edges, width = "100%") %>%
          visEdges(arrows = "to") %>%
          visNodes() %>%
          visHierarchicalLayout(direction = "LR", levelSeparation = 100, nodeSpacing = 50, sortMethod = 'directed') %>%
          # fontAwesome:  183,182: women, men sympols; 221,222: venus, mars sympols
          visGroups(groupname = "Men", shape='icon', icon = list(code = "f183", color="steelblue",size=30)) %>%
          visGroups(groupname = "Women",  shape='icon', icon = list(code = "f182", color="tomato", size=30)) %>%
          visOptions(highlightNearest = list(enabled = T, degree = 4, algorithm = "hierarchical"),
                     nodesIdSelection = list(enabled = T, useLabels = T)) %>%
          
          visPhysics(hierarchicalRepulsion = list(nodeDistance = 60)) %>%
          addFontAwesome()
          #visConfigure(enabled = TRUE) 
      
        
        # # minimal example
        # nodes <- data.frame(id = 1:3)
        # edges <- data.frame(from = c(1,2), to = c(1,3))
        # 
        # visNetwork(nodes, edges)
      
      })
    
  })
  output$plots = renderPlot({ # this gives some errors in absence of data/data with sufficient labels.
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
    d_ag$gender2 = factor(d_ag$gender,labels=c("Women","Men"), levels=c(0,1))
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
  output$x1 = renderDT(data1, selection = 'multiple', editable = TRUE, 
                       options = list(order = list(list(1, 'desc')),
                       columnDefs = list(list(
                                      targets = 7,
                                      render = JS(
                                        "function(data, type, row, meta) {",
                                        "return type === 'display' && data.length > 6 ?",
                                        "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                        "}")
                                    ),list(
                                      targets = 4,
                                      render = JS(
                                        "function(data, type, row, meta) {",
                                        "//console.log(data);",
                                        "return type === 'display' && data == 1 ?",
                                        "'male' : 'female';",
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