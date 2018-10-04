### Mixing matrix visualisation



# data_mr <- as.data.frame(read.csv("data.csv") )
lds <- length(data_mr$id)

# age group of ID
agegroups <- c("0-15", "16-30","31-45", "46-60", "60+") 
gender <- c("Frau", "Mann")
ag <- paste( rep(gender, each=length(agegroups)) ,agegroups ) 

#color of the external bands
col1 <- colorRampPalette(c("tomato4", "tomato"))(length(agegroups))
col3 <- colorRampPalette(c("steelblue4", "steelblue"))(length(agegroups))
col4 <- c(col1,col3)

#colors of the links (among men, among women and between men and women)
col_m <- "slategray1"
col_f <- "peachpuff"
col_inter <-  "lightgrey"  


migrationplot_f <- function(ag, data_mr, col4, col_m, col_f, col_inter) { 
  
  data_mr$agegroup_ID <-  ifelse(data_mr$age>=0 & data_mr$age<=15 &  data_mr$gender==0 ,ag[1],
                                 ifelse(data_mr$age>=16 & data_mr$age<=30 &  data_mr$gender==0 , ag[2],
                                        ifelse(data_mr$age>=31 & data_mr$age<=45 &  data_mr$gender==0 ,ag[3],
                                               ifelse(data_mr$age>=46 & data_mr$age<=60 &  data_mr$gender==0 , ag[4],
                                                      ifelse(data_mr$age>=60 &  data_mr$gender==0 , ag[5] ,
                                                             ifelse(data_mr$age>=0 & data_mr$age<=15 &  data_mr$gender==1 ,ag[6],
                                                                    ifelse(data_mr$age>=16 & data_mr$age<=30 &  data_mr$gender==1 , ag[7],
                                                                           ifelse(data_mr$age>=31 & data_mr$age<=45 &  data_mr$gender==1 ,ag[8],
                                                                                  ifelse(data_mr$age>=46 & data_mr$age<=60 &  data_mr$gender==1 , ag[9],
                                                                                         ifelse(data_mr$age>=60 &  data_mr$gender==1 , ag[10] , NA ) )))))))))
  #age group or SID
  for(i in min(data_mr$sid):max(data_mr$sid)){
    data_mr$agegroup_SID[data_mr$sid==i] <-  as.character(data_mr$agegroup_ID[data_mr$id==i])
  }
  
  # factor
  data_mr$agegroup_ID = factor(data_mr$agegroup_ID,levels=ag)
  data_mr$agegroup_SID = factor(data_mr$agegroup_SID,levels=ag)
  
  dt2 <- as.data.frame( table(data_mr$agegroup_ID,data_mr$agegroup_SID)  )
  dt3 <- dt2
  dt3$ts1 <- apply(dt3 , 1, function(x) if(any(x[2]== ag[1:5]) & any(x[1]== ag[6:10])) {"diss"}else if(any(x[2]== ag[6:10]) & any(x[1]== ag[1:5])){"diss"} else {"sim"} )   
  
  #define colors of links
  g.col.l <- apply(dt3, 1, function(x) ifelse( x[4]=="sim" & any(x[1]==ag[1:5]) , col_f ,ifelse( x[4]=="sim" & any(x[1]==ag[6:10]),  col_m, col_inter ) ) )
  

  #plot  contacts made and recieved
  p1 <- chordDiagram(dt2, grid.col= col4, col=g.col.l, annotationTrack=c("name", "grid") , preAllocateTracks =list(track.height = 0.03,0.01) )  
  
  return(p1)
}

# migrationplot_f(ag, data_mr, col4, col_m, col_f, col_inter)
