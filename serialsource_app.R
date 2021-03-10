## Tony Johnson
## Last modified 12/8/2020 to add leeway for subzero force values during pull 
## initialization
## added simple try catch to make sure code stops and closes port
## on warnings
## added buffer parsing if it's more than 1 read
## Functions to pull data from Exurgo via serial port connection

## libraries
library(shiny)
library(shinyjs)
library(lubridate)
library(serial)
library(stringr)
library(dplyr)
library(DT)
library(ggplot2)
#######################################################################
## function definitions: 10 functions (9 actually used and working)
#######################################################################


## function 1:
## Exsurgo serial port initialization
## initializes a serial connection
## opens the connection, starts recording and tares
## then flushes the buffer so it records after tare
con_init<-function(){
  
  ports<-listPorts()
  con <- serialConnection(name = "test_con",
                          port = ports[2],
                          mode = "115200,n,8,1",
                          buffering = "none",
                          handshake = "rtscts",
                          newline = 1,
                          translation = "auto")
  open(con)
  flush(con)
  write.serialConnection(con,"w")
  write.serialConnection(con,"q")
  write.serialConnection(con, "t")
  flush(con)
  return(con)
  
}


## function 2:
## Exsurgo serial port reading
## reads serial connection until it falls to 0 or below.
## returns a dataframe with time and force
con_read<-function(con,input_seconds){
  
  ## initialize storage vectors and initial values
  x<-0
  time<-0
  time_vec<-c()
  y<-c()
  fail_counts<-0
  exit_condition<-input_seconds+1
  
  while(x==0){
    temp<-read.serialConnection(con)
    boolstore<-str_detect(temp, "gStrength_MAX_25ms")
    
    if((boolstore==TRUE) | (temp=="")){
      flush(con)
      next
      
    }else{
      temp2<-unlist(strsplit(temp,"[.]"))
      read_len<-length(temp2)
      
      ## normal read
      if(read_len==2){
        #print(temp)
        y<-c(y,temp)
        time_vec<-c(time_vec,time)
        time = time + 0.025
        temp<-tryCatch(as.numeric(temp),warning=function(c){
          print("Serial Connection Closed (Data is NaN)")
          write.serialConnection(con, "w")
          close(con)
          c$message<-paste0("multiple serial readings in 1 buffer,
                          try test again.")
          stop(c)
        })
        flush(con)
        ## exit condition
        if((temp <= 0) & (time>exit_condition)) x=1
        
      ## filled buffer read
      }else{
        restore<-c()
        ## separate by decimals. Exsurgo reads in 3 digits after decimal place
        for(i in 1:(read_len-1)){
          if(i==1){
            restore<-c(restore,paste(temp2[i],substr(temp2[i+1],1,3),sep="."))
          }else{
            restore<-c(restore,
                       paste(substr(temp2[i],4,nchar(temp2[i],type="chars")),
                             substr(temp2[i+1],1,3),sep="."))
          }
        }
        y<-c(y,restore)
        ## catch up the time vector
        for(j in 1:(read_len-1)){
          time_vec<-c(time_vec,time)
          time = time + 0.025
          
        }
        
        temp<-tryCatch(as.numeric(restore),warning=function(c){
          print("Serial Connection Closed (Data is NaN)")
          write.serialConnection(con, "w")
          close(con)
          c$message<-paste0("multiple serial readings in 1 buffer,
                          try test again.")
          stop(c)
        })
        
        fail_counts = fail_counts + 1
        flush(con)
        
        ## exit condition
        #if(any(temp<=0)) x=1
        if((fail_counts>3) & (time>exit_condition)) x=1
      }
    }
  }
  
  write.serialConnection(con, "w")
  close(con)
  print("Serial Stopped Connection Normally")
  
  data<-as.data.frame(cbind(time_vec,as.numeric(y)))
  names(data)<-c("Time","Force")
  return(data)
  
}


## function 3:
## Finding the maximum in a column of a dataframe
## In this case, used for maximum force
## returns a dataframe with filtered only for the row with the max
df_max<-function(df,colname){
  
  ##create an index for the max data to be subset-able
  df$index<-1:nrow(df)
  col_index<-str_detect(names(df),paste("\\b",colname,"\\b",sep=""))
  max_index<-which.max(as.numeric(unlist(df[col_index])))
  max_data<-subset(df,index==max_index)
  ##remove the index from the max dataframe for continuity
  max_data<-max_data[!names(max_data) %in% c("index")]
  return(max_data)
  
}


## function 4:
## Plotting y vs x with a point of interest 
## assumes the point of interest is already found and saved in a dataframe
plot_poi<-function(df,df_poi,title,x_axis,y_axis,x_lab,y_lab,poi_lab){
  x_col_index<-str_detect(names(df),paste("\\b",x_axis,"\\b",sep=""))
  y_col_index<-str_detect(names(df),paste("\\b",y_axis,"\\b",sep=""))
  
  x_col_index_poi<-str_detect(names(df_poi),paste("\\b",x_axis,"\\b",sep=""))
  y_col_index_poi<-str_detect(names(df_poi),paste("\\b",y_axis,"\\b",sep=""))
  
  p<-ggplot(df, aes(as.numeric(unlist(df[x_col_index])),
                      as.numeric(unlist(df[y_col_index]))))+geom_line()+
    annotate("point", x=as.numeric(unlist(df_poi[x_col_index_poi])), 
             y=as.numeric(unlist(df_poi[y_col_index_poi])), colour="red")+
    geom_text(aes(x=as.numeric(unlist(df_poi[x_col_index_poi])),
                  y=as.numeric(unlist(df_poi[y_col_index_poi])),
                  label=paste(poi_lab,as.numeric(unlist(df_poi[y_col_index_poi])),sep=" ")),
              nudge_y = 5,
              nudge_x = 0.1,
              color="black")+
    xlab(x_lab) + ylab(y_lab)+ 
    ggtitle(title)+
    theme_bw() + theme(panel.border = element_blank(), 
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), 
                       axis.line = element_line(colour = "black"))
  
  return(p)
}


## function 5:
## Plotting y vs x with 2 points of interest 
## assumes the point of interests are already found and saved in a dataframes
plot_2poi<-function(df,df_poi,df_poi2,title,x_axis,y_axis,
                    x_lab,y_lab,poi_lab,poi_lab2){
  ## find index for the main data and pois since it is possible 
  ## that dataframes have different column sizes
  
  x_col_index<-str_detect(names(df),paste("\\b",x_axis,"\\b",sep=""))
  y_col_index<-str_detect(names(df),paste("\\b",y_axis,"\\b",sep=""))
  
  x_col_index_poi<-str_detect(names(df_poi),paste("\\b",x_axis,"\\b",sep=""))
  y_col_index_poi<-str_detect(names(df_poi),paste("\\b",y_axis,"\\b",sep=""))
  
  x_col_index_poi2<-str_detect(names(df_poi2),paste("\\b",x_axis,"\\b",sep=""))
  y_col_index_poi2<-str_detect(names(df_poi2),paste("\\b",y_axis,"\\b",sep=""))
  
  p<-ggplot(df, aes(as.numeric(unlist(df[x_col_index])),
                    as.numeric(unlist(df[y_col_index]))))+geom_line()+
    annotate("point", x=as.numeric(unlist(df_poi[x_col_index_poi])), 
             y=as.numeric(unlist(df_poi[y_col_index_poi])), colour="red")+
    geom_text(aes(x=as.numeric(unlist(df_poi[x_col_index_poi])),
                  y=as.numeric(unlist(df_poi[y_col_index_poi])),
                  label=paste(poi_lab,as.numeric(unlist(df_poi[y_col_index_poi])),sep=" ")),
              nudge_y = 5,
              nudge_x = 0.1,
              color="black")+
    annotate("point", x=as.numeric(unlist(df_poi2[x_col_index_poi2])), 
             y=as.numeric(unlist(df_poi2[y_col_index_poi2])), colour="red")+
    geom_text(aes(x=as.numeric(unlist(df_poi2[x_col_index_poi2])),
                  y=as.numeric(unlist(df_poi2[y_col_index_poi2])),
                  label=paste(poi_lab2,as.numeric(unlist(df_poi2[y_col_index_poi2])),sep=" ")),
              nudge_y = 5,
              color="black")+
    xlab(x_lab) + ylab(y_lab)+ 
    ggtitle(title)+
    theme_bw() + theme(panel.border = element_blank(), 
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), 
                       axis.line = element_line(colour = "black"))
  
  return(p)
}

## function 6:
## rate of force development dataframe construction
## using previously constructed force vs time dataframe
## assumes there is a column named "Force" and
## a column named "Time"
## note: Because first time point cannot have a Delta, Deltas are changed 
## to 0 instead of NAs
rfd_construction<-function(forcetime_df){
  
  dfdt_df<-forcetime_df %>% mutate(DForce=Force-lag(Force))
  dfdt_df<-dfdt_df %>% mutate(DTime=Time-lag(Time))
  dfdt_df<-dfdt_df %>% mutate(DFDT=DForce/DTime)
  dfdt_df[is.na(dfdt_df)]<-0
  return(dfdt_df)
  
}

## function 7:
## find the time of the force frame prior to a value =/= 0
## returns a df with the f1 time, force, Dtime, Dforce, and DFDT
## uses dataframe made from function 8
## change force noise estimation later to event based
find_f1<-function(dfdt_df,force_noise_estimation){
  
  dfdt_df$index<-1:nrow(dfdt_df)
  mean_time_frame<-mean(dfdt_df$DTime, na.rm=TRUE)
  f1_index<-which(dfdt_df$DFDT>force_noise_estimation/mean_time_frame)
  f1<-subset(dfdt_df,index==f1_index[1])
  ##remove the index from the max dataframe for continuity
  f1<-f1[!names(f1) %in% c("index")]
  return(f1)
  
}

## function 8:
## find rate of force development
## takes in the force time dataframe and the f1 dataframe from function 9
## returns rate of force development in whatever units are from the serial
## in this case kg/s
find_rfd<-function(forcetime_df,f1_df){
  max_force_df<-df_max(forcetime_df,"Force")
  rfd<-(max_force_df$Force-f1_df$Force)/(max_force_df$Time-f1_df$Time)
  return(rfd)
}

