source("serialsource_app.R", local=TRUE)

server <- function(input, output, session) {
  shinyjs::onclick("advanced2",shinyjs::toggle(id="advanced2", anim=TRUE))
  observe({
    
    if(input$select == "1"){
      
      shinyjs::disable(id="subjectpanel")
      shinyjs::enable(id="fileinputpanel")
      output$fileinputpanel <- renderPrint({ input$existing_data_input })
      
      if(is.null(input$existing_data_input) != TRUE){
        
        name<-str_split(toString(input$existing_data_input[1]),"-")
        test_name<-name[[1]][1]
        subject<-str_split(name[[1]][2],".csv")[[1]][1]
        csv_name<-paste(test_name,"-",subject,".csv",sep="")
        jpeg_name<-paste(test_name,"-",subject,".jpeg",sep="")
        
        exist_data <- reactiveFileReader(1000,session,
                                         filePath=toString(input$existing_data_input[4]),
                                         readFunc=read.csv)
        e_data<-exist_data()
        e_data<-e_data[!names(e_data) %in% c("X")]
        output$savecsv<-downloadHandler(
          filename=function(){
            paste(input$existing_data_input[1])
          },
          content=function(file){
            write.csv(e_data,file)
          }
        )
        
        output$table <- DT::renderDataTable({
          DT::datatable(e_data)
          
        })
        max_e_data<-df_max(e_data,"Force")
        data_dfdt<-rfd_construction(e_data)
        data_dfdt_max<-df_max(data_dfdt,"DFDT")
        data_dfdt_maxF<-df_max(data_dfdt,"Force")
        f1df<-reactiveVal()
        f1df<-find_f1(data_dfdt,input$noise)
        question_rfd<-any(is.na(f1df))
        
        if((question_rfd==FALSE) & (nrow(f1df)>0)){
          
          rfd<-find_rfd(e_data,f1df)
          
          rfd_out<-paste("The RFD is:", round(rfd,digits=2), "kg/s","or",
                         round(rfd*9.8,digits=2),"N/s",sep=" ")
          output$RFD<-renderText({rfd_out})
          
          plot_name<-paste("Force Development in",
                           test_name, ":", subject,"RFD:",
                           round(rfd,digits=2),"kg/s", sep=" ")
          p<-plot_2poi(e_data,f1df,max_e_data,plot_name,"Time","Force",
                       "Time (s)","Force (kg)","F1","Max")
          
          output$plot<-renderPlot({
            p
          })
          
          pp<-plot_2poi(data_dfdt,f1df,data_dfdt_maxF,plot_name,"Time","DFDT",
                        "Time (s)","DFDT (kg/s)","F1","Max Force")
          output$DFDT<-renderPlot({
            pp
          })
          
          output$savejpeg<-downloadHandler(
            filename=function(){paste(test_name,"-",subject,".jpeg",sep="")},
            content=function(file){
              ggsave(file,plot=p)
            }
          )
          
          output$saveDFDT<-downloadHandler(
            filename=function(){paste(test_name,"-",subject,"-DFDT",".jpeg",sep="")},
            content=function(file){
              ggsave(file,plot=pp)
            }
          )
          
        }else{
          output$RFD<-renderText("RFD did not reach threshold, try trial again")
          plot_name<-paste("Force Development in",
                           test_name, ",", subject, sep=" ")
          g<-plot_poi(e_data,max_e_data,plot_name,"Time","Force",
                      "Time (s)","Force (kg)","max")
          output$plot<-renderPlot({
            g
          })
          
          gg<-plot_poi(data_dfdt,data_dfdt_maxF,plot_name,"Time","DFDT",
                       "Time (s)","Force (kg)","max")
          output$DFDT<-renderPlot({
            gg
          })
          
          output$savejpeg<-downloadHandler(
            filename=function(){paste(test_name,"-",subject,".jpeg",sep="")},
            content=function(file){
              ggsave(file,plot=g)
            }
          )
          
          output$saveDFDT<-downloadHandler(
            filename=function(){paste(test_name,"-",subject,"-DFDT",".jpeg",sep="")},
            content=function(file){
              ggsave(file,plot=gg)
            }
          )
          
        }
      }
      
    }else{
      
      shinyjs::enable(id="subjectpanel")
      shinyjs::disable(id="fileinputpanel")
      #output$subject <- renderPrint({ input$subject })
      #output$test <- renderPrint({ input$test })
    }
  })
  
  
  
  # Initialize the timer, 5 seconds, not active.
  timer <- reactiveVal(5)
  active <- reactiveVal(FALSE)
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        print(paste("Timer for:",timer(),"s",sep=" "))
        if(timer()<=1){
          print("Serial Connection Initializing")
          con<-tryCatch(con_init()
                        ,error=function(e){
                          e$message<-paste0("Found no serial connection,
                          retry after checking connection")
                        })
        }
        
        
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed! Apply force to bar for more than 1 second."
          ))
          #write.serialConnection(con,"t")
          #data<-con_read(con(),input$seconds)
          data<-tryCatch(con_read(con,1),error=function(c){
            write.serialConnection(con, "w")
            close(con)
          }) 
          output$table <- DT::renderDataTable({
            DT::datatable(data)
          })
          
          ## make names for plots and csv file output
          subject<-reactive({
            subject<-input$subject
          })
          
          test_name_sub<-reactive({
            test_name_sub<-gsub(" ","_",input$test)
          })
          
          name<-reactive({
            name<-paste(test_name_sub(),"-",subject(),sep="")
          })
          
          jpeg_name<-reactive({
            jpeg_name<-paste(name(),".jpeg",sep="")
          })
          
          csv_name<-reactive({
            csv_name<-paste(name(),".csv",sep="")
          })
          
          output$savecsv<-downloadHandler(
            filename=function(){
              paste(csv_name())
            },
            content=function(file){
              write.csv(data,file)
            }
          )
          
          
          max_data<-df_max(data,"Force")
          data_dfdt<-rfd_construction(data)
          data_dfdt_max<-df_max(data_dfdt,"DFDT")
          data_dfdt_maxF<-df_max(data_dfdt,"Force")
          f1df<-reactiveVal()
          f1df<-find_f1(data_dfdt,input$noise)
          question_rfd<-any(is.na(f1df))
          if((question_rfd==FALSE)&(nrow(f1df)>0)){
            
            rfd<-find_rfd(data,f1df)
            
            rfd_out<-paste("The RFD is:", round(rfd,digits=2), "kg/s","or",
                           round(rfd*9.8,digits=2),"N/s",sep=" ")
            output$RFD<-renderText({rfd_out})
            
            plot_name<-paste("Force Development in",
                             input$test, ":", input$subject,"RFD:",
                             round(rfd,digits=2),"kg/s", sep=" ")
            p<-plot_2poi(data,f1df,max_data,plot_name,"Time","Force",
                         "Time (s)","Force (kg)","F1","Max")
            
            output$plot<-renderPlot({
              p
            })
            
            pp<-plot_2poi(data_dfdt,f1df,data_dfdt_maxF,plot_name,"Time","DFDT",
                          "Time (s)","DFDT (kg/s)","F1","Max Force")
            output$DFDT<-renderPlot({
              pp
            })
            
            output$savejpeg<-downloadHandler(
              filename=function(){paste(test_name_sub(),"-",subject(),".jpeg",sep="")},
              content=function(file){
                ggsave(file,plot=p)
              }
            )
            
            output$saveDFDT<-downloadHandler(
              filename=function(){paste(test_name,"-",subject,"-DFDT",".jpeg",sep="")},
              content=function(file){
                ggsave(file,plot=pp)
              }
            )
            
          }else{
            print("no rfd")
            output$RFD<-renderText("RFD did not reach threshold, try trial again")
            plot_name<-paste("Force Development in",
                             input$test, ",", input$subject, sep=" ")
            g<-plot_poi(data,max_data,plot_name,"Time","Force",
                        "Time (s)","Force (kg)","max")
            output$plot<-renderPlot({
              g
            })
            
            gg<-plot_poi(data_dfdt,data_dfdt_maxF,plot_name,"Time","DFDT",
                         "Time (s)","Force (kg)","max")
            output$DFDT<-renderPlot({
              gg
            })
            
            output$savejpeg<-downloadHandler(
              filename=function(){paste(test_name_sub(),"-",subject(),".jpeg",sep="")},
              content=function(file){
                ggsave(file,plot=g)
              }
            )
            
            output$saveDFDT<-downloadHandler(
              filename=function(){paste(test_name_sub(),"-",subject(),"-DFDT",".jpeg",sep="")},
              content=function(file){
                ggsave(file,plot=gg)
              }
            )
            
          }
        }
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(input$seconds)})
  observeEvent(input$tare,{try(write.serialConnection(con, "t"))})
  observeEvent(input$refresh,{shinyjs::js$refresh()})
  
  
  
}