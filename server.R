source("serialsource_app.R", local=TRUE)

server <- function(input, output, session) {
  shinyjs::onclick("advanced2",shinyjs::toggle(id="advanced2", anim=TRUE))
  observe({
    
    if(input$select == "1"){
      
      shinyjs::disable(id="subjectpanel")
      shinyjs::enable(id="fileinputpanel")
      output$fileinputpanel <- renderPrint({
        if(!is.null(input$existing_data_input) && !is.null(input$compare_data_input)){
          rbind(input$existing_data_input,input$compare_data_input)
        }else if(!is.null(input$existing_data_input)){
          input$existing_data_input
        }else{
          input$compare_data_input
        }
        
        })
      
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
          
          rfd_out<-paste("The Existing RFD is:", round(rfd,digits=2), "kg/s","or",
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
          
          if(is.null(input$compare_data_input) != TRUE){
            
            compare_data <- reactiveFileReader(1000,session,
                                               filePath=toString(input$compare_data_input[4]),
                                               readFunc=read.csv)
            c_data<-compare_data()
            c_data<-c_data[!names(c_data) %in% c("X")]
            
            max_c_data<-df_max(c_data,"Force")
            data_dfdt2<-rfd_construction(c_data)
            data_dfdt2_max<-df_max(data_dfdt2,"DFDT")
            data_dfdt2_maxF<-df_max(data_dfdt2,"Force")
            f1df2<-reactiveVal()
            f1df2<-find_f1(data_dfdt2,input$noise)
            question_rfd2<-any(is.na(f1df2))
            if(nrow(f1df2)>0){
              rfd2<-find_rfd(c_data,f1df2)
              rfd2_out<-paste("The Comparison RFD is:", round(rfd2,digits=2), "kg/s","or",
                             round(rfd2*9.8,digits=2),"N/s",sep=" ")
              output$RFD<-renderText({paste(rfd_out,rfd2_out)})
            }
            pp_compare<-plot_2poi_compare(e_data,max_e_data,f1df,
                              c_data,max_c_data,f1df2,
                              "Force Development Comparison","Time","Force",
                              "Time (s)","Force (kg)","Max","F1")
            output$plot2<-renderPlot({
              pp_compare
            })
            
            output$savecompare<-downloadHandler(
              filename=function(){paste(test_name,"-",subject,"compare.jpeg",sep="")},
              content=function(file){
                ggsave(file,plot=pp_compare)
              }
            )
          }
          
          output$info <- renderPrint({
            if(!is.null(input$plot_brush)){
              brushedPoints(e_data, input$plot_brush, xvar="Time", yvar="Force")
            } else{
              nearPoints(e_data,xvar="Time",yvar="Force",input$plot_click,threshold = 10, maxpoints=1)
            }
            
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
          
          if(is.null(input$compare_data_input) != TRUE){
            
            compare_data <- reactiveFileReader(1000,session,
                                               filePath=toString(input$compare_data_input[4]),
                                               readFunc=read.csv)
            c_data<-compare_data()
            c_data<-c_data[!names(c_data) %in% c("X")]
            
            max_c_data<-df_max(c_data,"Force")
            data_dfdt2<-rfd_construction(c_data)
            data_dfdt2_max<-df_max(data_dfdt2,"DFDT")
            data_dfdt2_maxF<-df_max(data_dfdt2,"Force")
            f1df2<-reactiveVal()
            f1df2<-find_f1(data_dfdt2,input$noise)
            question_rfd2<-any(is.na(f1df2))
            pp_compare<-plot_2poi_compare(e_data,max_e_data,f1df,
                                c_data,max_c_data,f1df2,
                                "Force Development Comparison","Time","Force",
                                "Time (s)","Force (kg)","Max","F1")
            output$plot2<-renderPlot({
              pp_compare
            })
            output$savecompare<-downloadHandler(
              filename=function(){paste(test_name,"-",subject,"compare.jpeg",sep="")},
              content=function(file){
                ggsave(file,plot=pp_compare)
              }
            )
          }
          
          output$info <- renderPrint({
            if(!is.null(input$plot_brush)){
              brushedPoints(e_data, input$plot_brush, xvar="Time", yvar="Force")
            } else{
              nearPoints(e_data,xvar="Time",yvar="Force",input$plot_click,threshold = 10, maxpoints=1)
            }
            
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
            div(tags$b("GO!", style = "color: green; font-size: 288px;"))
            , easyClose = TRUE
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
            
            if(is.null(input$compare_data_input) != TRUE){
              
              compare_data <- reactiveFileReader(1000,session,
                                               filePath=toString(input$compare_data_input[4]),
                                               readFunc=read.csv)
              c_data<-compare_data()
              c_data<-c_data[!names(c_data) %in% c("X")]
              
              max_c_data<-df_max(c_data,"Force")
              data_dfdt2<-rfd_construction(c_data)
              data_dfdt2_max<-df_max(data_dfdt2,"DFDT")
              data_dfdt2_maxF<-df_max(data_dfdt2,"Force")
              f1df2<-reactiveVal()
              f1df2<-find_f1(data_dfdt2,input$noise)
              question_rfd2<-any(is.na(f1df2))
              if(nrow(f1df2)>0){
                rfd2<-find_rfd(c_data,f1df2)
                rfd2_out<-paste("The Comparison RFD is:", round(rfd2,digits=2), "kg/s","or",
                                round(rfd2*9.8,digits=2),"N/s",sep=" ")
                output$RFD<-renderText({paste(rfd_out,rfd2_out)})
              }
              pp_compare<-plot_2poi_compare(data,max_data,f1df,
                                c_data,max_c_data,f1df2,
                                plot_name,"Time","Force",
                                "Time (s)","Force (kg)","Max","F1")
              
              output$plot2<-renderPlot({
                pp_compare
              })
              
              output$savecompare<-downloadHandler(
                filename=function(){paste(test_name_sub(),"-",subject(),"compare.jpeg",sep="")},
                content=function(file){
                  ggsave(file,plot=pp_compare)
                }
              )
            }
            
            output$info <- renderPrint({
              if(!is.null(input$plot_brush)){
                brushedPoints(data, input$plot_brush, xvar="Time", yvar="Force")
              } else{
                nearPoints(data,xvar="Time",yvar="Force",input$plot_click,threshold = 10, maxpoints=1)
              }
              
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
            
            if(is.null(input$compare_data_input) != TRUE){
              
              compare_data <- reactiveFileReader(1000,session,
                                                 filePath=toString(input$compare_data_input[4]),
                                                 readFunc=read.csv)
              c_data<-compare_data()
              c_data<-c_data[!names(c_data) %in% c("X")]
              
              max_c_data<-df_max(c_data,"Force")
              data_dfdt2<-rfd_construction(c_data)
              data_dfdt2_max<-df_max(data_dfdt2,"DFDT")
              data_dfdt2_maxF<-df_max(data_dfdt2,"Force")
              f1df2<-reactiveVal()
              f1df2<-find_f1(data_dfdt2,input$noise)
              question_rfd2<-any(is.na(f1df2))
              
              pp_compare<-plot_2poi_compare(data,max_data,f1df,
                                            c_data,max_c_data,f1df2,
                                            plot_name,"Time","Force",
                                            "Time (s)","Force (kg)","Max","F1")
              
              output$plot2<-renderPlot({
                pp_compare
              })
              
              output$savecompare<-downloadHandler(
                filename=function(){paste(test_name_sub(),"-",subject(),"compare.jpeg",sep="")},
                content=function(file){
                  ggsave(file,plot=pp_compare)
                }
              )
            }
            
            output$info <- renderPrint({
              if(!is.null(input$plot_brush)){
                brushedPoints(data, input$plot_brush, xvar="Time", yvar="Force")
              } else{
                nearPoints(data,xvar="Time",yvar="Force",input$plot_click,threshold = 10, maxpoints=1)
              }
              
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