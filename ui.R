library(shiny)
library(shinyjs)
jsCode <- "shinyjs.refresh = function() { location.reload(); }"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions=c("refresh")),
  titlePanel("Force Development Test"),
  fluidRow(
    column(2,id="datachoicepanel",
           selectInput("select", label = h3("Data Choices"),
                                       choices = list("Existing Data" = 1, 
                                                      "New Data" = 2),
                                       selected = 1),
           helpText("Instructions: Choose data input. If choosing existing data, 
                    choose a .csv with time and force columns. If reading new 
                    data, fill in subject information. When subject is ready to 
                    exert force, press the start button on the timer. Tare the 
                    weight when subject is relaxed but at ready during the 
                    countdown. Subject should pull as hard as possible and 
                    then relax when finished.")
    ),
    column(3, id="subjectpanel", h3("Subject"),
           actionButton('start','Start'),
           actionButton('stop','Stop'),
           actionButton('reset','Reset'),
           actionButton('tare','Tare'),
           hr(),
           numericInput('seconds','Timer in seconds:',
                        value=5,min=0,max=10,step=1),
           h2(textOutput('timeleft')),
                helpText("Fill in subject
                and test information if new data input is chosen"),
                textInput("subject", label = h5("Enter Subject Name:"),
                          value = Sys.info()["user"]),
                #verbatimTextOutput("subject"),
                hr(),
                textInput("test",label = h5("Enter Test Name:"),
                          value = "FDIP"),
                #verbatimTextOutput("test"),
                hr()
    ),
    column(7, h3("Visualization"),
           tabsetPanel(
             tabPanel("Plot",plotOutput("plot")),
             tabPanel("Table",DT::dataTableOutput("table")),
             tabPanel("DFDT",plotOutput("DFDT"))
             )
    )
    
  ),
  fluidRow(
    column(2,id="fileinputpanel",
           helpText("Only if existing data input is chosen"),
           fileInput("existing_data_input", 
                     label=h5("Browse existing data in .csv format"))),
    column(3,hr(),
           downloadButton("savecsv", label = "Save Data"),
           downloadButton("savejpeg", label = "Save Graph"),
           downloadButton("saveDFDT", label = "Save DFDT"),
           actionButton("refresh",label = "Refresh Page"),
           hr()
           ),
    column(3,helpText("RFD output if available. If not available, 
                      lower noise."),
           numericInput("noise", "RFD noise in kg:", 
                        value=4,min=1,max=10,step=1),
           textOutput("RFD"))
    
  ),
  fluidRow(
    column(2,helpText("File Upload Status:"),
           verbatimTextOutput("fileinputpanel")),
    hr()
  )
  
)