


library(shiny)
library(gmGeostats)
library(ggplot2)
library(magrittr)
library(dplyr)
library(gstat)


#upload data set-----------
# datas =read.csv("U:/Geostats/gmGeostats/data/Ibadullaev.csv",stringsAsFactors = T) # load data
# datas = datas[,-1] # clip 1st col
# 
# #these are numeric variables
# datas[,c(3,4,5,7,8)]




ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
     fileInput("file", NULL, accept = c(".csv", ".tsv")),
      
     sliderInput("nugget","Nugget", min = 0,step = 0.01, max = 1000,value = 225),
     uiOutput("dynamic_params"),
     uiOutput("selectcoords"),
     uiOutput("selectcolumns"),
      
      
      ##### try
      fluidRow( 
        column(4,uiOutput("dynamic_models")),column(4,uiOutput("dynamic_ranges")),
        column(4,uiOutput("dynamic_sills"))
      )
      
    ),
    mainPanel(
     uiOutput("Tabsoutput")
                  
                  
                  )
      
    )
  )

server <- function(input, output,session)
{
  #dynamic data frame
  datas <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file = file1$datapath, sep = ',', header = TRUE, stringsAsFactors = F)
   
  })
  
  
# tabsets  
  output$Tabsoutput =renderUI({
    tabsetPanel(id="tabs",
                tabPanel("DATA", tableOutput("showdata")), 
                tabPanel("PLOT",plotOutput("varioplot")))
    
    
  })
  
  #
  output$showdata =renderTable(datas() %>% head())
  
  #select coords
  output$coords =renderUI({selectInput("coords", "Choose columns with spatial coordinates",  multiple = T,choices = colnames(datas()))})
  
  
  #select data columns
  output$selectcolumns=renderUI({
   selectInput("allvariables", "Select variables of interest",  multiple = T,choices = colnames(datas()))
   
  })
  
  
  
  
  ### Dynamic UI working!-----
  output$dynamic_params <- renderUI({
   
    
     fluidRow(
     column(3, numericInput("num", "number of structures", value = 1, min = 1, max = 3)),
     column(3,selectInput("variable", "Variable", choices = colnames(input$allvariables))),
     column(3, sliderInput("cutoff","Cutoff", min = 1, max = max(as.matrix(dist(input$coords)))/2,value = 225)),
     column(3,sliderInput("width","Width", min = 1, max = 200,value = 11.25))
       )
    })
  
  
  ####### dynamic models-----------
  output$dynamic_models <- renderUI({
    
    num <- as.integer(input$num)
    lapply(1:num,function(i) {
      
      selectInput(inputId = paste("model",i),label=paste("Model",i),
                  choices = c("Exponential"="Exp","Wave"="Wav","Gaussian"="Gau","Spherical"= "Sph"))
      
    })
  })
  
  
  ####### dynamic ranges-----------
  output$dynamic_ranges <- renderUI({
    
    num <- as.integer(input$num)
    lapply(1:num,function(i) {
      
      sliderInput(inputId = paste("range",i),label=paste("Range",i),
                  min = 0, step = 0.01,max = max(as.matrix(dist(datas[,c(1,2)]))),value = 225) 
      
      
    })
  })
  ###### dynamic sills-----
  output$dynamic_sills <- renderUI({
    
    num <- as.integer(input$num)
    lapply(1:num,function(i) {
      
      sliderInput(inputId = paste("sill",i),label=paste("Sill",i),
                  min = 0, step = 0.01,max = var(datas[,input$variable])*1.5,value = var(datas[,input$variable])*1.5*0.01)
      
    })
  })
  
  
  
  
  ##  #empirical variogram ------------
  
  gs = reactive({
    frm = as.formula(paste("log(", input$variable, ")~1", sep=""))
    gstat(id=input$variable, formula=frm, locations = ~X+Y, data=datas)
  }) 
  
  ## configured empirical variogram ---------
  vg = reactive(variogram(gs(), cutoff=input$cutoff, width=input$width) )
  
  
  ## theoretical variogram--------
  
  # vgt= reactive(vgm(model=input$model, nugget=input$nugget,range=input$range, psill=input$sill, anis=c(input$alpha,input$r) ) %>%
  #                 variogramLine(maxdist = input$cutoff))
  
  # vgt = reactive(vgm(model=input$model, nugget=input$nugget, range=input$range, psill=input$sill) %>%
  #                  {vgm(model=input$model1, range=input$range1, psill=input$sill1, add.to=.)})
  
  vgt= reactive( {
    vgm(model=input$model, nugget=input$nugget, range=input$range, psill=input$sill) %>%
      lapply(2:as.integer(input$num),function(i){ 
        {vgm(model=input[[paste0("model",i)]], range=input[[paste0("range",i)]], psill=input[[paste0("input$sill",i)]], add.to=.)}
      } )
    
  })
  
  
  ## plotting ---------
  #with ggplot
  output$varioplot <- renderPlot({
    
    ggplot() +
      geom_point(data=vg(),aes(dist, gamma, colour = 2)) +
      geom_line(data=vgt()%>%variogramLine(maxdist = input$cutoff),aes(dist, gamma, colour = 2))+
      labs(y = "semivariogram",x="distance")
  }, res = 96)
  
  
  
  
  
} 








# Run the application 
shinyApp(ui = ui, server = server)












