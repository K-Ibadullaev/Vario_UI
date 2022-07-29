library(shiny)
library(gmGeostats)
library(ggplot2)
library(magrittr)
library(dplyr)
library(gstat)


#upload data set-----------
datas =read.csv("U:/Geostats/gmGeostats/data/Ibadullaev.csv",stringsAsFactors = T) # load data
datas = datas[,-1] # clip 1st col

#these are numeric variables
datas[,c(3,4,5,7,8)]




ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      # fileInput("upload", NULL, accept = c(".csv", ".tsv")),
      numericInput("num", "number of structures", value = 1, min = 1, max = 3),
      selectInput("variable", "Variable", choices = colnames(datas[,c(3,4,5,7,8)])),
      sliderInput("cutoff","Cutoff", min = 1, max = max(as.matrix(dist(datas[,c(1,2)])))/2,value = 225),
      sliderInput("width","Width", min = 1, max = 200,value = 11.25),
      sliderInput("nugget","Nugget", min = 0,step = 0.01, max = 1000,value = 225),
      sliderInput("alpha","Anisotropy angle", min = 0, max = 270,value=0),
      sliderInput("ratio","Ratio", min = 0, max = 1,value = 1),
      # FluidRow(uiOutput("dynamic_params"))
      
      
      ##### try
     fluidRow( 
               column(4,uiOutput("dynamic_models")),
               column(4,uiOutput("dynamic_ranges")),
                      column(4,uiOutput("dynamic_sills"))
                )
      
          ),
    mainPanel(
      plotOutput("varioplot")
    )
  )
)
server <- function(input, output,session)
  {

  
  
  
  ### Dynamic UI working!-----
  # output$dynamic_params <- renderUI({
  #   
  #   num <- as.integer(input$num)
  #   lapply(1:num,function(i) {
  #     
  #     fluidRow(
  #     column(4,selectInput(inputId = paste("model",i),label=paste("Model",i),
  #                          choices = c("Exponential"="Exp","Wave"="Wav","Gaussian"="Gau","Spherical"= "Sph"))),
  #     column(4,sliderInput(inputId = paste("sill",i),label=paste("Sill",i),
  #                          min = 0, step = 0.01,max = var(datas[,input$variable])*1.5,value = 0)),
  #     column(4,sliderInput(inputId = paste("range",i),label=paste("Range",i),
  #                          min = 0, step = 0.01,max = max(as.matrix(dist(datas[,c(1,2)]))),value = 225)) 
  #     )
  #   })
  #   
  # })
  
  
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
  
  # vgt= reactive( {
  #   vgm(model=input$model, nugget=input$nugget, range=input$range, psill=input$sill,anis = c(input$angle, input$ratio)) %>%
  #              lapply(2:as.integer(input$num),function(i){ 
  #                {vgm(model=input[[paste0("model",i)]], range=input[[paste0("range",i)]], psill=input[[paste0("input$sill",i)]], add.to=.)}
  #                } )
  #              
  #              })
  
  vgt= reactive(vgm(model=input$model1, nugget=input$nugget,range=input$range1, psill=input$sill1, anis=c(input$alpha,input$ratio) ) %>%
                  variogramLine(maxdist = input$cutoff))
  
  
  ## plotting ---------
  #with ggplot
  output$varioplot <- renderPlot({
    
    ggplot() +
      geom_point(data=vg(),aes(dist, gamma, colour = 2)) +
      geom_line(data=variogramLine(vgt(),maxdist = input$cutoff),aes(dist, gamma, colour = 2))+
      labs(y = "semivariogram",x="distance")
  }, res = 96)
  
  
  
  
  
} 
  
  
 





# Run the application 
shinyApp(ui = ui, server = server)












