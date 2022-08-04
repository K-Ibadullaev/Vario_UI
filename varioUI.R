library(shiny)
library(gmGeostats)
source("spSupport.R")
library(magrittr) 
library(dplyr)
library(gstat)

library(scico)
library(ggplot2)
library(plotly) #interactive plots
# plotly::plotlyOutput()
# plotly::renderPlotly({})

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
      
      
          ),
    mainPanel(
      ##### try
      tabsetPanel(id="Mainpanel",
                  
                  tabPanel("Data set",
                           DT::DTOutput('datatabshow')
                           
                           ),
                  
                  tabPanel("Variogram plot",
                           
                             fluidRow( 
                               
                               column(2,uiOutput("dynamic_models")),
                               column(3,uiOutput("dynamic_ranges")),
                               column(3,uiOutput("dynamic_sills")),
                               column(2,uiOutput("dynamic_alpha")),
                               column(2,uiOutput("dynamic_ratio"))
                               ),
                               plotly::plotlyOutput("varioplot")
                           
                           
                           ),
                  
                  tabPanel("Variogram surface"),
                  
                  tabPanel("Swath plots",
                           plotly::plotlyOutput("swathN"),
                           plotly::plotlyOutput("swathE")
                           
                           )
                  
                  
                  
                  )
      
      
      )
      
    )
  )




server <- function(input, output,session)
  {

  
  
  
  
  
  
  ####### dynamic models-----------
  output$dynamic_models <- renderUI({
    
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      selectInput(inputId = paste0("model",i),label=paste("Model",i),
                     choices = c("Exponential"="Exp","Wave"="Wav","Gaussian"="Gau","Spherical"= "Sph"))
      
    })
  })
  
  
  ####### dynamic ranges-----------
  output$dynamic_ranges <- renderUI({
    
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("range",i),label=paste("Range",i),
                             min = 0, step = 0.01,max = max(as.matrix(dist(datas[,c(1,2)]))),value = 225) 
    
    
                })
  })
  ###### dynamic sills-----
  output$dynamic_sills <- renderUI({
    
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("sill",i),label=paste("Sill",i),
                                           min = 0, step = 0.01,max = var(datas[,input$variable])*1.5,value = var(datas[,input$variable])*1.5*0.01)
      
    })
  })
  
  ####### dynamic anisotropy-----------
  output$dynamic_alpha <- renderUI({
    
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("alpha",i),label=paste("Anisotropy angle",i), min = 0, max = 270,value=0)
      
    })
  })
  
  output$dynamic_ratio <- renderUI({
    
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("ratio",i),label=paste("Ratio",i), min = 0, max = 1,value = 1)
    })
  })
  

  ##  empirical variogram ------------
  
  gs = reactive({
    frm = as.formula(paste("log(", input$variable, ")~1", sep=""))
    gstat(id=input$variable, formula=frm, locations = ~X+Y, data=datas)
  }) 
  
  ## configured empirical variogram ---------
  vg = reactive(variogram(gs(), cutoff=input$cutoff, width=input$width) )
 
  
  ## theoretical variogram--------
  
  
  vgt= reactive( {
    
     
  res = vgm(model=input$model1, nugget=input$nugget, range=input$range1, psill=input$sill1,
      anis = c(input$alpha1, input$ratio1))
    if(input$num>1){
      for (i in 2:as.integer(input$num)) {
        res = vgm(model=input[[paste0("model",i)]], range=input[[paste0("range",i)]], psill=input[[paste0("sill",i)]],
                  anis = c(input[[paste0("alpha",i)]], input[[paste0("ratio",i)]]),add.to=res)
        
      }
      
     
    }
    res
   
  
})

  ### render data frame ------------
  output$datatabshow <- DT::renderDT({    
    datas %>%      select(c(1,2),input$variable)
    
    
    })
  
  
  
  
  ## plotting ---------
  #with ggplot
  output$varioplot <-  plotly::renderPlotly({

    ggplot() +
      ggtitle(paste0("Variogram of ", input$variable))+
     
      # theme_classic()+
      theme_light()+ 
      geom_point(data=vg(),aes(dist, gamma, colour = 2)) +
       geom_line(data=variogramLine(object=vgt(),maxdist = input$cutoff),aes(dist, gamma, colour = 2))+
      labs(y = "semivariogram",x="distance")+
      theme(legend.position = "none")
  })
  
  
  ##swath plots----------


  output$swathN = plotly::renderPlotly(
    {
      
      vrbl = paste0("log(",input$variable,")")
      ggplot(datas) +
          geom_point(aes_string(x="Y", y=vrbl )) +
          theme_bw() +
          ggtitle(paste0(" Log(",input$variable,")", " vs Y"))+
          geom_smooth(aes_string(x="Y", y=vrbl ), method=loess, col=2)
      }



  )

  output$swathE = plotly::renderPlotly(
    {
      vrbl = paste0("log(",input$variable,")")
      ggplot(datas) +
          geom_point(aes_string(x="X", y=vrbl )) +
          theme_bw() +
          ggtitle(paste0(" Log(",input$variable,")", " vs X") )+
          geom_smooth(aes_string(x="X", y=vrbl ), method=loess, col=2)}
    )


# output$variosurf = plotly::renderPlotly({
#   
#   image.polargrid(z = gs())
#   
#   
# })
    
  
  
  
  
  

  
  
  
} 
  
  
 





# Run the application 
shinyApp(ui = ui, server = server)











