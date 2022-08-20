library(shiny)
library(gmGeostats)
source("spSupport.R")
library(magrittr) 
library(dplyr)
library(gstat)
library(scico)
library(ggplot2)
library(plotly)




ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "filedata",
                label = "Upload data. Choose csv file",
                accept = c(".csv")),
      numericInput("num", "Number of structures", value = 1, min = 1, max = 3),
      uiOutput("Xcoords"),
      uiOutput("Ycoords"),
      uiOutput("var_chem"),
      uiOutput("cutoff"),
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
                  
                  tabPanel("Kriging", plotly::plotlyOutput("krig_res")),
                  
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
  ### dynamic data frame--------------
  datas <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
  })
  
  
  
  
  ### coordinates------
  output$Xcoords <- renderUI({
    selectInput("Xcoords", "Easting",  choices = colnames(datas()))
  }) 
  output$Ycoords <- renderUI({
    selectInput("Ycoords", "Northing",  choices = colnames(datas()))
  }) 
  
  ### variable of interest--------
  output$var_chem <- renderUI({
    selectInput("var_chem", "Variable of interest",  choices = colnames(datas()))
  }) 
  ### subset the data w.r.t variable and coordinates------  
  selected_data = reactive({
    datas() %>% select(input$Xcoords,input$Ycoords,input$var_chem)
  })   
  
  
  ### cutoff---------
  output$cutoff<-renderUI({
    req(selected_data())
    sliderInput("cutoff","Cutoff", min = 1, 
                max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value = 225)
  })
  
  
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
                  min = 0, step = 0.01,
                  max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value = 225) 
      
      
    })
  })
  ###### dynamic sills-----
  output$dynamic_sills <- renderUI({
    
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("sill",i),label=paste("Sill",i),
                  min = 0, step = 0.01,max = var(selected_data()%>%select(input$var_chem))*1.5,value = var(selected_data()%>%select(input$var_chem))*1.5*0.01)
      
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
    frm = as.formula(paste("log(", input$var_chem, ")~1", sep=""))
    location_frm = as.formula(paste( "~X+Y", sep=""))
    gstat(id=input$var_chem, formula=frm, locations = location_frm , data=selected_data())
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
  
  ########## kriging----------
  
  
  # 
  # xy_grid = reactive{
  #   rangesXY = selected_data() %>% select(input$Xcoords,input$Ycoords) %>% sapply(range)
  #   x = seq(from=rangesXY[1,input$Xcoords]-20, to=rangesXY[2,input$Xcoords]+20, by=10)
  #   y = seq(from=rangesXY[1,input$Ycoords]-20, to=rangesXY[2,input$Ycoords]+20, by=10)
  #   expand.grid(X=x, Y=y)
  #   }
  # 
  # #cross-validation
  # # xv_Co = gstat.cv(gs(), nfold=nrow(selected_data()))
  # kriged = reactive(
  #   {
  #     req(gs, xy_grid)
  #     gs_krig =gstat(id=input$var_chem, formula=frm, locations = location_frm , data=selected_data(), model=vgt_Ni ,
  #                    # neighbourhood parameters: see ?gstat help
  #                    nmax = 20,    # maximum number of datapoints => increase if variogram has oscillations
  #                    omax = 6,    # maximum nr of datapoints per octant/quadrants 
  #                    nmin = 10,    # minimum nr of datapoints (otherwise return NA)
  #                    maxdist = 20, # maximum distance to seek for datapoints
  #                    force = TRUE  # ignore maxdist if nmin is not satisfied?
  #     )
  #     predict(gs(), newdata=xy_grid(), debug.level=-1)
  #     })
  # 
  # kplot=ggplot(kriged_Co, aes(X, Y))+
  #   geom_raster(aes(fill=Co.pred))+
  #   coord_fixed()+
  #   theme_bw()+
  #   ggtitle("Results of local kriging for Log(Co)")+
  #   scico::scale_fill_scico(palette = "roma",direction = -1)
  # 
  
  ################
  
  
  ### render data frame ------------
  output$datatabshow <- DT::renderDT({    
    selected_data()
    
    
  })
  
  
  

  ## plotting ---------
  #with ggplot
  output$varioplot <-  plotly::renderPlotly({
    
    ggplot() +
      ggtitle(paste0("Variogram of ", input$var_chem))+
      
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
      
      vrbl = paste0("log(",input$var_chem,")")
      ggplot(selected_data) +
        geom_point(aes_string(x="Y", y=vrbl )) +
        theme_bw() +
        ggtitle(paste0(" Log(",input$var_chem,")", " vs Y"))+
        geom_smooth(aes_string(x="Y", y=vrbl ), method=loess, col=2)
    }
    
    
    
  )
  
  output$swathE = plotly::renderPlotly(
    {
      vrbl = paste0("log(",input$var_chem,")")
      ggplot(selected_data) +
        geom_point(aes_string(x="X", y=vrbl )) +
        theme_bw() +
        ggtitle(paste0(" Log(",input$var_chem,")", " vs X") )+
        geom_smooth(aes_string(x="X", y=vrbl ), method=loess, col=2)}
  )
  
  
  output$krig_res = plotly::renderPlotly({
    
 
  })
  
  
  
  
  
  
  
  
  
  
} 



shinyApp(ui = ui, server = server)