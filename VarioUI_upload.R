library(shiny)
library(gmGeostats)
source("spSupport.R")
library(magrittr) 
library(dplyr)
library(gstat)
library(scico)
library(ggplot2)
library(plotly)
library(rlang)






## UI of the APP----------

#> Define widgets
ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      #> Display the upload widget
      fileInput(inputId = "filedata",
                label = "Upload data. Choose CSV file",
                accept = c(".csv",".tsv")),
      #> Display input for nested structures
      numericInput("num", "Number of structures", value = 1, min = 1, max = 3),
      
      #> Display dynamic ui of spatial coordinates
      uiOutput("Xcoords"),
      uiOutput("Ycoords"),
      
      #> Define widgets or variable of interest, e.g numeric/categorical
      fluidRow(
        column(4,uiOutput("var_chem")),
        column(4,uiOutput("indicator_box")), 
        column(4,uiOutput("indicator_list"))),
      
      #> Display cutoff of the variogram
      uiOutput("cutoff"),
      
      #> Display width/lag
      sliderInput("width","Width", min = 1, max = 200,value = 11.25),
      
      #> Display nugget 
      sliderInput("nugget","Nugget", min = 0,step = 0.01, max = 1000,value = 225),
      
      
    ),
    mainPanel(
      
      tabsetPanel(id="Mainpanel",
                  #> Display Data
                  tabPanel("Data set",
                           DT::DTOutput('datatabshow')
                           
                  ),
                  #> Display Variogram
                  tabPanel("Variogram plot",
                           
                           fluidRow( 
                             column(2,uiOutput("dynamic_models")),
                             column(3,uiOutput("dynamic_ranges")),
                             column(3,uiOutput("dynamic_sills")),
                             column(2,uiOutput("dynamic_alpha")),
                             column(2,uiOutput("dynamic_ratio"))
                           ),
                           plotly::plotlyOutput("varioplot"),
                           fluidRow(
                                    column(2,actionButton("save_model", "Save model")),
                                    column(2,actionButton("autofit", "Fit model")))
                           
                           
                           
                  ),
                  #> Tabpanel of swath plots
                  tabPanel("Swath plots",
                           plotly::plotlyOutput("swathN"),
                           plotly::plotlyOutput("swathE")),
                  #> Tabpanel of kriging
                  tabPanel(
                        "Kriging",
                        waiter::use_waiter(),
                        fluidRow(
                          column(4,
                                 fluidRow( sliderInput("nmax","Number of nearest observations", min = 10, max = 300,value = 50)),
                                 
                                 fluidRow( uiOutput("nmin")),
                                 # fluidRow( sliderInput("nmin","Minimal number of nearest observations", min = 10, max = 100,value = 50)),
                                 
                                 
                                 fluidRow( sliderInput("omax","Maximum number of observations to select per octant (3D) or quadrant (2D);", min = 1, max = 100,value = 6)),
                                 
                                 fluidRow( uiOutput("maxdist")),
                                 # fluidRow( sliderInput("maxdist","Maximal distance between 2 observations", min = 1, max = 200,value = 20)),
                                 fluidRow(actionButton("kriging_btn", "Kriging", value = F))
                                 
                                 ),
                          column(8,plotly::plotlyOutput("krig_res"))
                          
                          )
                         
                        
                        
                        
                        
                        
                        
                       
                     
                  ),
                  #> Tabpanel of simulations
                  tabPanel(
                    "Simulations",
                    waiter::use_waiter(),
                    fluidRow(
                      column(5,sliderInput("nsim","Select number of simulations", min = 1, 
                                           step = 1,   max = 100,value = 1)),
                      column(5,uiOutput("dispnsim")),
                      column(2,actionButton("sim_btn", "Simulate", value = F))
                    ),
                    
                    plotly::plotlyOutput("sim_res")
                    
                  )
                  
                  
                  
      )
      
      
    )
    
  )
)



## Server of the APP ------------
server <- function(input, output,session)
{
  ### dynamic data frame--------------
  datas <- reactive({
    req(input$filedata)
    ### check extension
    ext = tools::file_ext(input$filedata$name)
    switch (ext,
      csv = vroom::vroom(input$filedata$datapath,delim = ",", col_names = T),
      tsv = vroom::vroom(input$filedata$datapath,delim = "\t", col_names = T),
      {
        showModal(modalDialog(
          title = "Warning",
          "Please, upload a .csv or a .tsv file",
          easyClose = T
        ))
        validate("Invalid file. Please, upload a .csv or a .tsv file",cancelOutput = TRUE)
        }
    )
    
    
    
  })
  
  ### indicator box-----
  output$indicator_box <- renderUI({
    checkboxInput("indicator_box", "Indicator", value = F)
  })
  
  ### indicator list-----
  output$indicator_list <- renderUI({
    req(input$indicator_box,input$var_chem)
    selectInput("indicator_list", "Indicator variable", choices = selected_data() %>% select(input$var_chem)%>% unique) 
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
    if(input$indicator_box){
      datas() %>% select(input$Xcoords,input$Ycoords,input$var_chem) 
    }else{
      datas() %>% select(input$Xcoords,input$Ycoords,input$var_chem)
      
    }
    
    
    
  })   
  
  
  ### variogram cutoff---------
  output$cutoff<-renderUI({
    req(selected_data())
    sliderInput("cutoff","Cutoff", min = 1, 
                max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/4)
  })
  
  ### maxdist-------
  
  # Is it cutoff or width/lag ???
  output$maxdist<-renderUI({
    req(selected_data())
    sliderInput("maxdist","Maximal distance between 2 observations", min = 1,
                max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value =max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/4)
  })
  
  
  ### nmin--------
  output$nmin = renderUI({
    
    sliderInput("nmin","Minimal number of nearest observations", min = 10, max = input$nmax,value = input$nmax*0.5)
    
  })
  
  ### number of simulation-------
  output$dispnsim = renderUI({
    if(input$nsim>1){
      sliderInput("dispnsim","Display n-th simulation", min = 1, 
                  max = input$nsim,value = 1, step = 1)
      
    }
    
    
  })
  ### dynamic models-----------
  output$dynamic_models <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      selectInput(inputId = paste0("model",i),label=paste("Model",i),
                  choices = c("Exponential"="Exp","Wave"="Wav","Gaussian"="Gau","Spherical"= "Sph"))
      
    })
  })
  
  
  ### dynamic ranges-----------
  output$dynamic_ranges <- renderUI({
    #> creates input widgets depending on given number of nested structures 
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("range",i),label=paste("Range",i),
                  min = 0, step = 0.01,
                  max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value = 225) 
      
      
    })
  })
  ### dynamic sills-----
  output$dynamic_sills <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("sill",i),label=paste("Sill",i),
                  min = 0,
                  step = ifelse(!input$indicator_box,
                                var(selected_data()%>%select(input$var_chem))*1.5*0.001,
                                0.001),
                  max = ifelse(!input$indicator_box,
                                                    var(selected_data()%>%select(input$var_chem))*3,
                                                    0.25) ,
                  value = ifelse(!input$indicator_box,
                    var(selected_data()%>%select(input$var_chem))*1.5*0.01,
                  0.25)
                  
      )
      
    })
  })
  
  ### dynamic anisotropy-----------
  output$dynamic_alpha <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    
    purrr::map(1:num,function(i) {
      # value <- isolate(paste0("input$alpha",i))
      sliderInput(inputId = paste0("alpha",i),label=paste("Anisotropy angle",i), min = 0, max = 270,value=0)
      
      
      
    })
  })
  
  output$dynamic_ratio <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("ratio",i),label=paste("Ratio",i), min = 0, max = 1,value = 1)
    })
  })
  
  
  
  
  
  ###  empirical variogram ------------
  #> creates empricial variogram for the vaiable of interest
  gs = reactive({
    #> for categorical variable
    if(input$indicator_box){
      ddt = selected_data() 
      frm = as.formula(paste( as.character(input$var_chem), "=='", as.character(input$indicator_list), "'~1", sep=""))
      location_frm = as.formula(paste( "~X+Y", sep=""))
      
      gstat(id=input$indicator_list, formula=frm, locations = location_frm , data=ddt)
    }else{
      #> for numeric variable
      frm = as.formula(paste("log(", input$var_chem, ")~1", sep=""))
      d =selected_data() 
      location_frm = as.formula(paste( "~X+Y", sep=""))
      gstat(id=input$var_chem, formula=frm, locations = location_frm , data=d)
    }
    
    
    
  }) 
  
  
 
  
  ### configured empirical variogram ---------
  vg = reactive(variogram(gs(), cutoff=input$cutoff, width=input$width) )
  
  
  ### theoretical variogram--------
  
  
  vgt= reactive( {
    
    
    res = vgm(model=input$model1, nugget=input$nugget, range=input$range1, psill=input$sill1,
              anis = c(input$alpha1, input$ratio1))
    if(input$num>1){
      #> evaluates variogram depending on given number of nested structures
      for (i in 2:as.integer(input$num)) {
        res = vgm(model=input[[paste0("model",i)]], range=input[[paste0("range",i)]], psill=input[[paste0("sill",i)]],
                  anis = c(input[[paste0("alpha",i)]], input[[paste0("ratio",i)]]),add.to=res)
        
      }
      
      
    }
    res
    
    
  })
  ### save model parameters------
  observeEvent(input$save_model, {
    req(vgt())
    res = structure(vgt())
    variogram_model<<-res
    # if(exists(gs_autofit) ){
    #   
    #   
    # resautofit = structure(gs_autofit())
    # autofit_variogram_model<<-resautofit
    #   
    # }
    
  })
  
  ### autofit---------- 
  # gs_autofit=eventReactive(input$autofit,{
  # 
  #   gstat::fit.variogram(object=vg(),  model = vgt())
  # })


 #

  ### kriging----------

  observeEvent(input$kriging_btn,{


    rangesXY = {selected_data() %>% select(input$Xcoords,input$Ycoords) %>% sapply(range)}


    x = {seq(from=rangesXY[1,input$Xcoords]-20, to=rangesXY[2,input$Xcoords]+20, by=10)}
    y = {seq(from=rangesXY[1,input$Ycoords]-20, to=rangesXY[2,input$Ycoords]+20, by=10)}

    xy_grid = {expand.grid(X=x, Y=y)}
    if(input$indicator_box){

            frm = as.formula(paste( as.character(input$var_chem), "=='", as.character(input$indicator_list), "'~1", sep=""))
            location_frm = as.formula(paste("~X+Y", sep=""))

            gs_krig  =  {gstat(id=input$indicator_list, formula=frm, locations = location_frm ,
                  data=selected_data(), model=vgt() , 
                  
                   nmax = input$nmax %>% as.numeric(),    # maximum number of datapoints => increase if variogram has oscillations
                   omax = input$omax %>% as.numeric(),    # maximum nr of datapoints per octant/quadrants 
                   nmin = input$nmin %>% as.numeric(), 
                   maxdist = input$maxdist %>% as.numeric(), # maximum distance to seek for datapoints
                   force = T
               
                  
                  
                  
                  )}
          }else{
            #> for numeric variable
            frm = as.formula(paste("log(", input$var_chem, ")~1", sep=""))

            location_frm = as.formula(paste( "~X+Y", sep=""))
            gs_krig  = {gstat(id=input$var_chem, formula=frm, locations = location_frm ,
                  data=selected_data(), model=vgt(), 
                  nmax = input$nmax %>% as.numeric() ,    # maximum number of datapoints => increase if variogram has oscillations
                  nmin = input$nmin %>% as.numeric(),
                  omax = input$omax %>% as.numeric(),    # maximum nr of datapoints per octant/quadrants 
                  maxdist = input$maxdist %>% as.numeric(), # maximum distance to seek for datapoints
                  force = T
                  
                  )
              
              }
          }

    # ws<<-structure(gs_krig())
    waiter <- waiter::Waiter$new()
    waiter$show()
    
    kriged = predict(gs_krig, newdata=xy_grid, debug.level=-1)
    on.exit(waiter$hide())
    
    kriging.res <<- data.frame(kriged)

    
    
    #> Kriging visualization--------
    output$krig_res = plotly::renderPlotly({
     
      if (input$indicator_box) {
        ggplot(kriged, aes_string(x="X",y="Y"))+
          geom_raster(aes_string(fill=paste0(input$indicator_list,".pred")))+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
          coord_cartesian(
            ylim=c(min(y),max(y)),
            xlim=c(min(x),max(x))
            
          )+
          ggtitle(paste0("Results of kriging for ", input$indicator_list ))+
          scico::scale_fill_scico(palette = "roma",direction = -1)
        
        
        
      }else{
        ggplot(kriged, aes_string(x="X",y="Y"))+
          geom_raster(aes_string(fill=paste0(input$var_chem,".pred")))+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
          coord_cartesian(
            ylim=c(min(y),max(y)),
            xlim=c(min(x),max(x))
            
          )+
          ggtitle(paste0("Results of kriging for Log(", input$var_chem,")" ))+
          scico::scale_fill_scico(palette = "roma",direction = -1)
        
      }
      
      
      
    })


   


  })
  
  
  
  
  ### Simulations -------
  
  observeEvent(input$sim_btn,{
    
    
    rangesXY = {selected_data() %>% select(input$Xcoords,input$Ycoords) %>% sapply(range)}
    
    
    x = {seq(from=rangesXY[1,input$Xcoords]-20, to=rangesXY[2,input$Xcoords]+20, by=10)}
    y = {seq(from=rangesXY[1,input$Ycoords]-20, to=rangesXY[2,input$Ycoords]+20, by=10)}
    
    xy_grid = {expand.grid(X=x, Y=y)}
    if(input$indicator_box){
      
      frm = as.formula(paste( as.character(input$var_chem), "=='", as.character(input$indicator_list), "'~1", sep=""))
      location_frm = as.formula(paste("~X+Y", sep=""))
      
      gs_krig  =  {gstat(id=input$indicator_list, formula=frm, locations = location_frm ,
                         data=selected_data(), model=vgt() , 
                         nmax = input$nmax %>% as.numeric() ,    # maximum number of datapoints => increase if variogram has oscillations
                         nmin = input$nmin %>% as.numeric(),
                         omax = input$omax %>% as.numeric(),    # maximum nr of datapoints per octant/quadrants 
                         maxdist = input$maxdist %>% as.numeric(), # maximum distance to seek for datapoints
                         force = T
                         )}
    }else{
      #> for numeric variable
      frm = as.formula(paste("log(", input$var_chem, ")~1", sep=""))
      
      location_frm = as.formula(paste( "~X+Y", sep=""))
      gs_krig  = {gstat(id=input$var_chem, formula=frm, locations = location_frm ,
                        data=selected_data(), model=vgt(), 
                        nmax = input$nmax %>% as.numeric() ,    # maximum number of datapoints => increase if variogram has oscillations
                        nmin = input$nmin %>% as.numeric(),
                        omax = input$omax %>% as.numeric(),    # maximum nr of datapoints per octant/quadrants 
                        maxdist = input$maxdist %>% as.numeric(), # maximum distance to seek for datapoints
                        force = T
                        
                        ) }
    }
    
    
    waiter <- waiter::Waiter$new()
    waiter$show()
    
    sims= predict(gs_krig, newdata=xy_grid, debug.level=-1, nsim=input$nsim)
    
    on.exit(waiter$hide())
    
    sims.res <<- data.frame(sims)
    
    
    #> Simulation visualization-------
    output$sim_res = plotly::renderPlotly({
      
      if (input$indicator_box) {
        ggplot(sims, aes_string(x="X",y="Y"))+
          geom_raster(aes_string(fill=paste0(paste0("sim",input$dispnsim))))+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
          coord_cartesian(
            ylim=c(min(y),max(y)),
            xlim=c(min(x),max(x))
            
          )+
          ggtitle(paste0("Visualization of simulation № ",input$dispnsim, " for ", input$indicator_list ))+
          scico::scale_fill_scico(palette = "roma",direction = -1)
        
        
        
      }else{
        ggplot(sims, aes_string(x="X",y="Y"))+
          geom_raster(aes_string(fill=paste0(paste0("sim",input$dispnsim))))+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
          coord_cartesian(
            ylim=c(min(y),max(y)),
            xlim=c(min(x),max(x))
            
          )+
          ggtitle(paste0("Visualization of simulation № ",input$dispnsim, " for Log(", input$var_chem,")" ))+
          scico::scale_fill_scico(palette = "roma",direction = -1)
        
      }
      
      
      
    })
    
    
    
    
    
    
    
    
  })
  
  
  
  
  

  ### render data frame ------------
  output$datatabshow <- DT::renderDT({    
    selected_data()
    
    
  })
  
  
  

  ### plotting ---------
  #> plot empirical variogram and  model---------
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
  
  
  #> Swath plots-------------
  #> Plot spatial dependency of the variable w.r.t direction North/East
  
  output$swathN = plotly::renderPlotly(
    {
      if (!input$indicator_box) {
        vrbl = paste0("log(",input$var_chem,")")
      ggplot(selected_data()) +
        geom_point(aes_string(x="Y", y=vrbl )) +
        theme_bw() +
        ggtitle(paste0(" Log(",input$var_chem,")", " vs Y"))+
        geom_smooth(aes_string(x="Y", y=vrbl ), method=loess, col=2)
      }
      
      
    }
    
    
    
  )
  #> Swath easting--------
  output$swathE = plotly::renderPlotly(
    {
      if (!input$indicator_box) {
      vrbl = paste0("log(",input$var_chem,")")
      ggplot(selected_data()) +
        geom_point(aes_string(x="X", y=vrbl )) +
        theme_bw() +
        ggtitle(paste0(" Log(",input$var_chem,")", " vs X") )+
        geom_smooth(aes_string(x="X", y=vrbl ), method=loess, col=2)}
    }
  )
  
  
  
  
  
  
  
  
 

  
  
  
  
  
  
  
} 



 shinyApp(ui = ui, server = server)


