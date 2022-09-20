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


## Server of the APP ------------
VarServer <- function(input, output,session)
{
  ### dynamic data frame--------------
  datas <- reactive({
    req(input$filedata)
    ### check file extension
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
  ##check for indicator
  output$indicator_box <- renderUI({
    checkboxInput("indicator_box", "Indicator", value = F)
  })
  
  ### indicator list-----
  #list of indicators
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
  #> Define mmaximal distance between  observations for local kriging
  #> Is it a cutoff or  width/lag ???
  output$maxdist<-renderUI({
    req(selected_data())
    sliderInput("maxdist","Maximal distance between 2 observations", min = 1,
                max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value =max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/4)
  })
  
  
  ### nmin--------
  #> Define minimal number of observations for local kriging
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
                  #check for numeric/indicator 
                  step = ifelse(!input$indicator_box,
                                var(selected_data()%>%select(input$var_chem))*1.5*0.001,
                                0.001),
                  #check for numeric/indicator
                  max = ifelse(!input$indicator_box,
                               var(selected_data()%>%select(input$var_chem))*3,
                               0.25) ,
                  #check for numeric/indicator
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
  #> creates empirical variogram for the variable of interest indicator/numeric
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
    
    
    
  })
  
  
  
  
  
  
  ### kriging----------
  
  #> Computes kriging on button click
  observeEvent(input$kriging_btn,{
    
    #>set the range 
    rangesXY = {selected_data() %>% select(input$Xcoords,input$Ycoords) %>% sapply(range)}
    
    #> Grid extents
    x = {seq(from=rangesXY[1,input$Xcoords]-20, to=rangesXY[2,input$Xcoords]+20, by=10)}
    y = {seq(from=rangesXY[1,input$Ycoords]-20, to=rangesXY[2,input$Ycoords]+20, by=10)}
    
    #> Define the grid
    xy_grid = {expand.grid(X=x, Y=y)}
    
    #> Check which type of variable is used
    if(input$indicator_box){
      
      frm = as.formula(paste( as.character(input$var_chem), "=='", as.character(input$indicator_list), "'~1", sep=""))
      location_frm = as.formula(paste("~X+Y", sep=""))
      
      gs_krig  =  {gstat(id=input$indicator_list, formula=frm, locations = location_frm ,
                         data=selected_data(), model=vgt() , 
                         
                         nmax = input$nmax %>% as.numeric(),    # maximum number of datapoints => increase if variogram has oscillations
                         omax = input$omax %>% as.numeric(),    # maximum nr of datapoints per octant/quadrants 
                         nmin = input$nmin %>% as.numeric(),    # minimal number of nearest observations
                         maxdist = input$maxdist %>% as.numeric(), # maximum distance to seek for datapoints
                         force = T # in case nmin is given, search beyond maxdist until nmin neighbours are found.
                         
                         
                         
                         
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
                        force = T # in case nmin is given, search beyond maxdist until nmin neighbours are found.
                        
      )
        
      }
    }
    
    #> loading animation
    waiter <- waiter::Waiter$new()
    waiter$show()
    #> kriging
    kriged = predict(gs_krig, newdata=xy_grid, debug.level=-1)
    on.exit(waiter$hide())
    #> save as global var
    kriging.res <<- data.frame(kriged)
    
    
    
    #> Kriging visualization--------
    output$krig_res = plotly::renderPlotly({
      # Check the variable and kriging type
      if (input$indicator_box) {
        # indicator
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
        # numeric
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
  # similar to kriging, except taking parameters from input for nsim 
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
    # save as global var
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