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
    read.csv(input$filedata$datapath)
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
  
  ### kriging -----
  # output$kriging_box <- renderUI({
  #   checkboxInput("kriging_box", "Kriging", value = F)
  # })
  # 
  
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
  
  
  ### variogram cutoff---------
  output$cutoff<-renderUI({
    req(selected_data())
    sliderInput("cutoff","Cutoff", min = 1, 
                max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value = 225)
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
  ### save model parameters
  observeEvent(input$save_model, {
    res = structure(vgt())
    variogram_model<<-res
    
  })
  
  ### kriging----------
  
  observeEvent(input$kriging_btn,{
    rangesXY = reactive({selected_data() %>% select(input$Xcoords,input$Ycoords) %>% sapply(range)})
    
    
    x = reactive({seq(from=rangesXY[1,input$Xcoords]-20, to=rangesXY[2,input$Xcoords]+20, by=10)})
    y = reactive({seq(from=rangesXY[1,input$Ycoords]-20, to=rangesXY[2,input$Ycoords]+20, by=10)})
    
    xy_grid = reactive({expand.grid(X=x(), Y=y())})
    if(input$indicator_box){
      
      frm = as.formula(paste( as.character(input$var_chem), "=='", as.character(input$indicator_list), "'~1", sep=""))
      location_frm = as.formula(paste("~X+Y", sep=""))
      
      gs_krig  =  reactive({gstat(id=input$var_chem, formula=frm, locations = location_frm ,
                                  data=selected_data(), model=vgt )})
    }else{
      #> for numeric variable
      frm = as.formula(paste("log(", input$var_chem, ")~1", sep=""))
      
      location_frm = as.formula(paste( "~X+Y", sep=""))
      gs_krig  = reactive({gstat(id=input$var_chem, formula=frm, locations = location_frm ,
                                 data=selected_data(), model=vgt ) })
    }
    
    ws<<-structure(gs_krig())
    
    
    kriged = reactive(predict(freezeReactiveValue(gs_krig), newdata=freezeReactiveValue(xy_grid), debug.level=-1))
    # kriged = reactive({predict(isolate(gs_krig), newdata=isolate(xy_grid), debug.level=-1)})
    kriging.res <<- data.frame(kriged)
    
    #> Kriging visualization
    # output$krig_res = plotly::renderPlotly({
    #   ggplot(kriged(), aes(x="X",y="Y"))+
    #       geom_raster(aes(fill=paste0(input$varchem,".pred")))+
    #       coord_fixed()+
    #       theme_bw()+
    #       ggtitle(paste0("Results of kriging for Log(", input$varchem,")" ))+
    #       scico::scale_fill_scico(palette = "roma",direction = -1)
    # 
    # 
    # })
    
    
    
    
    
  })
  
  
  
  
  ### render data frame ------------
  output$datatabshow <- DT::renderDT({    
    selected_data()
    
    
  })
  
  
  
  
  ### plotting ---------
  # plot empirical variogram and  model
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
  
  
  #> Swath plots
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