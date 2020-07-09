####################### server ############################################


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


button.style <- paste0("float:center; color: #fff; background-color: ",
                       Settings.color.dark, "; border-color: ",
                       Settings.color.light)


render.menu <- function(input) {
  
  renderMenu({
    
    sidebarMenu(
      
      menuItem("About", tabName = "about", icon = icon("info-circle"), selected = TRUE),
      menuItem("Changelog", tabName = "changelog", icon = icon("paperclip")),
      
      # Estimator stuff
      convertMenuItem(menuItem("Simulator",
                               
                               
       # Buttons
       splitLayout(cellWidths = c("40%", "60%"),
                   
                   actionButton("resetButton", "Reset", icon("trash"),
                                style = button.style),
                   
                   actionButton("addButton", "Add to Plot", icon("chart-line"),
                                style = button.style)
       ),                       
                               
        # Basics
        radioGroupButtons("Admin", "Administration",
                          choiceNames = list("Bolus", "PO", "Inf", "Mix"),
                          choiceValues = list("IV", "PO", "Inf", "Mix"),
                          selected = "IV",
        ),
  
        sliderInput("SimTime", label = "Simulation [h]", min = 5, 
                    max = 240, value = 48),
        
        # Dosing
        menuItem("Dosing", 
                 icon = icon("pills"),
                 startExpanded = FALSE,
                 
                 
          prettySwitch("UseMultiDose",
                      label = "Multiple Dose", value = FALSE),
          prettySwitch("UseInitDose",
                      label = "Initial Dose", value = FALSE),
          
          numericInput("InitDose", label = "Initial Dose [mg]", min = 1, step = 0.1, 
                       max = 2000, value = 10),
                 
          numericInput("Dose", label = "Dose [mg]", min = 0.1, step = 0.1, 
                      max = 2000, value = 10),
          
          numericInput("DoseInterval", label = "Dosing Interval [h]", min = 0.1, step = 1, 
                       max = 240, value = 24),
          
          sliderInput("MultiDoses", label = "Doses (times)", min = 0, 
                      max = 20, value = 0),
          
          # Inf
          numericInput("Rate", label = "Rate [mg/h]", min = 0., step = 0.1, 
                       max = 2000, value = 10),
          
          # Duration
          numericInput("Duration", label = "Duration [h]", min = 0.1, step = 0.1, 
                       max = 240, value = 10),
          
          # mix
          textAreaInput("MixDosing", "Dosing Regimen", resize = "vertical")
        ),
        
        # PK
        # Display
        menuItem("PK", 
                 icon = icon("user-cog"),
                 startExpanded = FALSE,
                 
          numericInput("VD", label = "Vd [l]", min = 1, step = 0.1, 
                       max = 2000, value = 10),
          
          sliderInput("BioF", label = "Bioavailability [%]", min = 1, 
                      max = 100, value = 100),
          
          numericInput("KIN", label = "kin [1/h]", min = 0.00001, max = 10, value = 0.2),
          numericInput("KEL", label = "kel [1/h]", min = 0.00001, max = 10, value = 0.1)
        ),
        
        # Display
        menuItem("Plotting options", 
                 icon = icon("chart-bar"),
                 startExpanded = FALSE,
                 
          checkboxGroupInput("Display", label = NULL, 
                             choices = list("Logarithmic y axis" = "ylog", 
                                            "Show 5 x t1/2" = "five_thalf"))
        ),
        
      # Menu
      startExpanded = FALSE,
      tabName = "simulator", icon = icon("dashboard")
    ), "simulator")
      
    # sidebarMenu  
    )
      
  # renderMenu
  })
}

enableCheckboxGroupButton = function(inputId, value, enable=TRUE){
  if(enable)
    runjs(glue("$(\"input[name='{inputId}'][value='{value}']\").removeAttr('disabled');"))
  else
    runjs(glue("$(\"input[name='{inputId}'][value='{value}']\").prop('disabled', true);"))
}


validate_ui <- function(input, session) {
  
  if (length(input$Admin) <= 0) {
    return()
  }
  
  if (input$Admin == "Mix") {
    showElement("KIN")
    showElement("BioF")
    hideElement("UseMultiDose")
    hideElement("UseInitDose")
    hideElement("InitDose")
    hideElement("DoseInterval")
    hideElement("MultiDoses")
    hideElement("Dose")
    hideElement("Rate")
    hideElement("Duration")
    showElement("MixDosing")
    
    return()
  } else  {
    hideElement("MixDosing")
  }
  
  
  iv <- input$Admin == "IV" 
  inf <- input$Admin == "Inf"
  multi <- input$UseMultiDose
  inital_dose <- input$UseInitDose
  
  
  if (iv || inf) {
    hideElement("KIN")
    hideElement("BioF")
  } else {
    showElement("KIN")
    showElement("BioF")
  }
  
  print(inf)
  print(iv)
  
  if (inf) {
    hideElement("UseMultiDose")
    hideElement("UseInitDose")
    hideElement("InitDose")
    hideElement("DoseInterval")
    hideElement("MultiDoses")
    hideElement("Dose")
    
    showElement("Rate")
    showElement("Duration")
  
  } else {
    hideElement("Rate")
    hideElement("Duration")
    showElement("UseMultiDose")
    showElement("Dose")
    
    if (multi) {
      showElement("UseInitDose")
      showElement("DoseInterval")
      showElement("MultiDoses")
      
    } else {
      hideElement("UseInitDose")
      hideElement("InitDose")
      hideElement("DoseInterval")
      hideElement("MultiDoses")
    }
  }
  
  
  if (multi && inital_dose && !inf) {
    showElement("InitDose")
  } else {
    hideElement("InitDose")
  }
  
}


check_input <- function(input) {
  
  return(NULL)
  
  if (is.na(input$AGE))
    return("Error: Age must have a value")
  
  if (input$AGE < 0 || input$AGE > 120)
    return("Error: Age must be between 0 and 120")
  
  
  return(NULL)
}

show_boxes <- function(action = c("show", "hide")) {
  
  action <- match.arg(action)
  
  f <- shinyjs::show
  if (action == "hide")
    f <- shinyjs::hide
  
  f(id = "covid_score_box")
}



plot_sims <- function(data, output, ylog, five_thalf) {
  
  plot_abs <- plot_simulations(data, "abs", ylab = "Amount [mg]")
  plot_cent <- plot_simulations(data, "cent", ylab = "Concentration [mg/l]",
                                five_thalf = five_thalf,
                                ylog = ylog)
  
  output$central <- renderPlotly({plot_cent})
  output$abs <- renderPlotly({plot_abs})
}



# server is called ONCE PER SESSION !!
# EVERYTHING INSIDE IS instanciated per session !!
server <- function(input, output, session) {
  # after loading the page
  waiter_hide()
  
  # inject js code from app_ui
  tags$head(tags$script(js_code))
  # render menu
  output$menu <- render.menu(input)
  
  # observer menu changes
  observe({
    validate_ui(input, session)
  })
  
  
  ##########################################################################
  # init
  plots <- list(raw = data.frame(), 
                parameter = data.frame(), 
                pk = data.frame(),
                ids = c())
  
  ##########################################################################
  # reset button pressed
  observeEvent(input$resetButton, {
    confirmSweetAlert(
      session = session,
      inputId = "resetConfirm",
      title = "Do you really want to reset?"
    )
  })
  
  observeEvent(input$resetConfirm, {
    if (isTRUE(input$resetConfirm)) {
      plots <<- list(raw = data.frame(), 
                     parameter = data.frame(), 
                     pk = data.frame(),
                     ids = c())
      shinyjs::hide("body_div")
    }
  })
  
  ##########################################################################
  # display buttons pressed
  observeEvent(input$Display, {
    if (length(plots$ids) > 0) {
      ylog <- "ylog" %in% input$Display
      five_thalf <- "five_thalf" %in% input$Display
      plot_sims(plots, output, ylog, five_thalf)      
    }
  }, ignoreNULL = FALSE)
  
  ##########################################################################
  # calculate button pressed
  observeEvent(input$addButton, {
    
    # check for max plots
    if (length(plots$ids) >= 6) {
      confirmSweetAlert(
        session = session,
        inputId = "resetConfirm",
        title = "Max Plots reached. Do you want to reset?"
      )
      
      return()
    }
    
    waiter_show(html = spinner_calc)
    
    error <- check_input(input) 
    if (is.null(error))
    {
      shinyjs::show(id = "body_div")
      
      output$error_text <- NULL
      
      # input
      ##########################################################################
      BioF <- input$BioF / 100 # from %
      if (input$Admin == "IV")
        BioF <- 1
      
      KA <- input$KIN # in 1/h
      KE <- input$KEL # in 1/h
      Vd <- input$VD # in l
      Dose <- input$Dose # in mg
      DosingInterval <- input$DoseInterval # in h
      DosingTimes <- input$MultiDoses # times
      InitDose <- input$InitDose # mg
      
      sim_end <- input$SimTime
      
      # display
      ylog <- "ylog" %in% input$Display
      five_thalf <- "five_thalf" %in% input$Display
      
      # simulate
      ##########################################################################
      dose.to <- 1
      if (input$Admin == "IV" || input$Admin == "Inf") {
        dose.to <- 2
      }
      
      ev_t <- RxODE::eventTable()
      
      if (input$Admin == "Mix") {
        ev_t <- parseMixDosing(input$MixDosing, ev_t)
      }
      else 
      {
        if (input$Admin == "Inf") {
        
          # rate in mg/h
          # Duration in h
          Duration <- input$Duration
          Dose <- input$Rate*Duration 
          
          ev_t <- RxODE::add.dosing(ev_t, start.time = 0, 
                                    dosing.to = dose.to, 
                                    dose = Dose,
                                    dur = Duration,
                                    do.sampling = TRUE)
          
        } else if (input$UseMultiDose) {
          
          start.time <- 0
          nbr.doses  <-  DosingTimes
          if (input$UseInitDose) {
            ev_t <- RxODE::add.dosing(ev_t, start.time = 0, 
                                      dosing.to = dose.to, 
                                      dose = InitDose,
                                      do.sampling = TRUE)
            
            start.time <- DosingInterval
            nbr.doses <- max(0, nbr.doses - 1)
          }
          
          if (DosingTimes == 0) {
            nbr.doses <- floor((sim_end - start.time)/DosingInterval)
          }
          
          if (nbr.doses > 0) {
            ev_t <- RxODE::add.dosing(ev_t, start.time = start.time, 
                                      dosing.to = dose.to, 
                                      dose = Dose,
                                      nbr.doses = nbr.doses,
                                      dosing.interval = DosingInterval,
                                      do.sampling = TRUE)
            
            if (start.time + nbr.doses * DosingInterval > sim_end)
              sim_end <- start.time + (nbr.doses + 1) * DosingInterval
          }
          
        } else  {
          
         ev_t <- RxODE::add.dosing(ev_t, start.time = 0, 
                                   dosing.to = dose.to, 
                                   dose = Dose,
                                   do.sampling = TRUE)
        }
      }
      
      max_t_ev <- max(ev_t$time)
      if(max_t_ev > sim_end)
        sim_end <- max_t_ev
      
      ev_t <- RxODE::add.sampling(ev_t, seq(0, sim_end, length.out = 500))
      res <- simulate(model_1C, THETAS = c(KA = KA, KE = KE, BioF = BioF), event_table = ev_t)
      
      # add and postprocess
      ##########################################################################
      id <- paste(length(plots$ids) + 1)
      sim_name <- paste("Simulation", id)
      res <- data.frame(time = res$time,
                        abs = res$ABS,
                        cent = res$CENT / Vd, # in mg/l
                        id = id,
                        thalf <- log(2)/KE,
                        legend = sim_name)
      
      plots$raw <<- rbind(plots$raw, res)
      plots$ids <<- c(plots$ids, id)

      # calculate and add PK 
      ##########################################################################
      plots$pk <<- calculatePK(plots$pk, res, "cent", input, sim_name)
      
      # parameters
      ##########################################################################
      plots$parameter <<- addParameters(plots$parameter, input, sim_name)
      
      # plots
      ##########################################################################
      plot_sims(plots, output, ylog, five_thalf)      
      
      # tables
      output$parameter <- renderTable(plots$parameter, striped = TRUE)
      output$pk <- renderTable(plots$pk, striped = TRUE)
      
    } else {
      
      shinyjs::hide("body_div")
      shinyalert("Oops!", error, type = "error")
    }
    
    waiter_hide() # hide the waiter
  })
    
}
