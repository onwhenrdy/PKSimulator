####################### server ############################################


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class = NULL
  }
  
  return(mi)
}

button.style <- paste0("float:center; color: #fff; background-color: ",
                       Settings.color.dark, "; border-color: ",
                       Settings.color.light)


render.menu <- function(input) {
  renderMenu({
    sidebarMenu(
      
      menuItem("About", tabName = "about", icon = icon("info-circle"), selected = TRUE),
      menuItem("Changelog", tabName = "changelog", icon = icon("paperclip")),
      
      # Simulator stuff
      convertMenuItem(menuItem("Simulator",

       # Buttons
       splitLayout(cellWidths = c("40%", "60%"),
                   
                   actionButton("resetButton", "Reset", icon("trash"),
                                style = button.style),
                   
                   actionButton("addButton", "Add to Plot", icon("chart-line"),
                                style = button.style)
       ),                       
      
       sliderInput("SimTime", label = "Simulation [h]", min = 5, 
                   max = 240, value = 48),
       
       menuItem("Model", 
                icon = icon("project-diagram"),
                startExpanded = TRUE,                         
         
         radioGroupButtons("Model", "Model",
                           choiceNames = list("1-Comp", "2-Comp"),
                           choiceValues = list("1C", "2C"),
                           selected = "1C",
         ),
         
         prettySwitch("PD", "Pharmacodynamics"),
         
         radioGroupButtons("Admin", "Administration",
                          choiceNames = list("Bolus", "PO", "Inf", "Mix"),
                          choiceValues = list("IV", "PO", "Inf", "Mix"),
                          selected = "IV",
         )
       ),
        
      # Dosing
      menuItem("Dosing", 
               icon = icon("coffee"),
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
      menuItemOutput("pk_abs_menu"),
      
      menuItem("PK (Central)", 
               icon = icon("heart"),
               startExpanded = FALSE,
               
               radioGroupButtons("EliminationModel", "Elimination",
                                 choiceNames = list("FO", "MM"),
                                 choiceValues = list("FO", "MM"),
                                 selected = "FO",
               ),
               
               numericInput("VD", label = HTML("V<sub>Central</sub> [l]"), min = 1, step = 0.1, 
                            max = 2000, value = 10),
               
               # FO
               numericInput("KEL", label = HTML("k<sub>el</sub> [1/h]"), min = 0.00001, max = 10, value = 0.1),
               
               # MM
               splitLayout(
                numericInput("VMAX", label = HTML("V<sub>max</sub> [1/h]"), min = 0.00001, max = 10, value = 1),
                numericInput("KM", label = HTML("K<sub>m</sub> [mg/l]"), min = 0.00001, max = 10, value = 1)
               )
      ),
      menuItemOutput("pk_peri_menu"),
      menuItemOutput("pd_menu"),
      
      # Display
      menuItem("Display Options", 
               icon = icon("chart-bar"),
               startExpanded = FALSE,
        
        checkboxGroupInput("ShowComp", label = "Show Components", 
                           choices = list("Central (PK)" = "central", 
                                          "Peripheral (PK)" = "peripheral",
                                          "Effect (PD)" = "effect",
                                          "Tables" = "tables"),
                           selected = c("central", "peripheral", "tables")),
        
        checkboxGroupInput("PlotOpt", label = "Plot Options", 
                           choices = list("Show Plot Panels" = "panel"))
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

show_element <- function(element, show) {
  if (show)
    showElement(element)
  else
    hideElement(element)
}


validate_ui <- function(input, session) {
  print("Validate")
  if (length(input$Admin) <= 0) {
    return()
  }
  
  # El-Kinetics
  fo_kin <- "FO" %in% input$EliminationModel
  show_element("KEL", fo_kin)
  show_element("KM", !fo_kin)
  show_element("VMAX", !fo_kin)
  
  ###### Dosing
  inf <- input$Admin == "Inf"
  mix <- input$Admin == "Mix"
  
  # inf
  show_element("Rate", inf)
  show_element("Duration", inf)
  show_element("Dose", !inf && !mix)
  
  # mix
  show_element("MixDosing", mix)
  
  # Multi and Init
  show_multi <- !mix
  show_element("UseMultiDose", show_multi)
  
  multi_on <- show_multi && input$UseMultiDose
  show_element("DoseInterval", multi_on)
  show_element("MultiDoses", multi_on)
  show_element("UseInitDose", multi_on)
  
  init_on <- show_multi && input$UseMultiDose && input$UseInitDose
  show_element("InitDose", init_on)
  
  ###### PD
  pd_enabled <- input$PD
  if (pd_enabled) {
    
    emax <- ("emax" %in% input$PDModel)
    show_element("IDRMODEL", !emax)
    show_element("C50", emax)
    show_element("HILL", emax)
    show_element("EMAX", emax)
    show_element("EBASE", !emax)
    show_element("EKIN", !emax)
    show_element("EKOUT", !emax)
    show_element("ISMAX", !emax)
    show_element("ISC50", !emax)
    
    if ("idr" %in% input$PDModel) {
      inhibition <- (input$IDRMODEL == "1" || input$IDRMODEL == "2")
      max_tag <- if (inhibition) "I<sub>max</sub> [1]" else "S<sub>max</sub> [1]"
      c50_tag <- if (inhibition) "IC<sub>50</sub> [mg/l]" else "SC<sub>50</sub> [mg/l]"
      updateNumericInput(session, inputId = "ISMAX", label = max_tag)
      updateNumericInput(session, inputId = "ISC50", label = HTML(c50_tag))
    }
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


plot_box <- function(title, 
                     output, 
                     width,
                     layout_on = FALSE) {
  
  if (is.null(layout_on))
    layout_on = FALSE

  box(width = width,
      title = title, 
      status = "primary", 
      solidHeader = TRUE, 
      collapsible = TRUE,
      plotlyOutput(output),
      prettySwitch(paste0(output, "Log"), "Log", inline = TRUE, slim = TRUE),
      prettySwitch(paste0(output, "Layout"), "Wide Layout", inline = TRUE, slim = TRUE, value = layout_on)
      )
}


render_sim_layout <- function(input) {
  print("Layout render")
  
  renderUI({
    render_cent <- "central" %in% input$ShowComp
    render_per <- "peripheral" %in% input$ShowComp
    render_effect <- "effect" %in% input$ShowComp
    render_table <- "tables" %in% input$ShowComp
    
    if (render_cent) {
      cent_wide <- input$centralLayout
      plot_cent <- plot_box("Central", "central", 6 + 6 * cent_wide, 
                            layout_on = cent_wide)
    } else  {
      cent_wide <- FALSE
      plot_cent <- NULL
    }
    
    if (render_per) {
      peripheral_wide <- input$peripheralLayout
      plot_per <- plot_box("Peripheral", "peripheral", 6 + 6 * peripheral_wide,
                           layout_on = peripheral_wide)
    } else  {
      peripheral_wide <- FALSE
      plot_per <- NULL
    }
    
    if (render_effect) {
      effect_wide <- input$effectLayout
      plot_effect <- plot_box("Effect", "effect", 6 + 6 * effect_wide,
                              layout_on = effect_wide)
    } else  {
      effect_wide <- FALSE
      plot_effect <- NULL
    }
    
    tagList(
      # 1
      fluidRow(plot_cent, plot_per, plot_effect),
      
      # Table
      fluidRow(
        if (render_table) tabBox(title = "Tables", width = 12, 
                                tabPanel("Parameter", tableOutput("parameter")),
                                tabPanel("PK", tableOutput("pk"))) else NULL)
    )
  })
} 


plot_sims <- function(data, output, input) {
  
  show_panel <- "panel" %in% input$PlotOpt
  if (is.null(show_panel))
    show_panel <- FALSE
  
  if ("peripheral" %in% input$ShowComp) {
    show_log <- input$peripheralLog
    if (is.null(show_log))
      show_log <- FALSE
    
    plot_peri <- plot_simulations(data, "per", ylab = "Concentration [mg/l]",
                                  show_panel = show_panel,
                                  ylog = show_log)
    output$peripheral <- renderPlotly({plot_peri})
  }
  
  if ("central" %in% input$ShowComp) {
    show_log <- input$centralLog
    if (is.null(show_log))
      show_log <- FALSE
    
    plot_cent <- plot_simulations(data, "cent", ylab = "Concentration [mg/l]",
                                  show_panel = show_panel,
                                  ylog = show_log)
    
    output$central <- renderPlotly({plot_cent})
  }
  
  if ("effect" %in% input$ShowComp) {
    show_log <- input$effectLog
    if (is.null(show_log))
      show_log <- FALSE
    
    plot_effect <- plot_simulations(data, "effect", ylab = "Effect",
                                    show_panel = show_panel,
                                    ylog = show_log)
    
    output$effect <- renderPlotly({plot_effect})
  }
}

render_peripheral <- function(input, output) {
  per_menu <- menuItem("PK (Peripheral)",
                       icon = icon("exchange-alt"),
                       startExpanded = FALSE,
                 
                       numericInput("VD2", label = HTML("V<sub>Peripheral</sub> [l]"), min = 1, step = 0.1, 
                                    max = 2000, value = 10),
                 
                       numericInput("Q", label = "Q [l/h]", min = 0.00001, max = 1000, value = 0.1))
  
  output$pk_peri_menu <- renderMenu({
    if ("2C" %in% input$Model) per_menu else div()
  })
}

render_pd <- function(input, output) {
  pd_menu <- menuItem("Pharmacodynamics", 
                      icon = icon("creative-commons-pd-alt"),
                      startExpanded = FALSE,
                     
                      radioGroupButtons("PDModel", "PD Model",
                                        choiceNames = list("Emax", "IDR"),
                                        choiceValues = list("emax", "idr"),
                                        selected = "emax",
                      ),
                      
                      selectInput("IDRMODEL", label = "IDR Model", 
                                  choices = list("Inhibition (kin)" = 1, 
                                                 "Inhibition (kout)" = 2, 
                                                 "Stimulation (kin)" = 3,
                                                 "Stimulation (kout)" = 4), 
                                  selected = 1),
                      
                      splitLayout(
                        numericInput("EMAX", label = HTML("E<sub>max</sub> [1]"), 
                                     min = 0.00001, max = 1000, value = 0.1),
                      
                        numericInput("C50", label = HTML("EC<sub>50</sub> [mg/l]"), min = 1, step = 0.1, 
                                   max = 2000, value = 10)),
                      
                      numericInput("HILL", label = HTML("Hill exponent [1]"), min = 1, step = 0.1, 
                                   max = 2000, value = 1),
                     
  
                      numericInput("EBASE", label = HTML("E<sub>Base</sub> [1]"), 
                                   value = 30),
          
                      splitLayout(
                        numericInput("EKIN", label = HTML("k<sub>in</sub> [units/h]"), 
                                     value = 0.9),
            
                        numericInput("EKOUT", label = HTML("k<sub>out</sub> [1/h]"), 
                                     value = 0.1)),
                      
                      splitLayout(
                        numericInput("ISMAX", label = HTML("I/S<sub>max</sub> [1]"), 
                                     value = 0.2),
                        
                        numericInput("ISC50", label = HTML("I/S C<sub>50</sub> [mg/l]"), 
                                     value = 1))
  )
                      
  output$pd_menu <- renderMenu({
    if (input$PD) pd_menu else div()
  })
}

render_abs <- function(input, output) {
  abs_menu <- menuItem("PK (Absorption)",
                       id = "pk_abs_menu",
                       icon = icon("pills"),
                       startExpanded = FALSE,
                       
                       numericInput("KIN", label = HTML("k<sub>in</sub> [1/h]"), min = 0.00001, max = 10, value = 0.2),
                       
                       sliderInput("BioF", label = "Bioavailability [%]", min = 1, 
                                   max = 100, value = 100))
  
  output$pk_abs_menu <- renderMenu({
    if ("PO" %in% input$Admin || "Mix" %in% input$Admin) abs_menu else div()
  })
}


# server is called ONCE PER SESSION !!
# EVERYTHING INSIDE IS instanciated per session !!
server <- function(input, output, session) {
  # after loading the page
  waiter_hide()
  # render menu
  output$menu <- render.menu(input)
  # render simulation layout
  output$simLayout <- render_sim_layout(input)
  
  # observer menu changes
  observe({
    validate_ui(input, session)
  })
  
  observeEvent(input$Model, {
    render_peripheral(input, output)
  })
  
  observeEvent(input$PD, {
    render_pd(input, output)
  })
  
  observeEvent(input$Admin, {
    render_abs(input, output)
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
  # show comps buttons pressed
  observeEvent(length(input$ShowComp) + 
                 input$peripheralLayout +
                 input$centralLayout + 
                 input$effectLayout, {
    
    if (length(plots$ids) > 0) {
      render_sim_layout(input)
      plot_sims(plots, output, input)      
    }
  }, ignoreNULL = FALSE)
  
  
  ##########################################################################
  # display
  observeEvent(input$peripheralLog + 
               input$centralLog + 
               input$effectLog + 
               length(input$PlotOpt), {
    
    if (length(plots$ids) > 0) {
      plot_sims(plots, output, input)      
    }
  }, ignoreNULL = FALSE)
  
  ##########################################################################
  # add  button pressed
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
    
    # Error check
    error <- check_input(input) 
    if (is.null(error))
    {
      shinyjs::show(id = "body_div")
      
      output$error_text <- NULL
      
      # gather input
      ##########################################################################
      model_input <- gather_input(input)
      
      # create event table
      ##########################################################################
      event_table <- create_event_table(input)
      
      # simulate
      ##########################################################################
      if (input$PD && "idr" %in% input$PDModel) {
        THETAS = c(KA = model_input$KA, 
                   KE = model_input$KE, 
                   BioF = model_input$BioF,
                   V1 = model_input$V1,
                   Q = model_input$Q,
                   V2 = model_input$V2,
                   Km = model_input$Km,
                   Vmax = model_input$Vmax,
                   KIN = input$EKIN,
                   KOUT = input$EKOUT,
                   ISMAX = input$ISMAX,
                   ISC50 = input$ISC50,
                   EMODEL = as.numeric(input$IDRMODEL))
        
        res <- simulate(model_2, THETAS = THETAS, event_table = event_table, 
                        inits = c(ABS = 0, CENT = 0, PER = 0, EFFECT = input$EBASE))
      } else {
        THETAS = c(KA = model_input$KA, 
                   KE = model_input$KE, 
                   BioF = model_input$BioF,
                   V1 = model_input$V1,
                   Q = model_input$Q,
                   V2 = model_input$V2,
                   Km = model_input$Km,
                   Vmax = model_input$Vmax,
                   Emax = model_input$Emax,
                   C50 = model_input$C50,
                   HILL = model_input$Hill)
        
        res <- simulate(model_1, THETAS = THETAS, event_table = event_table)
      }
      # add and postprocess
      ##########################################################################
      id <- paste(length(plots$ids) + 1)
      sim_name <- paste("Simulation", id)
      res <- data.frame(time = res$time,
                        cent = res$CENTC,
                        per = res$PERC,
                        effect = res$EFFECT,
                        id = id,
                        thalf = 2,
                        legend = sim_name)
      
      plots$raw <<- rbind(plots$raw, res)
      plots$ids <<- c(plots$ids, id)

      # calculate and add PK 
      ##########################################################################
      #plots$pk <<- calculatePK(plots$pk, res, "cent", input, sim_name)
      
      # parameters
      ##########################################################################
      #plots$parameter <<- addParameters(plots$parameter, input, sim_name)
      
      # plots
      ##########################################################################
      plot_sims(plots, output, input)      
      
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
