
# adds parameters to table
addParameters <- function(table, input, sim_name) {
  
  if (input$Admin == "Mix") {
  
    df <- data.frame(Simulation = sim_name,
                     Type = "Mix",
                     "Dose [mg]" = NA,
                     "Duration [h]" = NA,
                     "Initial Dose [mg]" = NA,
                     "Dosis Interval [h]" =  NA,
                     "VD [l]" = input$VD,
                     "F [%]" = input$BioF,
                     "kin [1/h]" = input$KIN,
                     "kel [1/h]" = input$KEL,
                     "t 1/2 [h]" = log(2)/input$KEL,
                     check.names = FALSE)
    
    table <- rbind(table, df)
  }
  else {
    type_name <- if (input$Admin == "IV") "IV" else if (input$Admin == "Inf") "Inf" else "PO"
    
    if (input$Admin != "Inf")
    {  
      if (input$UseMultiDose)
        type_name <- paste(type_name, "(multi)")
      else
        type_name <- paste(type_name, "(single)")
    }
    
    df <- data.frame(Simulation = sim_name,
                     Type = type_name,
                     "Dose [mg]" = if (input$Admin == "Inf") input$Rate * input$Duration else input$Dose,
                     "Duration [h]" = if (input$Admin == "Inf") input$Duration else NA,
                     "Initial Dose [mg]" = if (input$Admin != "Inf" && input$UseMultiDose && input$UseInitDose) input$InitDose else NA,
                     "Dosis Interval [h]" = if (input$Admin != "Inf" && input$UseMultiDose) input$DoseInterval else NA,
                     "VD [l]" = input$VD,
                     "F [%]" = if (input$Admin != "PO") NA else input$BioF,
                     "kin [1/h]" = if (input$Admin != "PO") NA else input$KIN,
                     "kel [1/h]" = input$KEL,
                     "t 1/2 [h]" = log(2)/input$KEL,
                     check.names = FALSE)
    
    table <- rbind(table, df)
  }
  
  return(table)
} 


calculate_auc_linlog <- function(times, values) {
  
  if (length(times) != length(values))
    stop("Error: times and values must have the same length")
  
  if (length(times) < 2)
    return(0.)
  
  auc = 0.
  
  for (i in 2:length(times)) {
    t1 <- times[i - 1]
    t2 <- times[i]
    if (dplyr::near(t1, t2)) {
      next
    }
    
    c1 <- values[i - 1]
    c2 <- values[i]
    if (c1 > c2 && c2 > 0.)
      auc <- auc + (c1 - c2) * (t2 - t1)/(log(c1) - log(c2))
    else
      auc <- auc + 0.5 * (c1 + c2) * (t2 - t1)
  }
  
  return(auc)
}


calculatePK <- function(table, 
                        data, 
                        comp,
                        input, 
                        sim_name) {
  
  type_name <- if (input$Admin == "IV") "IV" else if (input$Admin == "Inf") "Inf" else "PO"
  
  if (input$Admin != "Inf")
  {  
    if (input$UseMultiDose)
      type_name <- paste(type_name, "(multi)")
    else
      type_name <- paste(type_name, "(single)")
  }
  
  if (input$Admin == "Mix")
    type_name <- "Mix"
  
  # get data that is needed
  data <- data[c("time", comp)]
  names(data) <- c("time", "value")
  
  # calculate PK
  cmax <- max(data$value)
  tmax <- data$time[which(data$value == cmax)][1]
  auc_sim <- calculate_auc_linlog(data$time, data$value)
  auc_last <- data$value[length(data$value)]/input$KEL
  auc_ges <- auc_sim + auc_last
  
  if (input$Admin == "Inf" || 
     (input$Admin == "IV" && !input$UseMultiDose)) {
    cmax <- NA
    tmax <- NA
  }
  
  df <- data.frame(Simulation = sim_name,
                   Type = type_name,
                   "Cmax [mg/l]" =  cmax,
                   "tmax [h]" = tmax,
                   "AUC(0-t) [mg*h/l]" = auc_sim,
                   "AUC(t-inf) [mg*h/l]" = auc_last,
                   "AUC(0-inf) [mg*h/l]" = auc_ges,
                   "AUC extrapolated [%]" = (auc_last / auc_ges) * 100,
                   check.names = FALSE)
  
  table <- rbind(table, df)
  
  return(table)
}


parseMixDosing <- function(input, ev_t) {
  ev_t <- ev_t$copy()
  
  input <- trimws(input)
  input <- gsub(",", ".", input, fixed = TRUE)
  
  lines <- unlist(stringr::str_split(input, pattern = "\\n"))
  for (l in lines) {
    parts <- unlist(strsplit(l, "\\s+"))
    parts <- trimws(parts)
    
    if (length(parts) < 3)
      return("Could not parse dosing (1)")
    
    type <- toupper(parts[1])
    if (type == "IV" || type == "PO") {
      
      if (length(parts) < 5)
        return("Could not parse dosing (2)")
      
      dose <- as.numeric(parts[2])
      start <- as.numeric(parts[3])
      interval <- suppressWarnings(as.numeric(parts[4]))
      nr <- suppressWarnings(as.numeric(parts[5]))
      
      if (is.na(interval)) {
        interval = 24
        nr <- 1
      }
      
      if (is.na(nr))
        nr <- 1
      
      dosing.to <- 1
      if (type == "IV")
        dosing.to <- 2
      
      print(dose)
      print(dosing.to)
      print(start)
      print(interval)
      print(nr)
      
      ev_t <- RxODE::add.dosing(ev_t, 
                                dose = dose,
                                dosing.to = dosing.to,
                                start.time = start,
                                dosing.interval = interval,
                                nbr.doses = nr,
                                do.sampling = TRUE)
      
    } else if (type == "INF") {
      
      if (length(parts) < 4)
        return("Could not parse dosing (3)")
      
      # rate in mg/h
      # Duration in h
      Rate <- as.numeric(parts[2])
      Duration <- as.numeric(parts[3])
      start <- as.numeric(parts[4])
      Dose <- Rate * Duration 
      
      ev_t <- RxODE::add.dosing(ev_t, start.time = start, 
                                dosing.to = 2, 
                                dose = Dose,
                                dur = Duration,
                                do.sampling = TRUE)
      
    } else {
      
      return("Unknown dosing type")
    }
  }
  
  return(ev_t)
}


# horizontal line
h_line <- function(y = 0, color  = "red", 
                  dash = 'dash') {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, dash = dash)
  )
}

# vertical line
v_line <- function(x = 0, color = "red", 
                  dash = 'dash', ...) {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash = dash, ...)
  )
}

# data in ng/ml
plot_simulations <- function(data, 
                             comp, 
                             ylab,
                             ylog = FALSE,
                             five_thalf = FALSE) {
  
  if (ylog == TRUE) {
    invalids <- which(data$raw[[comp]] <= 0)
    if (length(invalids) > 0)
      data$raw <- data$raw[-invalids,]
  }

  # color mapping
  all_colors <- c("darkgray", "blue", "red", 
                "cyan", "coral", "forestgreen")
  
  
  colors <- all_colors[1:length(data$ids)]
  names(colors) <- c(1:length(colors))
  
  plot <- plot_ly(data = data$raw, 
                  x = ~time, 
                  y = as.formula(paste0("~",comp)),
                  split = ~id,
                  color = ~id, 
                  colors = colors,
                  name = ~legend,
                  type = 'scatter', 
                  mode = 'lines', 
                  showlegend = TRUE)
  
  plot <- plot %>% layout(legend = list(orientation = "h", 
                                        xanchor = "center", x = 0.5, y = -0.2),
                          hovermode = 'x unified', 
                          xaxis = list(title = "Time [h]", 
                                       showspikes = TRUE,
                                       spikemode  = 'across',
                                       spikesnap = 'cursor',
                                       showline = TRUE,
                                       spikecolor = '#000000'),
                          yaxis = list(title = ylab,
                                       type = if (ylog) "log" else "linear")) 
  
  # lines
  if (five_thalf) {
    shapes <- list()
    for (id in data$ids) {
      v <- head(data$raw$thalf[data$raw$id == id], 1) * 5
      shapes <- c(shapes, list(v_line(x = v, color = colors[[id]], dash = 'dash')))
    }
    
    plot <- plot %>% layout(shapes = shapes)
  }
  
  
  # final layout and config
  marg <- list(
    l = 45,
    r = 45,
    b = 50,
    t = 10,
    pad = 4
  )
  
  plot <- plot  %>% config(displayModeBar = FALSE) %>%
    layout(margin = marg) 
  
  return(plot)
}






























