####################### Helper ############################################


####################### settings ############################################

Settings.app.name <- "PK Simulator"

Settings.model.version <- "2020-09-07"
Settings.app.version <- "0.1"

Settings.UI.showHeader = TRUE


Settings.color.light <- "#9dc3de"    #"#B17ACF"
Settings.color.dark <-  "#7395ae"    #"#852FB4"
Settings.color.hl1 <- "red"


# Spinner
spinner_start <- tagList(
  spin_wave(),
  span("Loading Estimator...", 
       style="color:white;")
)

spinner_calc <- tagList(
  spin_wave(),
  span("Calculating...", 
       style="color:white;")
)

################################################################################
################################################################################
####################### precomputes ############################################
