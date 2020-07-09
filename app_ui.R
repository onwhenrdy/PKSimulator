####################### ui ############################################

# JS
js_code <- '$(document).ready(function () {
  $(".sidebar-menu").children("li").on("click", function() {
    $("#main, #npi, #plotting, #advan, #rep").toggle();
  });
});
'

# components
ui_header <- dashboardHeader(title = Settings.app.name,  
                             titleWidth = 260, 
                             disable = !Settings.UI.showHeader)
  
ui_sidebar_menau <- sidebarMenu(id = "menu", sidebarMenuOutput("menu"))
ui_sidebar <- dashboardSidebar(useShinyjs(),
                               width = 260,
                               useShinyjs(),
                               ui_sidebar_menau,
                               tags$style(HTML(".skin-blue .main-sidebar .sidebar .sidebar-menu .active a { 
                                                background-color: #efefef; 
                                                color: #000000;
                                                }",
                                               ".skin-blue .main-sidebar .sidebar .sidebar-menu .active > a {
                                                 color: #7395ae;
                                                 background-color: #ffffff;
                                                 }"
                               ))
                              )

ui_body <- dashboardBody(
  useShinyjs(),
  useShinyalert(),
  shinyDashboardThemes(theme = "onenote"),
  tags$head(tags$script(js_code)),
  use_waiter(),
  waiter_show_on_load(spinner_start),
  tags$style(HTML(".box.box-solid.box-primary>.box-header {
                        color:#FFFFFF;
                        background:#9dc3de
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#9dc3de;
                    border-left-color:#9dc3de;
                    border-right-color:#9dc3de;
                    border-top-color:#9dc3de;}",
                  
                  ".box.box-solid.box-primary>.box-header h3, 
                  .box.box-primary>.box-header h3 {color: rgb(255, 255, 255);}"
                  )
             ),
  
  tags$head(tags$style(HTML(".skin-blue .main-header .logo {
                              background-color: #7395ae;
                              }",
                            ".skin-blue .main-header .logo:hover {
                              background-color: #7395ae;
                              }",
                            ".skin-blue .main-header .navbar {
                              background-color: #7395ae;
                              }",
                            ".skin-blue .main-sidebar {
                              background-color: #7395ae;
                              }",
                            ".skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #7395ae;
                              }",
                            ".skin-blue .main-sidebar .sidebar .sidebar-menu .active > a {
                              color: #7395ae;
                              }",
                            ".skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #7395ae;
                              }",
                            ".skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #7395ae;
                              }",
                            ".skin-blue .main-header .navbar .sidebar-toggle {
                              background: #7395ae;
                              color: #ffffff;
                              }"
  ))),
  
  tabItems(
    tabItem(tabName = "about",
            
      h2("PK Simulator"),
      br(),
      
      h5(paste("Current model version:", Settings.model.version)),
      h5(paste("Current appliction version:", Settings.app.version)),
      hr(),
      includeHTML("about.html")
    ),
    
    tabItem(tabName = "changelog",
            
            h2("Changelog"), br(),
            h5(paste("Current model version:", Settings.model.version)),
            h5(paste("Current appliction version:", Settings.app.version)),
            hr(),
            includeHTML("changelog.html")
    ),
    
    tabItem(tabName = "simulator",
           
      hidden(div(id = "body_div",
        fluidRow(
          box(
            title = "GI Tract", 
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = TRUE,
            column(12, align="center",
                   plotlyOutput("abs")
            )
          ),
          box(
            title = "Central", 
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = TRUE,
            column(12, align="center",
              plotlyOutput("central")
            )
          )
  
      # hidden and div
      ),
      fluidRow(
        tabBox(
          title = "Tables",
          width = 12,
          tabPanel("Parameter", tableOutput("parameter")),
          tabPanel("PK", tableOutput("pk"))
          )
        )
      ))
  )
))

# compile
ui <- dashboardPage(ui_header, ui_sidebar, ui_body)