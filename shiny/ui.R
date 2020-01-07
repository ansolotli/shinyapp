library(shiny)
library(shinydashboard)
library(shinyBS) # for popovers
library(stringi)
library(ggplot2)
library(shinydashboardPlus) # for fancier boxes

# Header
header <- dashboardHeader(title = "BONUS BLUEWEBS decision support tool", titleWidth = 450)


# Manually links menuItems to the corresponding tabItems that would otherwise become unlinked
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}


# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    convertMenuItem(
      menuItem("About", tabName = "about"), tabName = "about"),
    convertMenuItem(
      menuItem("Predicted time series", tabName = "predicted",
               
               # Create the dropdowns of scenario options
               selectInput(inputId = 'F',
                           label = "Fishery Policy Scenario", 
                           choices = c("Sustainable" = "Sus", "Pelagics-Focused" = "Pel", "Open Access" = "OA"), 
                           selected = "Sus"
               ),
               
               selectInput(inputId = 'Nutr_scen', 
                           label = "Nutrient Loading Policy",
                           choices = c("Baltic Sea Action Plan" = "BSAP", "Average 1995-2002" = "Ref"),
                           selected = "BSAP"
               ),
               
               selectInput(inputId = 'Climate', 
                           label = "Climate Change Scenario - Representative Concentration Pathways",
                           choices = c("RCP4.5", "RCP8.5"), 
                           selected = "RCP4.5"
               ),
               
               # Create the two colums of checkbox groups (biomass and catch)
               fluidRow(
                 column(width = 5,
                        checkboxGroupInput(inputId = "bioVars", 
                                           label = "Biomass Variables",
                                           choiceNames = list("Cod", "Herring", "Sprat", "Zooplankton", "Phytoplankton"),
                                           choiceValues = list("plotCod", "plotHer", "plotSpr", "plotZoo", "plotPhy"),
                                           selected = "plotCod")
                 ),
                 
                 column(width = 6, offset = 1,
                        checkboxGroupInput(inputId = "catchVars", 
                                           label = "Catch Size Variables",
                                           choiceNames = list("Cod", "Herring", "Sprat"),
                                           choiceValues = list("plotCodCatch","plotHerCatch","plotSprCatch"), 
                                           selected = "plotCodCatch")
                 )
               )
      ), tabName = "predicted"),
    
    
    convertMenuItem(
      menuItem("Novelty", tabName = "novelty",
               # Create the dropdowns of scenario options
               selectInput(inputId = 'Nutr_scen_nov', 
                           label = "Nutrient Loading Policy",
                           choices = c("Baltic Sea Action Plan" = "BSAP", "Average 1995-2002" = "Ref"),
                           selected = "BSAP"
               ),
               
               selectInput(inputId = 'Climate_nov', 
                           label = "Climate Change Scenario - Representative Concentration Pathways",
                           choices = c("RCP4.5", "RCP8.5"), 
                           selected = "RCP4.5"
               ),
               
               # Create the column of checkbox groups (totalNov, codRV, temp_MarchMay050, temp_Aug060, notHypoxic)
               fluidRow(
                 column(width = 12,
                        checkboxGroupInput(inputId = "novelVars", 
                                           label = "Novelty variables",
                                           choiceNames = list("Cod reproductive volume", "0-50m temperature March-May", 
                                                              "0-60m temperature August", "Inverse hypoxic area"),
                                           choiceValues = list("plotRv", "plotTemp1", "plotTemp2", "plotHyp"),
                                           selected = "plotRv"),
                        
                        checkboxInput(inputId = "novelTotal", 
                                      label = "Total abiotic novelty", value = TRUE)
                 )
               )
      ), tabName = "novelty"),
    
    convertMenuItem(
      menuItem("Optimize", tabName = "optimize",
               selectInput(inputId = 'Profit',
                                       label = "Minimum acceptable profit",
                                       choices = c("No profit", "Profit larger than 0", "Profit larger than 100", "Profit larger than 200"),
                                       selected = "No profit"
               ),
               
               selectInput(inputId = 'F_GES',
                                       label = "Fish SSB in relation to GES",
                                       choices = c("All above" = "All above Blim", "One below" = "One below Blim",
                                                   "Two below" = "Two below Blim", "All below" = "All below Blim"),
                                       selected = "All above"
               ),
               
               selectInput(inputId = 'Nutr_GES',
                                       label = "Nutrients in relation to GES",
                                       choices = c("Above" = "GES", "Below" = "Sub_GES"),
                                       selected = "Above"
               ),
               fluidRow(
                 column(width = 12,
                        checkboxGroupInput(inputId = "optVars", 
                                           label = "Scenario variables",
                                           choiceNames = list("Fishery policy", "Climate scenario", 
                                                              "Nutrient loading policy", "Decade"),
                                           choiceValues = list("opt_fish", "opt_clim", "opt_nutr", "opt_dec"),
                                           selected = "opt_fish")
               )))
      , tabName = "optimize")
  )
)

# Body
body <- dashboardBody(
  
  tabItems(
    tabItem("about",
            titlePanel("About the decision support tool"),
            fluidRow(
              tabBox(width = 12,
                id = "aboutBoxes",
                tabPanel("Introduction",
                         uiOutput("aboutText")
                         ),
                tabPanel("The model"),
                tabPanel("Map of the modelled area",
                  fluidRow(
                    column(width = 6,
                           box(solidHeader = TRUE, imageOutput("DSS", height = "auto"))),
                    column(width = 6,
                           gradientBox(title = "Central Baltic Sea",
                                       width = 12,
                                       collapsible = FALSE,
                                       footer = "The map covers ICES areas XX, XX and XX."
                                   )
                           )
                  )
                  
                )
            ))   
    ),
    tabItem("predicted",
            titlePanel("Explore the predicted biomasses and catches in different management scenarios"),
            fluidRow(
              splitLayout(cellWidths = c("50%", "50%"), uiOutput("bio_plot_list"), uiOutput("catch_plot_list"))
            )
    ),
    tabItem("novelty",
            titlePanel("Explore the uncertainty of model forecasts under novel conditions"),
            fluidRow(
              column(width = 12,
                     uiOutput('novel_info'),
                     uiOutput("novel_plot_list"),
                     uiOutput("novel_plot_total"))
            )
    )
    ,
    tabItem("optimize",
            titlePanel("Explore the scenarios leading to different outcomes"),
            fluidRow(
              column(width = 12,
                       uiOutput("opt_plots")
              )
            )
    )),
    # CSS
    tags$head(tags$style(HTML('
                              /* body */
                              .content-wrapper, .right-side {
                                background-color: #ffffff;
                              }
                              #aboutText {
                                 font-size: 16px;
                              }
                              #novel_info {
                                font-size: 16px;
                              }
                              
                              /* tabBox background */                    
                              .nav-tabs-custom>.nav-tabs {
                                font-size: 16px;
                                font-weight: 600;
                                background-color: #404040;
                              }
                              .nav-tabs-custom .nav-tabs li:not(.active) a {
                                color: #ffffff;
                              }
                              .nav-tabs-custom .nav-tabs li.active {
                                border-top-color: #ffffff;
                              }
                              
                              /* gradientBox */
                              .box-header {
                                background-color: #404040;
                                color: #ffffff;
                              }
                              .box-header .box-title {
                                font-size: 16px;
                              }
                              .box-body {
                                padding: 0px;
                              }
                              .box-footer {
                                font-size: 16px;
                              }
                              
                             
                              /* popovers */
                              .popover-title {
                                color: #ffffff;
                                font-size: 16px;
                                background-color: #3d0099;
                              }
                              .popover-content { 
                                color: #ffffff;
                                font-size: 14px;
                                background: #404040; 
                              }
                              '
                              ))) 
)


# Create the UI 
ui <- dashboardPage(header, sidebar, body, skin = "black")
