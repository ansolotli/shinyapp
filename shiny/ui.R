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
               # wrap rendered elements and add popovers to them
               popify(selectInput(inputId = 'F',
                           label = "Fishery Policy Scenario", 
                           choices = c("Sustainable" = "Sus", "Pelagics-Focused" = "Pel", "Open Access" = "OA"), 
                           selected = "Sus"
               ), "Fishery policies", 
               "Sustainable fishery policy assumes that healthy states of all three major fish stocks (i.e. cod, herring and sprat) supports a healthy and functioning food web and will be the basis for developing a sustainable exploitation of the Baltic Sea. <br><br> Pelagics-focused scenario assumes that given expected climate change, future living conditions of the cod stock will be detrimental and hence prioritisation of the pelagic fishery on herring and sprat would be the preferred fisheries management strategy. <br><br> Open access scenario assumes that a largely unregulated fishery would be the management of choice for the future Baltic Sea."
               , placement = "right", trigger = "hover", 
               #increase the size of the popover according to the length of content
               options = list(container = "body")),
               
               popify(selectInput(inputId = 'Nutr_scen', 
                           label = "Nutrient Loading Policy",
                           choices = c("Baltic Sea Action Plan" = "BSAP", "Average 1995-2002" = "Ref"),
                           selected = "BSAP"
               ), "Nutrient loading policies", "Nutrient loads were modelled according to the higher reference conditions between 1995 and 2002 and according to the lower nutrient loads outlined in the Baltic Sea Action Plan."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'Climate', 
                           label = "Climate Change Scenario - Representative Concentration Pathways",
                           choices = c("RCP4.5", "RCP8.5"), 
                           selected = "RCP4.5"
               ), "Climate scenarios", "Climate scenarios follow Representative Concentration Pathways (RCP) 4.5 and 8.5 according to the ICPP 2014."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
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
                                           label = "Catch Variables",
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
                        popify(checkboxGroupInput(inputId = "novelVars", 
                                           label = "Novelty variables",
                                           choiceNames = list("Cod reproductive volume", "0-50m temperature March-May", 
                                                              "0-60m temperature August", "Inverse hypoxic area"),
                                           choiceValues = list("plotRv", "plotTemp1", "plotTemp2", "plotHyp"),
                                           selected = "plotRv"), 
                               title = "Novelty variables", content = "Cod reproductive volume is the water layer where the water is warm enough but not too saline for the eggs to float in an oxygenated environment. If this volume decreases, the reproduction of cod will be less successful. <br><br> The temperature variables present the water temperature in the surface layer of the water column during spring months and in the summer. <br><br> Inverse hypoxic area is an area that has gone anoxic i.e. is not oxygenated."
                               , placement = "right", trigger = "hover", options = list(container = "body")),
                        
                        popify(checkboxInput(inputId = "novelTotal", 
                                      label = "Total abiotic novelty", value = TRUE),
                               title = "Total abiotic novelty", content = "Total abiotic novelty is the sum of other novelty variables and describes the total novelty expected to occur in the Central Baltic Sea in the current century."
                               , placement = "right", trigger = "hover", options = list(container = "body"))
                 )
               )
      ), tabName = "novelty"),
    
    convertMenuItem(
      menuItem("Optimize", tabName = "optimize",
               popify(selectInput(inputId = 'Profit',
                                       label = "Minimum acceptable profit",
                                       choices = c("No profit", "Profit larger than 0", "Profit larger than 100", "Profit larger than 200"),
                                       selected = "Profit larger than 100"
               ), title = "Minimum acceptable profit", content = "Minimum acceptable profit describes the profitability of fisheries on the scale of no profit to profit exceeding the value of 200."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'F_GES',
                                       label = "Environmental status of fish stocks",
                                       choices = c("All stocks above the ref. point" = "All above", "One stock below the ref. point" = "One below",
                                                   "Two stocks below the ref. point" = "Two below", "All stocks below the ref. point" = "All below"),
                                       selected = "All above"
               ), title = "Fish SSB to GES", content = "The spawning-stock biomass of the three major fish species (i.e. cod, herring and sprat) in relation to Good Ecosystem Status (GES) describes the expected condition of the fish stocks. The scale ranges from all three species being above the GES limit to none of the species faring well."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'Nutr_GES',
                                       label = "Environmental status of nutrients",
                                       choices = c("Above GES" = "GES", "Below GES" = "Sub-GES"),
                                       selected = "Above"
               ), title = "Nutrients to GES", content = "Nutrients in relation to GES can be over or under the limit representing Good Ecosystem Status. Nutrient concentration affects e.g. eutrophication levels and water clarity."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(radioButtons(inputId = 'Ref_point',
                                   label = "Reference point",
                                   choices = c("Blim" = "Blim", "B MSY trigger" = "B MSY"),
                                   selected = "Blim"
               ), title = "Reference point", content = "Some explanation here about the reference point options."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(radioButtons(inputId = 'Novelty',
                                  label = "Include novelty",
                                  choices = c("Yes" = "Yes", "No" = "No"),
                                  selected = "No"
               ), title = "Novelty", content = "Some explanation here on novelty."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
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
                tabPanel("The model",
                         uiOutput("modelText"),
                         fluidRow(
                           box(solidHeader = TRUE, imageOutput("model", height = "auto"))
                         ),
                         uiOutput("modelText2")
                         ),
                tabPanel("Map of the modelled area",
                         fluidRow(
                           box(solidHeader = TRUE, imageOutput("map", height = "auto"))
                         ),
                         uiOutput("mapText")
                  ))
            )  
    ),
    tabItem("predicted",
            titlePanel("Explore the predicted biomasses and catches in different management scenarios"),
            fluidRow(
              uiOutput("timeseries_info")
            ),
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
            titlePanel("Explore the forecasts about the ecosystem services"),
            fluidRow(
              column(width = 12,
                     uiOutput("opt_info"),
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
                              
                              * { font-family: Arial; }
                              
                              /* app title */
                              .main-header .logo {
                                font-family: Arial;
                              }
                              
                              /* page title */
                              h2 {
                                font-family: Arial;
                                font-size: 24px;
                              }
                              
                              #aboutText {
                                font-size: 15px;
                              }
                              #modelText {
                                font-size: 15px;
                              }
                              #modelText2 {
                                font-size: 15px;
                              }
                              #mapText {
                                font-size: 15px;
                              }
                              #timeseries_info {
                                font-size: 15px;
                              }
                              #timeseries_info .box {
                                background: #d9d9d9;
                                padding: 10px;
                              }
                              #novel_info {
                                font-size: 15px;
                              }
                              #novel_info .box {
                                background: #d9d9d9;
                                padding: 10px;
                              }
                              #opt_info {
                                font-size: 15px;
                              }
                              #opt_info .box {
                                background: #d9d9d9;
                                padding: 10px;
                              }
                              
                              .selectize-control {
                                font-size: 11px;
                              }
                              
                              /* tabBox */                    
                              .nav-tabs-custom>.nav-tabs {
                                font-size: 16px;
                                background-color: #222d32;
                              }
                              .nav-tabs-custom .nav-tabs li:not(.active) a {
                                color: #ffffff;
                              }
                              .nav-tabs-custom .nav-tabs li.active {
                                border-top-color: #000000;
                              }
                              
                              /* gradientBox */
                              .box-header {
                                background-color: #004d99;
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
                                background-color: #cce6ff;
                              }
                              
                              /* popovers */
                              .popover-title {
                                color: #ffffff;
                                font-family: Arial;
                                background-color: #222d32;
                              }
                              .popover-content { 
                                font-family: Arial;
                                color: #ffffff;
                                background: #2c3b41;
                                data-html: true;
                              }
                              '
                              ))) 
)


# Create the UI 
ui <- dashboardPage(header, sidebar, body, skin = "black")
