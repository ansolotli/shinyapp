suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyBS) # for popovers
  library(stringi)
  library(ggplot2)
})

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
               content = "Sustainable fishery policy explores if healthy states of all three major fish stocks (i.e. cod, herring and sprat) will support a healthy and functioning food web and will be the basis for developing a sustainable exploitation of the Baltic Sea. <br><br> Pelagics-Focused scenario explores the option that given the climate change and nutrient load scenarios, prioritisation of herring and sprat over cod would be the preferred fisheries management strategy. <br><br> Open access scenario enables exploring the impacts of largely unregulated fisheries management on the status of the fish stocks, the food web and the economic outcomes from fisheries in the future Baltic Sea."
               , placement = "right", trigger = "hover", 
               #increase the size of the popover according to the length of content
               options = list(container = "body")),
               
               popify(selectInput(inputId = 'Nutr_scen', 
                           label = "Nutrient Loading Policy",
                           choices = c("Baltic Sea Action Plan" = "BSAP", "Average 1995-2002" = "Ref"),
                           selected = "BSAP"
               ), "Nutrient loading policies", 
               content = "Nutrient loads were modelled according to the higher reference conditions between 1995 and 2002 and according to the lower nutrient loads outlined in the Baltic Sea Action Plan programme by HELCOM."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'Climate', 
                           label = "Climate Change Scenario - Representative Concentration Pathways",
                           choices = c("RCP4.5", "RCP8.5"), 
                           selected = "RCP4.5"
               ), "Climate scenarios", 
               content = "Climate scenarios follow Representative Concentration Pathways (RCP) 4.5 and 8.5 according to the ICPP 2014."
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
                               title = "Novelty variables", 
                               content = "Cod reproductive volume is the water layer where the water is warm enough but not too saline for the eggs to float in an oxygenated environment. If this volume decreases, the reproduction of cod will be less successful. <br><br> The temperature variables present the water temperature in the surface layer of the water column during spring months and in the summer. <br><br> Inverse hypoxic area is an area that has gone anoxic i.e. is not oxygenated."
                               , placement = "right", trigger = "hover", options = list(container = "body")),
                        
                        popify(checkboxInput(inputId = "novelTotal", 
                                      label = "Total abiotic novelty", value = TRUE),
                               title = "Total abiotic novelty", 
                               content = "Total abiotic novelty is the sum of other novelty variables and describes the total novelty expected to occur in the Central Baltic Sea in the current century."
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
               ), title = "Minimum acceptable profit", 
               content = "Minimum acceptable profit enables selection of the desired lowest limit of profitability of fisheries on cod, herring, and sprat in millions of euros."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'F_GES',
                                       label = "Environmental status of fish stocks",
                                       choices = c("All stocks above the ref. point" = "All above", "One stock below the ref. point" = "One below",
                                                   "Two stocks below the ref. point" = "Two below", "All stocks below the ref. point" = "All below"),
                                       selected = "All above"
               ), title = "Environmental status of fish stocks", 
               content = "The spawning-stock biomass of the three major fish species (i.e. cod, herring and sprat) in relation to the reference point describes the expected status of these fish stocks. The scale ranges from all three species being above the reference point limit to none of the species faring well."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(radioButtons(inputId = 'Ref_point',
                                   label = "Reference point",
                                   choices = c("Blim" = "Blim", "B MSY trigger" = "B MSY"),
                                   selected = "Blim",
                                   inline = TRUE # horizontal display
               ), title = "Reference point", content = "Blim is the reference point identified by ICES describing the limit below which there is a high risk of reduced recruitment. <br><br> B MSY trigger (or BMSYtrigger) is considered as the lower bound of spawning–stock biomass fluctuation below which fishing needs to be reduced to allow a fish stock to rebuild to levels capable of producing maximum sustainable yield (MSY)."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'Nutr_GES',
                                       label = "Status of water quality",
                                       choices = c("Above GES" = "GES", "Below GES" = "Sub-GES"),
                                       selected = "Above"
               ), title = "Status of water quality", 
               content = "The indicators of water quality comprise of the average concentrations of nitrogen, phosphorus and chlorophyll <i>a</i>. Low concentrations suggest high probability of reaching reaching the Good Environmental Status (GES)."
               , placement = "right", trigger = "hover", options = list(container = "body")),
                
               popify(radioButtons(inputId = 'Novelty',
                                  label = "Include novelty",
                                  choices = c("Yes" = "Yes", "No" = "No"),
                                  selected = "No",
                                  inline = TRUE
               ), title = "Novelty", 
               content = "Ecological novelty denotes unprecedented, human-mediated changes at different ecological levels. The rapid changes in climate and in other anthropogenic pressures may result in novel species communities and ecosystems without any historical analogue."
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
              splitLayout(cellWidths = c("50%", "50%"), 
                          uiOutput("bio_plot_list"), 
                          uiOutput("catch_plot_list"))
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
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")) 
)

# Create the UI 
ui <- dashboardPage(header, sidebar, body, skin = "black")
