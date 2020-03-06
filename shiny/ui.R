suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyBS) # for popovers
  library(stringi)
  library(ggplot2)
  library(rmarkdown)
  library(reshape2)
  library(shinyjs)
  library(patchwork)
  library(dplyr)
  library(scales)
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
      menuItem(HTML("Predicted time series <br> of fish stocks"), tabName = "predicted",
               
               # Create the dropdowns of scenario options
               # wrap rendered elements and add popovers to them
               popify(selectInput(inputId = 'F',
                           label = "Fishery Policy", 
                           choices = c("Sustainable" = "Sus", "Pelagics-Focused" = "Pel", "Open Access" = "OA"), 
                           selected = "Sus"
               ), "Fishery policies", 
               content = "<b>Sustainable fishery policy</b> assumes healthy states of all three major fish stocks (i.e. cod, herring and sprat) as well as a healthy and functioning food web, therefore providing a basis for developing a sustainable exploitation of the Baltic Sea. <br><br> <b>Pelagics-Focused</b> scenario explores the option where, given the climate change and nutrient loading scenarios, the preferred fisheries management strategy would be prioritisation of herring and sprat over cod. <br><br> <b>Open access</b> scenario enables exploring the impacts of largely unregulated fisheries management on the status of the fish stocks, the food web and the economic outcomes from fisheries in the future Baltic Sea."
               , placement = "right", trigger = "hover", 
               #increase the size of the popover according to the length of content
               options = list(container = "body")),
               
               popify(selectInput(inputId = 'Nutr_scen', 
                           label = "Nutrient Loading Policy",
                           choices = c("Baltic Sea Action Plan" = "BSAP", "Reference conditions" = "Ref"),
                           selected = "BSAP"
               ), "Nutrient loading policies", 
               content = "Nutrient loads were modelled both according to the lower nutrient loads outlined in the <b>Baltic Sea Action Plan</b> (BSAP) by HELCOM (Baltic Marine Environment Protection Commission – Helsinki Commission), and the higher reference conditions. <br><br> The <b>reference conditions</b> are based on the HELCOM assessment of the average nutrient loads between 1995 and 2002, and correspond with the current level of nutrient loading in the Baltic Sea."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'Climate', 
                           label = "Climate Change Scenario",
                           choices = c("RCP4.5", "RCP8.5"), 
                           selected = "RCP4.5"
               ), "Climate scenarios", 
               content = "The two climate scenarios follow Representative Concentration Pathways (RCP) <b>4.5</b> and <b>8.5</b>, according to the fifth Assessment Report of the Intergovernmental Panel on Climate Change (IPCC) in 2014. <br><br> These pathways describe the greenhouse gas concentrations in the atmosphere, with higher concentrations resulting in higher warming effects on earth."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               # Create the two colums of checkbox groups (biomass and catch)
               fluidRow(
                 column(width = 5,
                        checkboxGroupInput(inputId = "bioVars", 
                                           label = "Biomass Variables",
                                           choiceNames = list("Cod", "Herring", "Sprat"),
                                           choiceValues = list("plotCod", "plotHer", "plotSpr"),
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
               popify(selectInput(inputId = 'Nutr_scen_nov', 
                                  label = "Nutrient Loading Policy",
                                  choices = c("Baltic Sea Action Plan" = "BSAP", "Reference conditions" = "Ref"),
                                  selected = "BSAP"
               ), "Nutrient loading policies", 
               content = "Nutrient loads were modelled both according to the lower nutrient loads outlined in the <b>Baltic Sea Action Plan</b> (BSAP) by HELCOM (Baltic Marine Environment Protection Commission – Helsinki Commission), and the higher reference conditions. <br><br> The <b>reference conditions</b> are based on the HELCOM assessment of the average nutrient loads between 1995 and 2002, and correspond with the current level of nutrient loading in the Baltic Sea."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               popify(selectInput(inputId = 'Climate_nov', 
                                  label = "Climate Change Scenario",
                                  choices = c("RCP4.5", "RCP8.5"), 
                                  selected = "RCP4.5"
               ), "Climate scenarios", 
               content = "The two climate scenarios follow Representative Concentration Pathways (RCP) <b>4.5</b> and <b>8.5</b>, according to the fifth Assessment Report of the Intergovernmental Panel on Climate Change (IPCC) in 2014. <br><br> These pathways describe the greenhouse gas concentrations in the atmosphere, with higher concentrations resulting in higher warming effects on earth."
               , placement = "right", trigger = "hover", options = list(container = "body")),
               
               # Create the column of checkbox groups (totalNov, codRV, temp_MarchMay050, temp_Aug060, notHypoxic)
               fluidRow(
                 column(width = 12,
                        popify(checkboxGroupInput(inputId = "novelVars", 
                                           label = "Novelty variables",
                                           choices = list("Cod reproductive volume" = "codRV", 
                                                          "Spring temperature" = "T_050_MarchMay", 
                                                          "August temperature" = "Aug060mT",
                                                          "Inverse hypoxic area" = "notHypoxicA"),
                                           #choiceValues = list("plotRv", "plotTemp1", "plotTemp2", "plotHyp"),
                                           selected = "codRV"), 
                               title = "Novelty variables", 
                               content = "Cod spawns in the open sea and the eggs drift in the water. The <b>reproductive volume</b> indicates changes in the size of the water layer where salinity is high enough and thereby also the density of the water (weight of water per its unit volume) is high enough to prevent the eggs from sinking into the oxygen deficient deep water. <br><br> Eutrophication and less saline water inflow to the Baltic Sea via the Danish Straits tend to decrease the reproductive volume of cod. <br><br> The <b>temperature</b> variables describe the water temperature in the surface layer (0-50m for March-May temperatures and 0-60m for August temperatures) of the water column during spring months and in the summer. <br><br> <b>Inverse hypoxic area</b> is the inverse of the deep water area that has gone hypoxic i.e. this variable describes the proportion of the study area where oxygen concentration in the deep water is above 2 mg/l."
                               , placement = "right", trigger = "hover", options = list(container = "body")),
                        
                        popify(checkboxInput(inputId = "novelTotal",
                                      label = "Total abiotic novelty", value = TRUE),
                               title = "Total abiotic novelty",
                               content = "<b>Total abiotic novelty</b> is the sum of other novelty variables and describes the total novelty expected to occur in the Central Baltic Sea in the current century. <br><br>     The five other novelty variables were each scaled between 0 and 1; 0 denoting values falling in the 95% confidence interval from observational data, and 1 denoting the extreme values of the model predictions till the end of the 21st century."
                               , placement = "right", trigger = "hover", options = list(container = "body"))
                 )
               )
      ), tabName = "novelty"),
    
    convertMenuItem(
      menuItem(HTML("How to balance profit and <br> good environmental status?"), tabName = "optimize")
      , tabName = "optimize")
  )
)

# Body
body <- dashboardBody(
 useShinyjs(),
  
  tabItems(
    tabItem("about",
            titlePanel("About the decision support tool"),
            fluidRow(
              tabBox(width = 12,
                id = "aboutBoxes",
                tabPanel("Introduction",
                         fluidRow(
                           uiOutput("about_shortly"),
                           actionButton("about_button", "Read more about the project."),
                           hidden(
                              uiOutput("about_indepth")
                           ))
                ),
                tabPanel("Decision tool",
                         fluidRow(
                           uiOutput("modelText"),
                           box(solidHeader = TRUE, width = 12, imageOutput("model", height = "auto")),
                           uiOutput("modelText2"),
                           actionButton("model_button", "Read more about the decision support system."),
                           hidden(
                             uiOutput("modelText3")
                           ))
                         
                ),
                tabPanel("Model area",
                         fluidRow(
                           box(solidHeader = TRUE, imageOutput("map", height = "auto")),
                           uiOutput("mapText")
                         )
                ),
                tabPanel("Aknowledgement",
                         fluidRow(
                           column(width = 12,
                                  uiOutput("aknowledgement")
                           ),
                           column(width = 12,
                                  imageOutput("logos", width = 3))
                         )))
            )  
    ),
    tabItem("predicted",
            titlePanel("Explore the predicted biomasses and catches in different management scenarios"),
            fluidRow(
              box(id = "timeseries_box",
                uiOutput("timeseries_info"),
                actionLink("time_series_link", "Show more."),
                hidden(
                  uiOutput("timeseries_info2")
                ),
                solidHeader = TRUE, width = 12) 
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
              tabBox(width = 12,
                     id = "novelty_boxes",
                     tabPanel("Novelty in the Baltic Sea",
                              uiOutput('novel_info'),
                              fluidRow(
                                box(plotOutput("novel_plot", height = 300), width = 12, solidHeader = TRUE),
                                uiOutput("novel_plot_total")
                              )),
                     tabPanel("What is novelty?",
                              fluidRow(
                                uiOutput("aboutNovelty"),
                                box(solidHeader = TRUE, width = 12, imageOutput("noveltyci", height = "auto")),
                                uiOutput("aboutNovelty2")
                              ))
            ))
    )
    ,
    tabItem("optimize",
            titlePanel("How to balance profit and good environmental status?"),
            
            fluidRow(
              column(width = 12,
                     uiOutput("opt_info")
                     ),
              column(width = 4,
                     
                     popify(selectInput(inputId = 'Profit',
                                        label = "Minimum acceptable profit",
                                        choices = c("No profit", "Profit larger than 0", "Profit larger than 100", "Profit larger than 200"),
                                        selected = "Profit larger than 100"
                     ), title = "Minimum acceptable profit", 
                     content = "Minimum acceptable profit enables selection of the desired lowest limit of annual profitability of fisheries on cod, herring, and sprat in millions of euros."
                     , placement = "right", trigger = "hover", options = list(container = "body")),

                     popify(radioButtons(inputId = 'Novelty',
                                         label = "Include expert knowledge",
                                         choices = c("Yes" = "Yes", "No" = "No"),
                                         selected = "Yes"
                     ), title = "Expert knowledge", 
                     content = "The decision support system includes studies on ecological novelty which denotes unprecedented, human-mediated changes at different ecological levels. <br><br> Novelty and its effect on a system are difficult to predict with models as novel conditions can not be calibrated against historical observations. <br><br> Adding expert scientific knowledge about this uncertainty on top of the modelling can increase the certainty of modelled predictions."
                     , placement = "right", trigger = "hover", options = list(container = "body"))
                     
                     ),
              column(width = 4,
                     
                     popify(selectInput(inputId = 'F_GES',
                                        label = "Status of fish stocks",
                                        choices = c("All stocks above the ref. point" = "All above", "One stock below the ref. point" = "One below",
                                                    "Two stocks below the ref. point" = "Two below", "All stocks below the ref. point" = "All below"),
                                        selected = "All above"
                     ), title = "Status of fish stocks", 
                     content = "The spawning-stock biomass of the three major fish species (i.e. cod, herring and sprat) in relation to the reference point describes the expected status of these fish stocks. The stocks are in good condition if their spawning-stock biomass exceeds the reference point. <br><br> The scale ranges from all three fish species being above the reference point limit to none of the species faring well."
                     , placement = "right", trigger = "hover", options = list(container = "body")),
                     
                     popify(radioButtons(inputId = 'Ref_point',
                                         label = "Fish stock reference point",
                                         choices = c("Blim" = "Blim", "B MSY trigger" = "B MSY"),
                                         selected = "Blim"
                     ), title = "Reference point", 
                     content = "Reference points are indexes applied in the regulation of fisheries. They are defined by the International Council for the Exploration of the Sea (ICES), and describe the status of the fish stocks. <br><br> <b>Blim</b> is the reference point describing the limit below which there is a high risk of reduced recruitment. <br><br> <b>B MSY trigger</b> (or MSYBtrigger) is considered to be the lower bound of spawning–stock biomass fluctuation below which fishing needs to be reduced to allow a fish stock to rebuild to levels capable of producing maximum sustainable yield (MSY)."
                     , placement = "right", trigger = "hover", options = list(container = "body"))
                     
                     ),
              column(width = 4,
                     
                     popify(selectInput(inputId = 'Nutr_GES',
                                        label = "Status of water quality",
                                        choices = c("Above GES" = "GES", "Below GES" = "Sub-GES"),
                                        selected = "Above"
                     ), title = "Status of water quality", 
                     content = "The indicators of water quality comprise of the average concentrations of nitrogen, phosphorus and chlorophyll <i>a</i>. <br><br> Low concentrations suggest high probability of reaching the Good Environmental Status (GES) as defined by HELCOM (Baltic Marine Environment Protection Commission – Helsinki Commission) whereas high concentrations indicate increased eutrophication."
                     , placement = "below", trigger = "hover", options = list(container = "body"))
                     
                     )
            ),
            
            fluidRow(
              column(width = 12,
                     uiOutput("scens_info")),
              column(width = 12,
                     htmlOutput("opt_scens")
                     ),
              column(width = 5,
                     box(plotOutput("opt_fish", height = 200), width = 12, solidHeader = TRUE),
                     box(plotOutput("opt_clim", height = 200), width = 12, solidHeader = TRUE)),
              column(width = 5,
                     box(plotOutput("opt_nutr", height = 200), width = 12, solidHeader = TRUE),
                     box(plotOutput("opt_dec", height = 200), width = 12, solidHeader = TRUE))
            )
    )),
    # CSS
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")) 
)

# Create the UI 
ui <- dashboardPage(header, sidebar, body, skin = "black")
