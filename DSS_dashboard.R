library(shiny)
library(shinydashboard)


# Load data
datBio <- read.csv("D:/Github/shinyapp/EwE_bio_results_corrected.csv", row.names = NULL, sep = ";")
datCth <- read.csv("D:/Github/shinyapp/EwE_catch_results_corrected.csv", row.names = NULL, sep = ";")
datNov <- read.csv("D:/Github/shinyapp/Corrected_novelty_30_09_2019.csv", row.names = NULL, sep = ";")


# Header
header <- dashboardHeader(title = "BONUS BLUEWEBS decision support tool", titleWidth = 450)

# Manually links menuItems to the corresponding tabItems that would otherwise be unlinked
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
                                           choiceNames = list("Cod SSB", "Herring SSB", "Sprat SSB", "Zooplankton SSB", "Phytoplankton SSB"),
                                           choiceValues = list("plotCod", "plotHer", "plotSpr", "plotZoo", "plotPhy"),
                                           selected = "plotCod")
                 ),
                 
                 column(width = 6, offset = 1,
                        checkboxGroupInput(inputId = "catchVars", 
                                           label = "Catch Size Variables",
                                           choiceNames = list("Cod Catch", "Herring Catch", "Sprat Catch"),
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
               
               # Create the column of checkbox groups (codRV, temp_Aug060, notHypoxic)
               fluidRow(
                 column(width = 9,
                        checkboxGroupInput(inputId = "novelVars", 
                                           label = "Novelty variables",
                                           choiceNames = list("Cod reproductive volume", "Water temperature", "Inverse Hypoxic area"),
                                           choiceValues = list("plotRv", "plotTemp", "plotHyp"),
                                           selected = "plotRv")
                 )
              )
      ), tabName = "novelty"),
    convertMenuItem(
      menuItem("Optimize", tabName = "optimize"), tabName = "optimize")
  )
)

# Body
body <- dashboardBody(
  tabItems(
    tabItem("about",
            titlePanel("About the decision support tool"),
            htmlOutput("aboutText"), width = 10),
            
    tabItem("predicted",
            titlePanel("Explore the predicted biomasses and catches in different management scenarios"),
            fluidRow(
              splitLayout(cellWidths = c("50%", "50%"), uiOutput("bio_plot_list"), uiOutput("catch_plot_list"))
            )
    ),
    tabItem("novelty",
            titlePanel("Explore the uncertainty of model forecasts under novel conditions"),
            fluidRow(
              splitLayout(cellWidths = c("60%", "40%"), uiOutput("novel_plot_list"), uiOutput("novel_info"))
            )
    ),
    tabItem("optimize",
            titlePanel("Explore the predictions for ecosystem services in different scenarios")
    )
  )
)

# Create the UI 
ui <- dashboardPage(header, sidebar, body, skin = "black") #skin determines the colour of the dashboard



# Define the server function
server <- function(input, output) {
  
  # "ABOUT" TAB
  
  output$aboutText <- renderText({
    HTML("This decision support tool has been created as part of the BONUS BLUEWEBS project. 
         It aims to visualize and explain the key results of various environmental modelling scenarios.",
         "<br>",
         "<br>",
         "The model behind this application has been used to forecast the impacts of different climatic futures,
         nutrient load management schemes and fisheries management options in the open Baltic Sea marine ecosystem
         during the 21st century.")
  })
  
  
  
  # "PREDICTED TIME SERIES" TAB
  
  ## BIOMASS VARIABLES
  
  # Cod
  output$plotCod <- renderPlot(
    {
      if("plotCod" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),] 
        plot(x=tmp$Year, y=tmp$Cod,  xlab="Year", ylab="Biomass", ylim=c(0,250), xlim=c(2004,2096), type = 'n', main = "Biomass of cod")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Cod - tmp$CodSD), rev(tmp$Cod + tmp$CodSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Cod, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )  
  
  # Herring
  output$plotHer <- renderPlot(
    {
      if("plotHer" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Herring,  xlab="Year", ylab="Biomass", ylim=c(0,800), xlim=c(2004,2096), type = 'n', main = "Biomass of Herring")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Herring - tmp$HerringSD), rev(tmp$Herring + tmp$HerringSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Herring, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Sprat
  output$plotSpr <- renderPlot(
    {
      if("plotSpr" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Sprat,  xlab="Year", ylab="Biomass", ylim=c(0,1150), xlim=c(2004,2096), type = 'n', main = "Biomass of Sprat")
        # !!!! NOTE In data file there is SpratDS instead of SpratSD
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Sprat - tmp$SpratDS), rev(tmp$Sprat + tmp$SpratDS)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Sprat, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Zooplankton
  output$plotZoo <- renderPlot(
    {
      if("plotZoo" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Zooplankton,  xlab="Year", ylab="Biomass", ylim=c(0,12), xlim=c(2004,2096), type = 'n', main = "Biomass of Zooplankton")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Zooplankton - tmp$ZooplanktonSD), rev(tmp$Zooplankton + tmp$ZooplanktonSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Zooplankton, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  #Phytoplankton
  output$plotPhy <- renderPlot(
    {
      if("plotPhy" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Phytoplankton,  xlab="Year", ylab="Biomass", ylim=c(0,12), xlim=c(2004,2096), type = 'n', main = "Biomass of Phytoplankton")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Phytoplankton - tmp$PhytoplanktonSD), rev(tmp$Phytoplankton + tmp$PhytoplanktonSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Phytoplankton, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  ## CATCH VARIABLES
  
  # Cod
  output$plotCodCatch <- renderPlot(
    {
      if("plotCodCatch" %in% input$catchVars){
        tmp <- datCth[(datCth$F == input$F & datCth$Nutr_scen == input$Nutr_scen & datCth$Climate == input$Climate),] 
        plot(x=tmp$Year, y=tmp$CodCatch,  xlab="Year", ylab="Catch", ylim=c(0,80), xlim=c(2004,2096), type = 'n', main = "Cod Catch")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$CodCatch - tmp$CodSD), rev(tmp$CodCatch + tmp$CodSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$CodCatch, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )  
  
  # Herring
  output$plotHerCatch <- renderPlot(
    {
      if("plotHerCatch" %in% input$catchVars){
        tmp <- datCth[(datCth$F == input$F & datCth$Nutr_scen == input$Nutr_scen & datCth$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$HerringCatch,  xlab="Year", ylab="Catch", ylim=c(0,250), xlim=c(2004,2096), type = 'n', main = "Herring Catch")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$HerringCatch - tmp$HerringSD), rev(tmp$HerringCatch + tmp$HerringSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$HerringCatch, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Sprat
  output$plotSprCatch <- renderPlot(
    {
      if("plotSprCatch" %in% input$catchVars){
        tmp <- datCth[(datCth$F == input$F & datCth$Nutr_scen == input$Nutr_scen & datCth$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$SpratCatch,  xlab="Year", ylab="Catch", ylim=c(0,400), xlim=c(2004,2096), type = 'n', main = "Sprat Catch")
        # !!!! NOTE In data file there is SpratDS instead of SpratSD
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$SpratCatch - tmp$SpratDS), rev(tmp$SpratCatch + tmp$SpratDS)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$SpratCatch, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Render Biomass variable time-series
  output$bio_plot_list <- renderUI(
    {
      bio_plot_output_list <- lapply(
        input$bioVars, function(plotname) {
            column(width=12, box(plotOutput(plotname, height = 300), width = 13))
        }
      )
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, bio_plot_output_list)
    }
  )
  
  # Render Catch variable time-series  
  output$catch_plot_list <- renderUI(
    {
      catch_plot_output_list <- lapply(
        input$catchVars, function(plotname) {
            column(width=12, box(plotOutput(plotname, height = 300), width = 13))
        }
      )
      do.call(tagList, catch_plot_output_list)
    }
  )
  
  
  # "NOVELTY TAB"
  
  output$plotRv <- renderPlot(
    {
      if("plotRv" %in% input$novelVars){
        tmp <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),] 
        plot(x=tmp$Year, y=tmp$codRV,  xlab="Year", ylab="Cod reproductive volume", ylim=c(0,1550), xlim=c(2004,2096), type = 'n', main = "Novelty for cod reproductive volume")
        #polygon(c(tmp$Year, rev(tmp$Year)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$codRV, col="black")
      }
    }
  )
  
  
  output$plotTemp <- renderPlot(
    {
      if("plotTemp" %in% input$novelVars){
        tmp <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),] 
        plot(x=tmp$Year, y=tmp$Aug060mT,  xlab="Year", ylab="Water (0-60m) temperature", ylim=c(-0.5,2.3), xlim=c(2004,2096), type = 'n', main = "Novelty for August 0-60m water temperature")
        #polygon(c(tmp$Year, rev(tmp$Year)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Aug060mT, col="black")
      }
    }
  )
  
  output$plotHyp <- renderPlot(
    {
      if("plotHyp" %in% input$novelVars){
        tmp <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),]
        plot(x=tmp$Year, y=tmp$notHypoxicA,  xlab="Year", ylab="Inverse hypoxic area", ylim=c(0,58500), xlim=c(2004,2096), type = 'n', main = "Novelty for inverse hypoxic area")
        #polygon(c(tmp$Year, rev(tmp$Year)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$notHypoxicA, col="black")
      }
    }
  )
  
  # Render Novelty variable time-series
  output$novel_plot_list <- renderUI(
    {
      novel_plot_output_list <- lapply(
        input$novelVars, function(plotname) {
          column(width=12, box(plotOutput(plotname, height = 300), width = 13))
        }
      )
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, novel_plot_output_list)
    }
  )
  
  
  output$novel_info <- renderUI({
          box(
            title = "About novelty", width = 12, height = 320, fill = TRUE, background = "purple", style = "position:fixed",
            "Novel conditions, i.e. conditions that have not been observed in the past..."
            
          )
  })
  
}

shinyApp(ui = ui, server = server)
