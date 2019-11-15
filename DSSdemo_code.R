###############################################################
## Demo version of BONUS BLUEWEBS decision support online tool
## WARNING: toy data!

library("shiny")
library("shinyWidgets")


datBio <- read.csv("D:/Github/shinyapp/EwE_bio_results.csv")
datCth <- read.csv("D:/Github/shinyapp/EwE_catch_results.csv")



# Main layout
ui <- navbarPage("BONUS BLUEWEBS decision support tool",
                 # Fix navbar to top of window
				 position = "fixed-top",
                 # Add padding due to fixed navbar and add possibility to scroll in case of overflow
                 tags$style(type="text/css", "body {padding-top: 70px;}", "overflow-y:scroll"),
                 
                 # About panel
                 tabPanel("About",
                          titlePanel("About the decision support tool"),
                          mainPanel(
                            textOutput("This decision support tool has been created as part of the BONUS BLUEWEBS  project. It summarizes the ..."))
                          ),
                 
                 # Predicted time-series 
                 tabPanel("Predicted time series",
                          titlePanel("Explore the predicted biomasses and catches in different management scenarios"),
                          sidebarLayout(
                            # Fix sidebar 
                            sidebarPanel(style = "position:fixed; width:inherit;", width = 3, 
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
                                         
                                         # Do two colums of checkbox groups (biomass and catch)
                                         fluidRow(
                                           column(width = 6,
                                                  checkboxGroupInput(inputId = "bioVars", 
                                                                     label = "Biomass Variables",
                                                                     choiceNames = list("Cod SSB", "Herring SSB", "Sprat SSB", "Zooplankton SSB", "Phytoplankton SSB"),
                                                                     choiceValues = list("plotCod", "plotHer", "plotSpr", "plotZoo", "plotPhy"),
                                                                     selected = "plotCod")
                                                  ),
                                           
                                           column(width = 6,
                                                  checkboxGroupInput(inputId = "catchVars", 
                                                                     label = "Catch Size Variables",
                                                                     choiceNames = list("Cod Catch", "Herring Catch", "Sprat Catch"),
                                                                     choiceValues = list("plotCodCatch","plotHerCatch","plotSprCatch"), 
                                                                     selected = "plotCodCatch")
                                                  )
                                        )
                            ),
							# Main panel with 50-50 split between Biomass plots and Catch plots
                            mainPanel(width = 9,
                              fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"), uiOutput("bio_plot_list"), uiOutput("catch_plot_list")) 
                              )
                            )
                          )
               ),
               tabPanel("Novelty"),
               tabPanel("Optimize"),
               selected="Predicted time series"
)

server <- function(input, output, session) {
  
  #The logic here will TENTATIVELY be as follows:
  # - create a large table with all the possible combinations of scenarios & output variables we want to show here
  # - select the correct lines from the data based on user input, visualize 
  # - we could mark threshold values such as GES limit or Blim as fixed into the charts
  #
  # Things to figure out:
  # - Can we show a flexible number of graphs (depending of how many checkboxes the user has checked)
  # - how to structure the data table and the input form so that is is easy to pick the correct data
  
  ## BIOMASS VARIABLES
  
  # Cod
  output$plotCod <- renderPlot(
    {
      if("plotCod" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),] 
        plot(x=tmp$Year, y=tmp$Cod,  xlab="Year", ylab="Biomass", ylim=c(0,3.5), xlim=c(2004,2096), type = 'n', main = "Biomass of cod")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Cod - tmp$CodSD), rev(tmp$Cod + tmp$CodSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Cod, col="black")
        abline(h=0.8, col = "red")
        text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )  
  
  # Herring
  output$plotHer <- renderPlot(
    {
      if("plotHer" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Herring,  xlab="Year", ylab="Biomass", ylim=c(0,3.5), xlim=c(2004,2096), type = 'n', main = "Biomass of Herring")
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
        plot(x=tmp$Year, y=tmp$Sprat,  xlab="Year", ylab="Biomass", ylim=c(0,3.5), xlim=c(2004,2096), type = 'n', main = "Biomass of Sprat")
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
        plot(x=tmp$Year, y=tmp$CodCatch,  xlab="Year", ylab="Catch", ylim=c(0,1.2), xlim=c(2004,2096), type = 'n', main = "Cod Catch")
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
        plot(x=tmp$Year, y=tmp$HerringCatch,  xlab="Year", ylab="Catch", ylim=c(0,1.2), xlim=c(2004,2096), type = 'n', main = "Herring Catch")
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
        plot(x=tmp$Year, y=tmp$SpratCatch,  xlab="Year", ylab="Catch", ylim=c(0,1.2), xlim=c(2004,2096), type = 'n', main = "Sprat Catch")
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
            column(width=12, plotOutput(plotname, height = 300))
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
          column(width=12, plotOutput(plotname, height = 300))
        }
      )
      do.call(tagList, catch_plot_output_list)
    }
  )
  
}

shinyApp(ui = ui, server = server)

## End(Not run)

